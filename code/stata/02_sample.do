/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 02_sample.do
* Purpose: Apply sample restrictions per research design
* Author: Derek Tharp
* Created: January 2025
*******************************************************************************/

* ============================================================================
* PROGRAM SETUP
* ============================================================================
* This script is designed to be run from 00_master.do, which sources
* 00_config.do and sets all required globals ($project, $raw_data, etc.).
* It can also be run standalone if 00_config.do is sourced first.

* Source config if globals are not already set (allows standalone execution)
if "$project" == "" | "$config_loaded" != "1" {
    capture noisily do "code/stata/00_config.do"
    if _rc {
        capture noisily do "00_config.do"
    }
    if "$project" == "" {
        di as error "Could not locate 00_config.do. Run from the project root or code/stata, or set the manual override in 00_config.do."
        exit 601
    }
}

clear all
set more off

* Log
cap log close sample
log using "$logs/02_sample.log", replace name(sample)

di "=============================================="
di "02_sample.do: Sample Restrictions"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* Load merged data
use "$derived/hrs_merged.dta", clear

di "Starting observations: `c(N)'"

* ============================================================================
* SAMPLE RESTRICTION 1: BIRTH COHORT (1931-1959)
* ============================================================================

di _n "Restriction 1: Birth cohort 1931-1959"

* Check birth year distribution before restriction
tab rabyear if rabyear >= 1920 & rabyear <= 1970, missing

* Apply restriction
drop if rabyear < $cohort_lo | rabyear > $cohort_hi | missing(rabyear)

di "Remaining observations: `c(N)'"

* ============================================================================
* SAMPLE RESTRICTION 2: CLAIMED SOCIAL SECURITY
* ============================================================================

di _n "Restriction 2: Claimed Social Security with valid claiming age"

* Check claiming age distribution
sum rassageb, detail

* Claiming age should be within valid claiming window
* Drop if no claiming age or outside valid range
drop if missing(rassageb)
drop if rassageb < $claim_age_lo | rassageb > $claim_age_hi

di "Remaining observations: `c(N)'"

* Tabulate claiming ages
tab rassageb

* ============================================================================
* RESHAPE TO PERSON-WAVE (LONG FORMAT)
* ============================================================================

di _n "Reshaping to person-wave format..."

* First, document which waves have LB data for this sample
* LB collected: Wave 8 (2006), 9 (2008), 10 (2010), ..., 16 (2022)

* Harmonize Wave 8 affect variable names before reshape
* Wave 8 uses r8lbposaffect6/r8lbnegaffect6; waves 9+ use r*lbposaffect/r*lbnegaffect
capture confirm variable r8lbposaffect6
if !_rc {
    di "Renaming Wave 8 affect variables for reshape compatibility..."
    rename r8lbposaffect6 r8lbposaffect
    rename r8lbnegaffect6 r8lbnegaffect
}

* Reshape from wide to long
* This creates one observation per person-wave

* Identify stub patterns for reshape
* Format: r{wave}varname → rvarname with wave dimension

rename hhidpn id

* Keep waves 8-16 (2006-2022) which have Leave-Behind data
reshape long ///
    r@agey_b ///
    r@shlt ///
    r@hosp ///
    r@conde ///
    r@mstat ///
    r@lbrf ///
    r@proxy ///
    r@liv75 ///
    r@cesd ///
    r@wtresp ///
    h@itot ///
    h@atotb ///
    r@lbelig ///
    r@lbcomp ///
    r@lbwgtr ///
    r@lbsatwlf ///
    r@lbposaffect ///
    r@lbnegaffect ///
    r@lblonely3 ///
    r@lbfinprb ///
    r@lbcon5 ///
    r@lbopen ///
    r@lbsatfin ///
    r@lbsatinc ///
    , i(id) j(wave)

rename id hhidpn

* Keep only waves 8-16
keep if wave >= 8 & wave <= 16

di "Observations after reshape (person-waves): `c(N)'"

* Create year variable from wave (wave 8 = 2006, ..., wave 16 = 2022)
gen year = 1990 + wave * 2

label define wave_lbl 8 "2006" 9 "2008" 10 "2010" 11 "2012" 12 "2014" ///
    13 "2016" 14 "2018" 15 "2020" 16 "2022"
label values wave wave_lbl

* Rename reshaped variables for clarity
rename ragey_b age
rename rshlt shlt
rename rhosp hosp
rename rconde conde
rename rmstat mstat
rename rlbrf lbrf
rename rproxy proxy
rename rliv75 liv75
rename rcesd cesd
rename rwtresp resp_weight
rename hitot hh_income
rename hatotb hh_assets
rename rlbelig lb_elig
rename rlbcomp lb_comp
rename rlbwgtr lb_weight
rename rlbsatwlf life_sat
rename rlbposaffect pos_affect
rename rlbnegaffect neg_affect
rename rlblonely3 loneliness
rename rlbfinprb fin_strain
rename rlbcon5 conscientious
rename rlbopen openness
rename rlbsatfin satfin
rename rlbsatinc satinc

* ============================================================================
* SAVE PRE-LB CES-D SAMPLE (core interview outcomes, no LB requirement)
* ============================================================================

di _n "Saving pre-LB dataset for CES-D analysis..."

* CES-D is a core HRS variable available for all interview respondents.
* Save the full post-claim, non-proxy sample before LB restrictions.

preserve

* Apply non-proxy restriction for CES-D sample
keep if proxy == 0 | missing(proxy)

* Require valid CES-D (but do NOT restrict to post-claim yet)
drop if missing(cesd)

* Save full CES-D sample including pre-claim observations (for event study)
count
di "CES-D full sample (incl pre-claim): `r(N)' person-waves"
distinct hhidpn
di "CES-D full sample unique respondents: `r(ndistinct)'"
save "$derived/hrs_cesd_full_sample.dta", replace
di "Saved: $derived/hrs_cesd_full_sample.dta"

* Now apply post-claim restriction for the cross-sectional CES-D analysis
gen years_since_claim_cesd = age - rassageb
keep if years_since_claim_cesd >= 0 & !missing(years_since_claim_cesd)
drop years_since_claim_cesd

count
di "CES-D post-claim sample: `r(N)' person-waves"

distinct hhidpn
di "CES-D post-claim unique respondents: `r(ndistinct)'"

save "$derived/hrs_cesd_sample.dta", replace
di "Saved: $derived/hrs_cesd_sample.dta"

restore

* ============================================================================
* SAMPLE RESTRICTION 3: LEAVE-BEHIND ELIGIBILITY AND COMPLETION
* ============================================================================

di _n "Restriction 3: Leave-Behind eligible and completed"

* Tabulate LB completion
tab lb_elig lb_comp, missing

* Keep if eligible and completed LB
* lb_elig = 1 means eligible
* lb_comp = 1 (mail) or 2 (phone) or 4 (other) means completed

keep if lb_elig == 1 & inlist(lb_comp, 1, 2, 4)

di "Remaining person-waves: `c(N)'"

* ============================================================================
* SAMPLE RESTRICTION 4: NON-PROXY RESPONDENTS (PRIMARY ANALYSIS)
* ============================================================================

di _n "Restriction 4: Non-proxy respondents"

* Tabulate proxy status
tab proxy, missing

* For primary analysis, keep non-proxy (proxy = 0)
* We'll flag this for sensitivity analysis
gen proxy_flag = (proxy == 1)
label var proxy_flag "Proxy interview (for sensitivity analysis)"

* Primary sample: non-proxy
keep if proxy == 0 | missing(proxy)

di "Remaining person-waves (non-proxy): `c(N)'"

* ============================================================================
* SAMPLE RESTRICTION 5: VALID OUTCOME MEASURES
* ============================================================================

di _n "Restriction 5: Valid primary outcome (life satisfaction)"

* Check life satisfaction availability
sum life_sat, detail

* Keep observations with valid life satisfaction
drop if missing(life_sat)

di "Remaining person-waves with valid outcomes: `c(N)'"

* ============================================================================
* SAMPLE RESTRICTION 6: POST-CLAIM OBSERVATIONS ONLY
* ============================================================================

di _n "Restriction 6: Post-claim observations only"

* Only include observations measured AFTER the person claimed SS.
* Otherwise we are measuring pre-claim SWB and relating it to future claiming age,
* which conflates selection effects with causal effects

* Create years since claiming
gen years_since_claim_temp = age - rassageb

* Tabulate to see distribution
di "Years since claiming distribution (before restriction):"
tab years_since_claim_temp, missing

* Count pre-claim observations
count if years_since_claim_temp < 0
local n_preclaim = r(N)
di "Observations measured BEFORE claiming (will drop): `n_preclaim'"

* Keep only post-claim observations
* (age at interview >= claiming age)
keep if years_since_claim_temp >= 0 & !missing(years_since_claim_temp)

drop years_since_claim_temp

di "Remaining person-waves (post-claim only): `c(N)'"

* ============================================================================
* SUMMARIZE FINAL SAMPLE
* ============================================================================

di _n "=============================================="
di "Final Sample Summary"
di "=============================================="

* Person-wave counts
di _n "Person-waves by year:"
tab year

* Unique persons
capture which distinct
if _rc ssc install distinct, replace
distinct hhidpn
di "Unique respondents: `r(ndistinct)'"

* Claiming age distribution in final sample
di _n "Claiming age distribution:"
tab rassageb

* Demographics
di _n "Gender:"
tab ragender

di _n "Education:"
tab raeduc

di _n "Race:"
tab raracem

* ============================================================================
* SAMPLE SIZE ASSERTIONS
* ============================================================================
* Guard against silent data changes that would alter the analytic sample

count
local n_final = r(N)
assert `n_final' > 5000 & `n_final' < 30000  // Expected ~19,000 person-waves

distinct hhidpn
local n_unique = r(ndistinct)
assert `n_unique' > 3000 & `n_unique' < 15000  // Expected ~8,000 unique persons

di "Assertions passed: N = `n_final' person-waves, `n_unique' unique persons"

* Save analytic sample
save "$derived/hrs_analytic.dta", replace

di _n "Saved: $derived/hrs_analytic.dta"

log close sample

* ============================================================================
* END OF 02_sample.do
* ============================================================================
