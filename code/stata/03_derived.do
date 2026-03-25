/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 03_derived.do
* Purpose: Create derived variables for analysis
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
cap log close derived
log using "$logs/03_derived.log", replace name(derived)

di "=============================================="
di "03_derived.do: Derived Variables"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* Load analytic sample
use "$derived/hrs_analytic.dta", clear

di "Starting observations: `c(N)'"

* ============================================================================
* PART 1: CLAIMING AGE CATEGORIES
* ============================================================================

di _n "Creating claiming age categories..."

* Primary categorization: Early, FRA-adjacent, Delayed
* NOTE: Using half-open intervals [a, b) to avoid gaps with fractional claiming ages
* Previously: 62-63, 64-66, 67-70 created gaps at 63.01-63.99 and 66.01-66.99
gen claim_cat3 = .
replace claim_cat3 = 1 if rassageb >= 62 & rassageb < 64   // Early claimers [62, 64)
replace claim_cat3 = 2 if rassageb >= 64 & rassageb < 67   // FRA-adjacent [64, 67)
replace claim_cat3 = 3 if rassageb >= 67 & rassageb <= 70  // Delayed claimers [67, 70]

label define claim_cat3_lbl 1 "Early (62-63)" 2 "FRA (64-66)" 3 "Delayed (67+)"
label values claim_cat3 claim_cat3_lbl
label var claim_cat3 "Claiming age category (3 groups)"

tab claim_cat3, missing

* Binary: Early vs Post-FRA (for robustness)
* Using consistent half-open interval logic
gen claim_early = (rassageb >= 62 & rassageb < 64)
gen claim_post_fra = (rassageb >= 67 & rassageb <= 70)

label var claim_early "Claimed at 62-63 (early)"
label var claim_post_fra "Claimed at 67+ (post-FRA)"

* Continuous claiming age (centered at 62 for interpretation)
gen claim_age_c62 = rassageb - 62
label var claim_age_c62 "Claiming age (centered at 62)"

* Continuous claiming age (centered at FRA=66 for interpretation)
gen claim_age_c66 = rassageb - 66
label var claim_age_c66 "Claiming age (centered at 66)"

* ============================================================================
* PART 2: BENEFIT GAIN (OPPORTUNITY COST OF EARLY CLAIMING)
* ============================================================================

di _n "Benefit gain variable not constructed (SS Wealth file not used in this analysis)."
di "Claiming age serves as a sufficient proxy for the benefit-level variation."

* ============================================================================
* PART 3: STANDARDIZE OUTCOME VARIABLES
* ============================================================================

di _n "Standardizing outcome variables..."

* Life satisfaction - standardize for effect size interpretation
egen life_sat_z = std(life_sat)
label var life_sat_z "Life satisfaction (z-score)"

* Positive affect - standardize
egen pos_affect_z = std(pos_affect)
label var pos_affect_z "Positive affect (z-score)"

* Negative affect - standardize
egen neg_affect_z = std(neg_affect)
label var neg_affect_z "Negative affect (z-score)"

* Loneliness - standardize
egen loneliness_z = std(loneliness)
label var loneliness_z "Loneliness (z-score)"

* Financial strain - standardize
egen fin_strain_z = std(fin_strain)
label var fin_strain_z "Financial strain (z-score)"

* Summary statistics of standardized outcomes
sum life_sat_z pos_affect_z neg_affect_z loneliness_z fin_strain_z

* ============================================================================
* PART 4: CONTROL VARIABLE PREPARATION
* ============================================================================

di _n "Preparing control variables..."

* -----------------------------------------------------------------------------
* Demographics
* -----------------------------------------------------------------------------

* Female indicator
gen female = (ragender == 2) if !missing(ragender)
label var female "Female"

* Education dummies (reference: less than HS)
tab raeduc, gen(educ_)
rename educ_1 educ_lt_hs
rename educ_2 educ_ged
rename educ_3 educ_hs
rename educ_4 educ_some_col
rename educ_5 educ_col_plus

label var educ_lt_hs "Education: Less than HS"
label var educ_ged "Education: GED"
label var educ_hs "Education: HS graduate"
label var educ_some_col "Education: Some college"
label var educ_col_plus "Education: College+"

* Race dummies (reference: White)
gen black = (raracem == 2) if !missing(raracem)
gen other_race = (raracem == 3) if !missing(raracem)
gen hispanic = (rahispan == 1) if !missing(rahispan)

label var black "Black"
label var other_race "Other race"
label var hispanic "Hispanic"

* -----------------------------------------------------------------------------
* Health
* -----------------------------------------------------------------------------

* Self-rated health (1=excellent to 5=poor)
* Create fair/poor health indicator
gen poor_health = (shlt >= 4) if !missing(shlt)
label var poor_health "Fair/poor self-rated health"

* Retirement status (from RAND labor force status)
* lbrf: 5 = retired, .a = 65+/presumed retired
gen retired = (lbrf == 5) if !missing(lbrf)
replace retired = 1 if lbrf == .a
label var retired "Currently retired"

* Health conditions (count)
label var conde "Number of health conditions"

* Hospitalized indicator
gen hospitalized = (hosp == 1) if !missing(hosp)
label var hospitalized "Hospitalized since last wave"

* -----------------------------------------------------------------------------
* Subjective survival probability
* -----------------------------------------------------------------------------

* Survival probability controls (already 0-100 scale)
* Note: liv85 not available for waves 8-16 (only waves 1-4)
label var liv75 "Subjective P(survive to 75)"

* Low survival expectation indicator
gen low_surv_exp = (liv75 < 50) if !missing(liv75)
label var low_surv_exp "Low survival expectations (P75 < 50%)"

* -----------------------------------------------------------------------------
* Economic variables
* -----------------------------------------------------------------------------

* Log household income (adding 1 to handle zeros)
gen ln_hh_income = ln(hh_income + 1)
label var ln_hh_income "Log household income"

* Inverse Hyperbolic Sine transformation for assets (handles negatives correctly)
* IHS(x) = ln(x + sqrt(x^2 + 1)), approximates log for large positive values
* but handles zeros and negatives appropriately
gen ihs_hh_assets = asinh(hh_assets)
label var ihs_hh_assets "Household assets (IHS transformation)"

* Also create log version for robustness (with caveat about negatives)
gen ln_hh_assets = ln(hh_assets + 1) if hh_assets > 0
replace ln_hh_assets = 0 if hh_assets <= 0
label var ln_hh_assets "Log household assets (zeros/negatives set to 0)"

* Income quartiles (within sample)
capture count if !missing(hh_income)
if r(N) > 0 {
    xtile income_q4 = hh_income, nq(4)
    label var income_q4 "Household income quartile"
}
else {
    gen income_q4 = .
    label var income_q4 "Household income quartile (not available)"
}

* Asset quartiles
capture count if !missing(hh_assets)
if r(N) > 0 {
    xtile asset_q4 = hh_assets, nq(4)
    label var asset_q4 "Household asset quartile"
}
else {
    gen asset_q4 = .
    label var asset_q4 "Household asset quartile (not available)"
}

* -----------------------------------------------------------------------------
* Marital status
* -----------------------------------------------------------------------------

* Marital status categories
gen married = inlist(mstat, 1, 2, 3) if !missing(mstat)
gen widowed = inlist(mstat, 7) if !missing(mstat)
gen divorced = inlist(mstat, 4, 5, 6) if !missing(mstat)
gen never_married = inlist(mstat, 8) if !missing(mstat)

label var married "Currently married/partnered"
label var widowed "Widowed"
label var divorced "Divorced/separated"
label var never_married "Never married"

* -----------------------------------------------------------------------------
* Age controls
* -----------------------------------------------------------------------------

* Age at interview
label var age "Age at interview"

* Age squared
gen age_sq = age^2
label var age_sq "Age squared"

* Age centered at 65
gen age_c65 = age - 65
label var age_c65 "Age (centered at 65)"

* -----------------------------------------------------------------------------
* Personality controls (potential confounders)
* -----------------------------------------------------------------------------

* Big Five personality traits (standardize)
* Available: Conscientiousness, Openness
* Not in RAND file: Neuroticism, Agreeableness, Extraversion

capture confirm variable conscientious
if !_rc {
    egen conscientious_z = std(conscientious)
    label var conscientious_z "Conscientiousness (z-score)"
}
else {
    gen conscientious_z = .
    label var conscientious_z "Conscientiousness (not available)"
}

capture confirm variable openness
if !_rc {
    egen openness_z = std(openness)
    label var openness_z "Openness to Experience (z-score)"
}
else {
    gen openness_z = .
    label var openness_z "Openness (not available)"
}

* ============================================================================
* PART 5: YEARS SINCE CLAIMING
* ============================================================================

di _n "Creating years since claiming..."

* Years since started receiving SS
* Note: This is approximate since we only have claiming year, not exact date
gen years_since_claim = age - rassageb
replace years_since_claim = . if years_since_claim < 0  // Should not happen

label var years_since_claim "Years since started receiving SS"

* Indicator for recently claimed (within 2 years)
gen recent_claim = (years_since_claim <= 2) if !missing(years_since_claim)
label var recent_claim "Claimed within last 2 years"

* ============================================================================
* PART 5B: FULL RETIREMENT AGE (FRA) - FOR IV AND PERSON-SPECIFIC CATEGORIES
* ============================================================================

di _n "Creating Full Retirement Age based on birth cohort..."

* The 1983 Social Security Amendments increased FRA for cohorts born after 1937
* This provides legislatively-determined, plausibly exogenous variation
* FRA schedule:
*   Born 1937 or earlier: FRA = 65
*   Born 1938: 65 + 2 months
*   Born 1939: 65 + 4 months
*   Born 1940: 65 + 6 months
*   Born 1941: 65 + 8 months
*   Born 1942: 65 + 10 months
*   Born 1943-1954: FRA = 66
*   Born 1955: 66 + 2 months
*   Born 1956: 66 + 4 months
*   Born 1957: 66 + 6 months
*   Born 1958: 66 + 8 months
*   Born 1959: 66 + 10 months
*   Born 1960+: FRA = 67

gen fra = .
replace fra = 65 if rabyear <= 1937
replace fra = 65 + 2/12  if rabyear == 1938
replace fra = 65 + 4/12  if rabyear == 1939
replace fra = 65 + 6/12  if rabyear == 1940
replace fra = 65 + 8/12  if rabyear == 1941
replace fra = 65 + 10/12 if rabyear == 1942
replace fra = 66 if rabyear >= 1943 & rabyear <= 1954
replace fra = 66 + 2/12  if rabyear == 1955
replace fra = 66 + 4/12  if rabyear == 1956
replace fra = 66 + 6/12  if rabyear == 1957
replace fra = 66 + 8/12  if rabyear == 1958
replace fra = 66 + 10/12 if rabyear == 1959

label var fra "Full Retirement Age (based on birth cohort)"

* Distance from baseline FRA of 65 (instrument)
gen fra_distance = fra - 65
label var fra_distance "FRA distance from 65 (IV instrument)"

* Claiming age relative to person-specific FRA
gen claim_rel_fra = rassageb - fra
label var claim_rel_fra "Claiming age relative to FRA"

* Person-specific claiming categories (more theoretically appropriate)
gen claim_cat_fra = .
replace claim_cat_fra = 1 if rassageb < fra - 0.5   // Early: claimed >6 months before FRA
replace claim_cat_fra = 2 if rassageb >= fra - 0.5 & rassageb <= fra + 0.5  // At FRA (±6 months)
replace claim_cat_fra = 3 if rassageb > fra + 0.5   // Delayed: claimed >6 months after FRA

label define claim_cat_fra_lbl 1 "Early (before FRA)" 2 "At FRA" 3 "Delayed (after FRA)"
label values claim_cat_fra claim_cat_fra_lbl
label var claim_cat_fra "Claiming category relative to individual FRA"

tab claim_cat_fra, missing

* ============================================================================
* PART 6: INTERACTION TERMS
* ============================================================================

di _n "Creating interaction terms for heterogeneity analysis..."

* Claiming age × Health
gen claim_x_poorhealth = claim_age_c62 * poor_health
label var claim_x_poorhealth "Claiming age × Poor health"

* Claiming age × Low survival expectations
gen claim_x_lowsurv = claim_age_c62 * low_surv_exp
label var claim_x_lowsurv "Claiming age × Low survival expectations"

* Claiming age × Female
gen claim_x_female = claim_age_c62 * female
label var claim_x_female "Claiming age × Female"

* Claiming age × Education (college+)
gen claim_x_college = claim_age_c62 * educ_col_plus
label var claim_x_college "Claiming age × College education"

* ============================================================================
* PART 7: WAVE/TIME INDICATORS
* ============================================================================

di _n "Creating time indicators..."

* Wave dummies (for fixed effects)
tab wave, gen(wave_)

* LB subsample indicator
* Subsample A: 2006, 2010, 2014, 2018, 2022 (waves 8, 10, 12, 14, 16)
* Subsample B: 2008, 2012, 2016, 2020 (waves 9, 11, 13, 15)
gen lb_subsample = "A" if inlist(wave, 8, 10, 12, 14, 16)
replace lb_subsample = "B" if inlist(wave, 9, 11, 13, 15)
label var lb_subsample "Leave-Behind subsample (A/B)"

* ============================================================================
* PART 8: MISSING DATA FLAGS
* ============================================================================

di _n "Creating missing data indicators..."

* Flag observations with complete controls
gen complete_controls = !missing(female, raeduc, shlt, hh_income, married)
label var complete_controls "Complete control variables"

tab complete_controls

* ============================================================================
* SAVE FINAL ANALYTIC FILE
* ============================================================================

di _n "Saving final analytic dataset..."

* Standardize CES-D (if present in the primary analytic sample)
capture confirm variable cesd
if !_rc {
    egen cesd_z = std(cesd)
    label var cesd_z "CES-D depressive symptoms (z-score)"
}

* Order variables logically
order hhidpn wave year rabyear ///
    rassageb fra fra_distance claim_rel_fra ///
    claim_cat3 claim_cat_fra claim_early claim_post_fra claim_age_c62 claim_age_c66 ///
    life_sat life_sat_z pos_affect pos_affect_z neg_affect neg_affect_z ///
    loneliness loneliness_z fin_strain fin_strain_z ///
    female raeduc black other_race hispanic ///
    age shlt poor_health conde hospitalized ///
    liv75 low_surv_exp ///
    hh_income hh_assets ln_hh_income ihs_hh_assets ///
    married widowed divorced ///
    years_since_claim recent_claim ///
    lb_weight resp_weight

compress
save "$derived/hrs_analysis_ready.dta", replace

di _n "Saved: $derived/hrs_analysis_ready.dta"
di "Final observations: `c(N)'"
di "Final variables: `c(k)'"

* ============================================================================
* CREATE CES-D ANALYSIS-READY DATASET
* ============================================================================

di _n "Creating CES-D analysis-ready dataset..."

capture confirm file "$derived/hrs_cesd_sample.dta"
if !_rc {
    preserve

    use "$derived/hrs_cesd_sample.dta", clear
    di "CES-D sample loaded: `c(N)' observations"

    * Create same derived variables as the primary sample
    gen claim_age_c62 = rassageb - 62
    label var claim_age_c62 "Claiming age (centered at 62)"

    gen female = (ragender == 2) if !missing(ragender)
    label var female "Female"

    gen black = (raracem == 2) if !missing(raracem)
    gen other_race = (raracem == 3) if !missing(raracem)
    gen hispanic = (rahispan == 1) if !missing(rahispan)
    label var black "Black"
    label var other_race "Other race"
    label var hispanic "Hispanic"

    gen poor_health = (shlt >= 4) if !missing(shlt)
    label var poor_health "Fair/poor self-rated health"

    gen married = inlist(mstat, 1, 2, 3) if !missing(mstat)
    gen widowed = inlist(mstat, 7) if !missing(mstat)
    gen divorced = inlist(mstat, 4, 5, 6) if !missing(mstat)
    label var married "Currently married/partnered"
    label var widowed "Widowed"
    label var divorced "Divorced/separated"

    gen age_sq = age^2
    label var age_sq "Age squared"

    gen ln_hh_income = ln(hh_income + 1)
    label var ln_hh_income "Log household income"

    gen ihs_hh_assets = asinh(hh_assets)
    label var ihs_hh_assets "Household assets (IHS transformation)"

    gen years_since_claim = age - rassageb
    label var years_since_claim "Years since started receiving SS"

    * FRA
    gen fra = .
    replace fra = 65 if rabyear <= 1937
    replace fra = 65 + 2/12  if rabyear == 1938
    replace fra = 65 + 4/12  if rabyear == 1939
    replace fra = 65 + 6/12  if rabyear == 1940
    replace fra = 65 + 8/12  if rabyear == 1941
    replace fra = 65 + 10/12 if rabyear == 1942
    replace fra = 66 if rabyear >= 1943 & rabyear <= 1954
    replace fra = 66 + 2/12  if rabyear == 1955
    replace fra = 66 + 4/12  if rabyear == 1956
    replace fra = 66 + 6/12  if rabyear == 1957
    replace fra = 66 + 8/12  if rabyear == 1958
    replace fra = 66 + 10/12 if rabyear == 1959
    label var fra "Full Retirement Age (based on birth cohort)"

    compress
    save "$derived/hrs_cesd_analysis_ready.dta", replace
    di "Saved: $derived/hrs_cesd_analysis_ready.dta"
    di "CES-D analysis-ready observations: `c(N)'"

    restore
}
else {
    di "CES-D sample file not found -- skipping CES-D derived variable creation"
}

* ============================================================================
* CREATE CES-D FULL SAMPLE (incl pre-claim, for event study)
* ============================================================================

di _n "Creating CES-D full sample with derived variables..."

capture confirm file "$derived/hrs_cesd_full_sample.dta"
if !_rc {
    preserve

    use "$derived/hrs_cesd_full_sample.dta", clear
    di "CES-D full sample loaded: `c(N)' observations"

    * Same derived variables as CES-D analysis-ready
    gen claim_age_c62 = rassageb - 62
    label var claim_age_c62 "Claiming age (centered at 62)"

    gen female = (ragender == 2)
    label var female "Female"

    gen black = (raracem == 2) if !missing(raracem)
    gen other_race = (raracem == 3) if !missing(raracem)
    gen hispanic = (rahispan == 1) if !missing(rahispan)

    gen married = inlist(mstat, 1, 2, 3) if !missing(mstat)
    gen widowed = inlist(mstat, 7) if !missing(mstat)
    gen divorced = inlist(mstat, 4, 5, 6) if !missing(mstat)

    gen age_sq = age^2

    gen ln_hh_income = ln(hh_income + 1)
    gen ihs_hh_assets = asinh(hh_assets)

    gen years_since_claim = age - rassageb
    label var years_since_claim "Years since started receiving SS"

    gen poor_health = (shlt >= 4) if !missing(shlt)
    label var poor_health "Fair/poor health"

    * FRA
    gen fra = .
    replace fra = 65 if rabyear <= 1937
    replace fra = 65 + 2/12  if rabyear == 1938
    replace fra = 65 + 4/12  if rabyear == 1939
    replace fra = 65 + 6/12  if rabyear == 1940
    replace fra = 65 + 8/12  if rabyear == 1941
    replace fra = 65 + 10/12 if rabyear == 1942
    replace fra = 66 if rabyear >= 1943 & rabyear <= 1954
    replace fra = 66 + 2/12  if rabyear == 1955
    replace fra = 66 + 4/12  if rabyear == 1956
    replace fra = 66 + 6/12  if rabyear == 1957
    replace fra = 66 + 8/12  if rabyear == 1958
    replace fra = 66 + 10/12 if rabyear == 1959

    compress
    save "$derived/hrs_cesd_full_analysis_ready.dta", replace
    di "Saved: $derived/hrs_cesd_full_analysis_ready.dta"
    di "CES-D full sample observations: `c(N)'"

    restore
}
else {
    di "CES-D full sample file not found -- skipping"
}

* ============================================================================
* SUMMARY OF KEY VARIABLES
* ============================================================================

di _n "=============================================="
di "Key Variable Summary"
di "=============================================="

sum rassageb claim_age_c62 life_sat fin_strain age female poor_health, sep(0)

log close derived

* ============================================================================
* END OF 03_derived.do
* ============================================================================
