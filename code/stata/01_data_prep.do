/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 01_data_prep.do
* Purpose: Load and merge HRS data files, keep relevant variables
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
cap log close data_prep
log using "$logs/01_data_prep.log", replace name(data_prep)

di "=============================================="
di "01_data_prep.do: Data Preparation"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* ============================================================================
* PART 1: LOAD RAND HRS LONGITUDINAL FILE
* ============================================================================

di _n "Loading RAND HRS Longitudinal File..."

use "$raw_data/randhrs1992_2022v1.dta", clear

di "Observations: `c(N)'"
di "Variables: `c(k)'"

* ============================================================================
* PART 2: KEEP RELEVANT VARIABLES
* ============================================================================

di _n "Selecting relevant variables..."

* -----------------------------------------------------------------------------
* Identifiers
* -----------------------------------------------------------------------------
local id_vars hhidpn hhid pn

* -----------------------------------------------------------------------------
* Time-invariant demographics
* -----------------------------------------------------------------------------
local demog_static ///
    ragender        /* Gender (1=Male, 2=Female) */ ///
    raracem         /* Race (1=White, 2=Black, 3=Other) */ ///
    rahispan        /* Hispanic (0=No, 1=Yes) */ ///
    raeduc          /* Education (1=<HS, 2=GED, 3=HS, 4=Some Col, 5=Col+) */ ///
    raedyrs         /* Years of education */ ///
    rabdate         /* Birth date (SAS date) */ ///
    rabyear         /* Birth year */ ///
    rabmonth        /* Birth month */

* -----------------------------------------------------------------------------
* Social Security claiming variables
* -----------------------------------------------------------------------------
local ss_claim ///
    rassageb        /* Age (years) started receiving SS */ ///
    rassagem        /* Age (months) started receiving SS */

* -----------------------------------------------------------------------------
* Wave-varying variables (Waves 8-16 = 2006-2022)
* We use loops to generate variable lists for multiple waves
* -----------------------------------------------------------------------------

* Age at interview
local age_vars ""
forval w = 8/16 {
    local age_vars `age_vars' r`w'agey_b
}

* Self-reported health
local health_vars ""
forval w = 8/16 {
    local health_vars `health_vars' r`w'shlt
}

* Hospitalized since last wave
local hosp_vars ""
forval w = 8/16 {
    local hosp_vars `hosp_vars' r`w'hosp
}

* Number of health conditions
local conde_vars ""
forval w = 8/16 {
    local conde_vars `conde_vars' r`w'conde
}

* Marital status
local mstat_vars ""
forval w = 8/16 {
    local mstat_vars `mstat_vars' r`w'mstat
}

* Labor force status (for retirement robustness check)
local lbrf_vars ""
forval w = 8/16 {
    local lbrf_vars `lbrf_vars' r`w'lbrf
}

* Proxy interview indicator
local proxy_vars ""
forval w = 8/16 {
    local proxy_vars `proxy_vars' r`w'proxy
}

* Subjective survival probability
* Note: liv75 available waves 8-16; liv85 only waves 1-4 (not in our sample)
local liv75_vars ""
forval w = 8/16 {
    local liv75_vars `liv75_vars' r`w'liv75
}

* CES-D depression scale (core interview, 0-8 count)
local cesd_vars ""
forval w = 8/16 {
    local cesd_vars `cesd_vars' r`w'cesd
}

* Respondent-level weight (for core-interview outcomes like CES-D)
local resp_weight_vars ""
forval w = 8/16 {
    local resp_weight_vars `resp_weight_vars' r`w'wtresp
}

* Household income and assets
* Note: Assets use h*atotb (bottom-coded total assets)
local income_vars ""
local asset_vars ""
forval w = 8/16 {
    local income_vars `income_vars' h`w'itot
    local asset_vars  `asset_vars'  h`w'atotb
}

* -----------------------------------------------------------------------------
* Leave-Behind psychosocial variables (Waves 8-16)
* Note: LB collected in rotating subsamples, so not all waves for each person
* -----------------------------------------------------------------------------

* LB eligibility and completion
local lb_admin ""
forval w = 8/16 {
    local lb_admin `lb_admin' r`w'lbelig r`w'lbcomp
}

* LB weight
local lb_weight ""
forval w = 8/16 {
    local lb_weight `lb_weight' r`w'lbwgtr
}

* Life satisfaction (Diener scale)
local lb_lifesat ""
forval w = 8/16 {
    local lb_lifesat `lb_lifesat' r`w'lbsatwlf
}

* Positive and negative affect
* Note: Variable names vary by wave - use wildcard pattern and rename later
* Wave 8: r8lbposaffect6, r8lbnegaffect6
* Waves 9+: r*lbposaffect, r*lbnegaffect
local lb_affect "r8lbposaffect6 r8lbnegaffect6"
forval w = 9/16 {
    local lb_affect `lb_affect' r`w'lbposaffect r`w'lbnegaffect
}

* Loneliness
local lb_lonely ""
forval w = 8/16 {
    local lb_lonely `lb_lonely' r`w'lblonely3
}

* Financial strain/problems
local lb_finprb ""
forval w = 8/16 {
    local lb_finprb `lb_finprb' r`w'lbfinprb
}

* Personality traits (Big 5)
* Available: Conscientiousness (r*lbcon5), Openness (r*lbopen)
* Not in RAND file: Neuroticism, Agreeableness, Extraversion
local lb_person ""
forval w = 8/16 {
    local lb_person `lb_person' r`w'lbcon5 r`w'lbopen
}

* Purpose in life
local lb_purpose ""
forval w = 8/16 {
    local lb_purpose `lb_purpose' r`w'lbpurpose
}

* Domain-specific satisfaction (LB)
* Financial satisfaction (waves 8-16)
local lb_satfin ""
forval w = 8/16 {
    local lb_satfin `lb_satfin' r`w'lbsatfin
}

* Income satisfaction (waves 11-16 only; not available waves 8-10)
local lb_satinc ""
forval w = 11/16 {
    local lb_satinc `lb_satinc' r`w'lbsatinc
}

* -----------------------------------------------------------------------------
* Combine all variable lists
* -----------------------------------------------------------------------------

local keepvars ///
    `id_vars' ///
    `demog_static' ///
    `ss_claim' ///
    `age_vars' ///
    `health_vars' ///
    `hosp_vars' ///
    `conde_vars' ///
    `mstat_vars' ///
    `lbrf_vars' ///
    `proxy_vars' ///
    `liv75_vars' ///
    `cesd_vars' ///
    `resp_weight_vars' ///
    `income_vars' ///
    `asset_vars' ///
    `lb_admin' ///
    `lb_weight' ///
    `lb_lifesat' ///
    `lb_affect' ///
    `lb_lonely' ///
    `lb_finprb' ///
    `lb_person' ///
    `lb_purpose' ///
    `lb_satfin' ///
    `lb_satinc'

* Keep only variables that exist in the dataset
local existing_vars ""
foreach var of local keepvars {
    capture confirm variable `var'
    if !_rc {
        local existing_vars `existing_vars' `var'
    }
    else {
        di as text "Note: Variable `var' not found in dataset"
    }
}

keep `existing_vars'

di "Variables retained: `c(k)'"

* ============================================================================
* PART 3: MERGE SS WEALTH FILE (IF SEPARATE)
* ============================================================================

di _n "Checking for SS Wealth file..."

* Check if SS Wealth file exists
capture confirm file "$raw_data/sswealth.dta"

if !_rc {
    di "Merging SS Wealth file..."

    * Preserve current data
    preserve

    * Load SS Wealth file to identify available variables
    use "$raw_data/sswealth.dta", clear

    * Keep claiming and benefit variables (waves 8-16)
    local ssw_vars "hhidpn"
    forval w = 8/16 {
        foreach v in claimed sswrer sswrnr sswrxa {
            capture confirm variable r`w'`v'
            if !_rc {
                local ssw_vars `ssw_vars' r`w'`v'
            }
        }
    }
    keep `ssw_vars'

    tempfile sswealth_temp
    save `sswealth_temp'

    * Restore and merge
    restore
    merge 1:1 hhidpn using `sswealth_temp', keep(master match) nogen

    di "Observations after merge: `c(N)'"
}
else {
    di "SS Wealth file not found at $raw_data/sswealth.dta"
    di "Proceeding with RAND Longitudinal file only."
    di "Note: RwCLAIMED and SS benefit variables may be included in RAND file."
}

* ============================================================================
* PART 4: BASIC CLEANING
* ============================================================================

di _n "Basic data cleaning..."

* Label key variables if not already labeled
capture label define ragender_lbl 1 "Male" 2 "Female"
capture label values ragender ragender_lbl

capture label define raeduc_lbl 1 "Less than HS" 2 "GED" 3 "HS graduate" ///
    4 "Some college" 5 "College+"
capture label values raeduc raeduc_lbl

capture label define raracem_lbl 1 "White" 2 "Black" 3 "Other"
capture label values raracem raracem_lbl

* Convert birth date to birth year if needed
capture confirm variable rabyear
if _rc {
    gen rabyear = year(rabdate) if rabdate < .
}

* ============================================================================
* PART 5: SAVE MERGED DATA
* ============================================================================

di _n "Saving merged dataset..."

compress
save "$derived/hrs_merged.dta", replace

di "Saved: $derived/hrs_merged.dta"
di "Final observations: `c(N)'"
di "Final variables: `c(k)'"

* ============================================================================
* PART 6: SUMMARY
* ============================================================================

di _n "=============================================="
di "Data Preparation Summary"
di "=============================================="

* Check key variables exist
di _n "Key variable availability:"

foreach var in hhidpn ragender raeduc rabyear rassageb {
    capture confirm variable `var'
    if !_rc {
        di "  `var': " _col(20) "Available"
    }
    else {
        di as error "  `var': " _col(20) "MISSING"
    }
}

* Check LB variables for a sample wave
di _n "Leave-Behind variables (Wave 10 sample):"
foreach var in r10lbelig r10lbcomp r10lbsatwlf r10lbfinprb r10lbwgtr {
    capture confirm variable `var'
    if !_rc {
        qui count if !missing(`var')
        di "  `var': " _col(20) "`r(N)' non-missing"
    }
    else {
        di as text "  `var': " _col(20) "Not found"
    }
}

log close data_prep

* ============================================================================
* END OF 01_data_prep.do
* ============================================================================
