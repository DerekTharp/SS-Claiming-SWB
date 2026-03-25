/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 00_master.do
* Purpose: Master do-file to run all analysis scripts in sequence
*******************************************************************************/

clear all
set more off
set maxvar 32767
version 17

* ============================================================================
* LOAD CONFIGURATION (paths, control macros, constants)
* ============================================================================

* Source config using relative path; user should set working directory to project root
* before running this script (e.g., cd "/path/to/project")
do "code/stata/00_config.do"

* ============================================================================
* EXPECTED RAW DATA FILES
* ============================================================================
/*
Place these files in $raw_data:

1. randhrs1992_2022v1.dta
   - RAND HRS Longitudinal File 2022 (V1)
   - Download from: https://hrsdata.isr.umich.edu/data-products/rand

2. sswealth.dta (or similar)
   - HRS SS Wealth Cross-Wave File
   - Contains: RwCLAIMED, RwSSWRER, RwSSWRNR, RwSSWRXA
   - May need to download separately from HRS restricted data

Note: RASSAGEB (claiming age) should be in the RAND Longitudinal file.
Leave-Behind variables (RwLBSATWLF, etc.) are in Section L of RAND file.
*/

* ============================================================================
* VERIFY PATHS EXIST
* ============================================================================

capture confirm file "$raw_data/."
if _rc {
    di as error "ERROR: Raw data directory not found: $raw_data"
    di as error "Create the directory and place HRS data files there."
    exit 601
}

* Create output directories if they don't exist
capture mkdir "$derived"
capture mkdir "$tables"
capture mkdir "$figures"
capture mkdir "$logs"

* ============================================================================
* RUN ANALYSIS PIPELINE
* ============================================================================

* Start log
cap log close _all
log using "$logs/master_`c(current_date)'.log", replace name(master)

di "=============================================="
di "SS Claiming Timing and Subjective Well-Being"
di "Master Do-File"
di "=============================================="
di "Date: `c(current_date)' `c(current_time)'"
di "Stata version: `c(stata_version)'"
di "=============================================="

* -----------------------------------------------------------------------------
* Step 1: Data Preparation
* -----------------------------------------------------------------------------
di _n "Step 1: Data Preparation..."
timer clear 1
timer on 1

do "$code/01_data_prep.do"

timer off 1
timer list 1

* -----------------------------------------------------------------------------
* Step 2: Sample Restrictions
* -----------------------------------------------------------------------------
di _n "Step 2: Sample Restrictions..."
timer clear 2
timer on 2

do "$code/02_sample.do"

timer off 2
timer list 2

* -----------------------------------------------------------------------------
* Step 3: Derived Variables
* -----------------------------------------------------------------------------
di _n "Step 3: Derived Variables..."
timer clear 3
timer on 3

do "$code/03_derived.do"

timer off 3
timer list 3

* -----------------------------------------------------------------------------
* Step 4: Descriptive Statistics
* -----------------------------------------------------------------------------
di _n "Step 4: Descriptive Statistics..."
timer clear 4
timer on 4

do "$code/04_descriptive.do"

timer off 4
timer list 4

* -----------------------------------------------------------------------------
* Step 5: Main Analysis
* -----------------------------------------------------------------------------
di _n "Step 5: Main Analysis..."
timer clear 5
timer on 5

do "$code/05_analysis.do"

timer off 5
timer list 5

* -----------------------------------------------------------------------------
* Step 6: Robustness Checks
* -----------------------------------------------------------------------------
di _n "Step 6: Robustness Checks..."
timer clear 6
timer on 6

do "$code/06_robustness.do"

timer off 6
timer list 6

* -----------------------------------------------------------------------------
* Step 7: Event Study Analysis
* -----------------------------------------------------------------------------
di _n "Step 7: Event Study Analysis..."
timer clear 7
timer on 7

do "$code/07_event_study.do"

timer off 7
timer list 7

* -----------------------------------------------------------------------------
* Step 8: Extended Sensitivity Analyses
* -----------------------------------------------------------------------------
di _n "Step 8: Extended Sensitivity Analyses..."
timer clear 8
timer on 8

do "$code/08_sensitivity.do"

timer off 8
timer list 8

* ============================================================================
* COMPLETION
* ============================================================================

di _n "=============================================="
di "Pipeline Complete"
di "Date: `c(current_date)' `c(current_time)'"
di "=============================================="

log close master
