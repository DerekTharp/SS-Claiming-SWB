/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 00_test_syntax.do
* Purpose: Test syntax of all do-files using simulated data
*
* WARNING: This script creates simulated data in a TEMPORARY directory for
* syntax testing. It does NOT write to $raw_data and will not overwrite any
* real HRS data files. Temporary files are cleaned up at the end.
*******************************************************************************/

clear all
set more off
version 17

* ============================================================================
* CONFIGURE PATHS
* ============================================================================

do "code/stata/00_config.do"

* Override raw_data path to use a temporary directory for testing
local test_dir "/tmp/test_ss_claiming"
capture mkdir "`test_dir'"
global raw_data "`test_dir'"

* Create output directories
capture mkdir "$derived"
capture mkdir "$tables"
capture mkdir "$figures"
capture mkdir "$logs"

* ============================================================================
* CREATE SIMULATED DATA FOR TESTING
* ============================================================================

di "Creating simulated HRS-like data for syntax testing..."
di "Simulated data will be saved to: $raw_data (temporary directory)"

clear
set seed 12345
set obs 5000

* Identifiers
gen hhidpn = _n
gen hhid = floor((_n-1)/2) + 1
gen pn = mod(_n-1, 2) + 1

* Time-invariant demographics
gen ragender = runiformint(1, 2)
gen raracem = runiformint(1, 3)
gen rahispan = rbinomial(1, 0.15)
gen raeduc = runiformint(1, 5)
gen raedyrs = 8 + raeduc * 2 + runiformint(0, 2)
gen rabyear = runiformint(1931, 1959)
gen rabmonth = runiformint(1, 12)
gen rabdate = mdy(rabmonth, 15, rabyear)

* SS claiming age (62-70) with realistic distribution
gen double u_claim = runiform()
gen rassageb = .
replace rassageb = 62 if u_claim < 0.40
replace rassageb = 63 if u_claim >= 0.40 & u_claim < 0.48
replace rassageb = 64 if u_claim >= 0.48 & u_claim < 0.55
replace rassageb = 65 if u_claim >= 0.55 & u_claim < 0.68
replace rassageb = 66 if u_claim >= 0.68 & u_claim < 0.82
replace rassageb = 67 if u_claim >= 0.82 & u_claim < 0.90
replace rassageb = 68 if u_claim >= 0.90 & u_claim < 0.95
replace rassageb = 69 if u_claim >= 0.95 & u_claim < 0.98
replace rassageb = 70 if u_claim >= 0.98
drop u_claim
gen rassagem = rassageb * 12 + runiformint(0, 11)

di "Simulated claiming age distribution:"
tab rassageb

* Generate wave-varying variables for waves 8-16
forval w = 8/16 {
    gen r`w'agey_b = rabyear + 1992 + (`w'-1)*2 - rabyear
    replace r`w'agey_b = 2006 + (`w'-8)*2 - rabyear

    gen r`w'shlt = runiformint(1, 5)
    gen r`w'hosp = rbinomial(1, 0.15)
    gen r`w'conde = rpoisson(2)
    gen r`w'mstat = runiformint(1, 8)
    gen r`w'proxy = rbinomial(1, 0.05)
    gen r`w'wtresp = runiform(0.5, 2)
    gen r`w'cesd = rpoisson(1.5)
    replace r`w'cesd = min(r`w'cesd, 8)
    gen r`w'liv75 = runiform(20, 100)
    gen r`w'liv85 = runiform(10, 80)
    gen h`w'itot = exp(rnormal(10.5, 1))
    gen h`w'atotb = exp(rnormal(12, 1.5)) - 50000
    gen r`w'lbelig = rbinomial(1, 0.9)
    gen r`w'lbcomp = cond(r`w'lbelig == 1, runiformint(1, 5), 5)
    replace r`w'lbcomp = 1 if r`w'lbcomp <= 2
    replace r`w'lbcomp = 5 if r`w'lbcomp > 2 & r`w'lbcomp < 5
    gen r`w'lbwgtr = runiform(0.5, 2)
    gen r`w'lbsatwlf = rnormal(5, 1.2)
    replace r`w'lbsatwlf = 1 if r`w'lbsatwlf < 1
    replace r`w'lbsatwlf = 7 if r`w'lbsatwlf > 7
    gen r`w'lbsatfin = rnormal(3, 1)
    replace r`w'lbsatfin = max(1, min(5, r`w'lbsatfin))
    gen r`w'lbsatinc = rnormal(3, 1)
    replace r`w'lbsatinc = max(1, min(5, r`w'lbsatinc))
    gen r`w'lbposaffect = rnormal(3.5, 0.8)
    gen r`w'lbnegaffect = rnormal(1.8, 0.6)
    gen r`w'lblonely3 = rnormal(4, 1)
    gen r`w'lbfinprb = rnormal(2, 0.8)
    gen r`w'lbneuro = rnormal(2.5, 0.7)
    gen r`w'lbcon5 = rnormal(3.5, 0.6)
    gen r`w'lbopen = rnormal(3.2, 0.6)
    gen r`w'lbpurpose = rnormal(4, 0.9)
}

* Add correlation structure (later claimers: healthier, higher income)
forval w = 8/16 {
    replace r`w'shlt = r`w'shlt - 0.1*(rassageb - 62)
    replace r`w'shlt = max(1, min(5, r`w'shlt))
    replace h`w'itot = h`w'itot * (1 + 0.05*(rassageb - 62))
    replace r`w'lbsatwlf = r`w'lbsatwlf + 0.08*(rassageb - 62) + rnormal(0, 0.3)
    replace r`w'lbfinprb = r`w'lbfinprb - 0.05*(rassageb - 62) + rnormal(0, 0.2)
}

* Save simulated data to temporary directory
save "$raw_data/randhrs1992_2022v1.dta", replace

di "Simulated data saved with `c(N)' observations and `c(k)' variables"
di "Location: $raw_data/randhrs1992_2022v1.dta (temporary)"

* ============================================================================
* TEST 01_data_prep.do
* ============================================================================

di _n "============================================================"
di "TESTING 01_data_prep.do"
di "============================================================"

do "$code/01_data_prep.do"

confirm file "$derived/hrs_merged.dta"
di "01_data_prep.do: PASSED"

* ============================================================================
* TEST 02_sample.do
* ============================================================================

di _n "============================================================"
di "TESTING 02_sample.do"
di "============================================================"

do "$code/02_sample.do"

confirm file "$derived/hrs_analytic.dta"
di "02_sample.do: PASSED"

* ============================================================================
* TEST 03_derived.do
* ============================================================================

di _n "============================================================"
di "TESTING 03_derived.do"
di "============================================================"

do "$code/03_derived.do"

confirm file "$derived/hrs_analysis_ready.dta"
di "03_derived.do: PASSED"

* ============================================================================
* TEST 04_descriptive.do
* ============================================================================

di _n "============================================================"
di "TESTING 04_descriptive.do"
di "============================================================"

capture which estout
if _rc {
    di "Installing estout package..."
    ssc install estout, replace
}

do "$code/04_descriptive.do"

di "04_descriptive.do: PASSED"

* ============================================================================
* TEST 05_analysis.do
* ============================================================================

di _n "============================================================"
di "TESTING 05_analysis.do"
di "============================================================"

do "$code/05_analysis.do"

di "05_analysis.do: PASSED"

* ============================================================================
* TEST 06_robustness.do
* ============================================================================

di _n "============================================================"
di "TESTING 06_robustness.do"
di "============================================================"

do "$code/06_robustness.do"

di "06_robustness.do: PASSED"

* ============================================================================
* TEST 07_event_study.do
* ============================================================================

di _n "============================================================"
di "TESTING 07_event_study.do"
di "============================================================"

do "$code/07_event_study.do"

di "07_event_study.do: PASSED"

* ============================================================================
* TEST 08_sensitivity.do
* ============================================================================

di _n "============================================================"
di "TESTING 08_sensitivity.do"
di "============================================================"

do "$code/08_sensitivity.do"

di "08_sensitivity.do: PASSED"

* ============================================================================
* CLEAN UP TEMPORARY FILES
* ============================================================================

di _n "============================================================"
di "CLEANING UP TEMPORARY FILES"
di "============================================================"

capture erase "`test_dir'/randhrs1992_2022v1.dta"
di "Removed temporary simulated data from: `test_dir'"

* ============================================================================
* SUMMARY
* ============================================================================

di _n "============================================================"
di "ALL TESTS PASSED"
di "============================================================"
di _n "The code syntax is valid and runs successfully on simulated data."
di "To run on real HRS data:"
di "  1. Register at https://hrsdata.isr.umich.edu/user/register"
di "  2. Download RAND HRS Longitudinal File 2022 (Stata format)"
di "  3. Place randhrs1992_2022v1.dta in: $raw_data"
di "  4. Run: do 00_master.do"
di "============================================================"
