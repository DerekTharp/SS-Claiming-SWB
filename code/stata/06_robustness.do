/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 06_robustness.do
* Purpose: Robustness checks and sensitivity analyses
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
cap log close robust
log using "$logs/06_robustness.log", replace name(robust)

di "=============================================="
di "06_robustness.do: Robustness Checks"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* Load analysis-ready data
use "$derived/hrs_analysis_ready.dta", clear

* ============================================================================
* ESTIMATION STRATEGY: Weighted OLS with Person-Clustered Standard Errors
* ============================================================================
* All regressions use: reg y x [pw=lb_weight], vce(cluster hhidpn)

di "Estimation: Weighted OLS with person-clustered robust standard errors"

* ============================================================================
* CREATE SAMPLE INDICATOR
* ============================================================================
* Ensures all robustness checks use the exact same sample as the main analysis

gen sample_primary = !missing(life_sat, claim_age_c62, female, raeduc, shlt, poor_health, conde, ///
    married, widowed, divorced, age, black, other_race, hispanic, wave)

* Run primary regression first to get regression N (may differ from indicator count)
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local n_primary = e(N)
di _n "=============================================="
di "PRIMARY SAMPLE FOR ALL ROBUSTNESS CHECKS: N = `n_primary'"
di "=============================================="
di "All robustness regressions will use: if sample_primary == 1"
di "Note: N reflects regression N (e(N)), not indicator count"

di "Starting observations: `c(N)'"

* ============================================================================
* ROBUSTNESS 2: ALTERNATIVE CLAIMING AGE CATEGORIZATION
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 2: Alternative Claiming Categories"
di "=============================================="

* Binary: age 62 only vs. 66+
gen claim_62_vs_66 = .
replace claim_62_vs_66 = 0 if rassageb >= 62 & rassageb < 63
replace claim_62_vs_66 = 1 if rassageb >= 66 & rassageb <= 70

di _n "Model: Age 62 vs. Age 66+ (Total Effect controls)"
reg life_sat claim_62_vs_66 $controls_total $wave_fe [pw=lb_weight] if claim_62_vs_66 != . & sample_primary == 1, vce(cluster hhidpn)
estimates store rob2a

* Four categories using half-open intervals
gen claim_cat4 = .
replace claim_cat4 = 1 if rassageb >= 62 & rassageb < 63
replace claim_cat4 = 2 if rassageb >= 63 & rassageb < 66
replace claim_cat4 = 3 if rassageb >= 66 & rassageb < 67
replace claim_cat4 = 4 if rassageb >= 67 & rassageb <= 70

label define claim_cat4_lbl 1 "62" 2 "63-65" 3 "66" 4 "67-70"
label values claim_cat4 claim_cat4_lbl

di _n "Model: Four claiming categories (Total Effect controls)"
reg life_sat i.claim_cat4 $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
testparm i.claim_cat4
estimates store rob2b

* FRA-relative categories (from 03_derived.do)
di _n "Model: FRA-relative categories (Early/At FRA/Delayed)"
capture confirm variable claim_cat_fra
if !_rc {
    reg life_sat i.claim_cat_fra $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
    testparm i.claim_cat_fra
    estimates store rob2c_fra

    di _n "Model: Continuous claiming age relative to FRA"
    reg life_sat claim_rel_fra $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
    estimates store rob2d_fra_cont
}
else {
    di "FRA-relative categories not found. Create using 03_derived.do."
}

* Export
capture confirm variable claim_cat_fra
if !_rc {
    esttab rob2a rob2b rob2c_fra rob2d_fra_cont using "$tables/robustness_categories.csv", ///
        se star(* 0.05 ** 0.01 *** 0.001) ///
        stats(N r2_a) ///
        mtitles("62 vs 66+" "4 Categories" "FRA-relative" "Cont. FRA") ///
        csv replace ///
        title("Alternative Claiming Age Categorizations")
}
else {
    esttab rob2a rob2b using "$tables/robustness_categories.csv", ///
        se star(* 0.05 ** 0.01 *** 0.001) ///
        stats(N r2_a) ///
        mtitles("62 vs 66+" "4 Categories") ///
        csv replace ///
        title("Alternative Claiming Age Categorizations")
}

* ============================================================================
* ROBUSTNESS 3: DIFFERENT CONTROL SPECIFICATIONS
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 3: Alternative Control Sets"
di "=============================================="

* Minimal controls (demographics only)
di _n "Minimal controls:"
reg life_sat claim_age_c62 $demo $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store rob3a

* Direct effect (includes income/assets but not survival expectations)
di _n "Direct effect (no survival expectations):"
reg life_sat claim_age_c62 $demo $health $econ $marital $age_controls $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store rob3b

* Total effect (preferred specification)
di _n "Total effect (no income/assets):"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store rob3_total

* With personality controls (potential over-control)
local personality_vars ""
capture confirm variable conscientious_z
if !_rc {
    local personality_vars `personality_vars' conscientious_z
}
capture confirm variable openness_z
if !_rc {
    local personality_vars `personality_vars' openness_z
}

if "`personality_vars'" != "" {
    di _n "With personality controls (`personality_vars'):"
    reg life_sat claim_age_c62 $controls_total `personality_vars' $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
    estimates store rob3c

    esttab rob3a rob3b rob3_total rob3c using "$tables/robustness_controls.csv", ///
        se star(* 0.05 ** 0.01 *** 0.001) ///
        stats(N r2_a) ///
        mtitles("Minimal" "No Survival" "Total Effect" "+ Personality") ///
        csv replace ///
        addnotes("Total Effect: excludes income/assets (preferred specification)")
}
else {
    di _n "Personality controls not available, skipping"

    esttab rob3a rob3b rob3_total using "$tables/robustness_controls.csv", ///
        se star(* 0.05 ** 0.01 *** 0.001) ///
        stats(N r2_a) ///
        mtitles("Minimal" "No Survival" "Total Effect") ///
        csv replace ///
        addnotes("Total Effect: excludes income/assets (preferred specification)")
}

* ============================================================================
* ROBUSTNESS 4: FIXED EFFECTS vs. POOLED OLS
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 4: Person Fixed Effects"
di "=============================================="

* With person FE, claiming age is absorbed (time-invariant);
* we instead examine years since claiming

xtset hhidpn wave

di _n "Random effects model (unweighted):"
xtreg life_sat claim_age_c62 $controls_total i.wave if sample_primary == 1, re
estimates store rob4_re

di _n "Fixed effects with years since claiming (unweighted):"
xtreg life_sat years_since_claim $health_basic i.wave if sample_primary == 1, fe
estimates store rob4_fe

* ============================================================================
* ROBUSTNESS 5: SUBSAMPLE ANALYSIS
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 5: Subsample Analysis"
di "=============================================="

di _n "Men only:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if female == 0 & sample_primary == 1, vce(cluster hhidpn)
estimates store rob5_men

di _n "Women only:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if female == 1 & sample_primary == 1, vce(cluster hhidpn)
estimates store rob5_women

di _n "Less than college:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if educ_col_plus == 0 & sample_primary == 1, vce(cluster hhidpn)
estimates store rob5_nocol

di _n "College+:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if educ_col_plus == 1 & sample_primary == 1, vce(cluster hhidpn)
estimates store rob5_col

di _n "Married:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if married == 1 & sample_primary == 1, vce(cluster hhidpn)
estimates store rob5_married

di _n "Not married:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if married == 0 & sample_primary == 1, vce(cluster hhidpn)
estimates store rob5_notmarried

esttab rob5_men rob5_women rob5_nocol rob5_col rob5_married rob5_notmarried ///
    using "$tables/robustness_subsamples.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Men" "Women" "No Col" "College" "Married" "Not Mar") ///
    csv replace ///
    addnotes("All models use Total Effect controls (no income/assets)")

* ============================================================================
* ROBUSTNESS 6: TIMING - YEARS SINCE CLAIMING
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 6: Years Since Claiming"
di "=============================================="

* Years-since-claiming interaction has identification issues due to collinearity
* (age = claiming_age + years_since_claim). See 07_event_study.do for the
* event-study design that addresses this.
di _n "NOTE: Years since claiming interaction omitted"
di "See 07_event_study.do for event-time analysis"

* ============================================================================
* ROBUSTNESS 7: UNWEIGHTED ESTIMATES
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 7: Unweighted Estimates"
di "=============================================="

di _n "Unweighted OLS:"
reg life_sat claim_age_c62 $controls_total i.wave if sample_primary == 1, cluster(hhidpn)
estimates store rob7_unweighted

di _n "Comparison: Weighted vs Unweighted"
estimates table rob7_unweighted, b(%9.4f) se(%9.4f)

* ============================================================================
* ROBUSTNESS 8: STANDARDIZED OUTCOMES
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 8: Standardized Outcomes"
di "=============================================="

di _n "Standardized life satisfaction:"
reg life_sat_z claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store rob8_std

di "Interpretation: 1-year delay in claiming associated with"
di "  " %5.3f _b[claim_age_c62] " SD change in life satisfaction"

* ============================================================================
* ROBUSTNESS 9: NONLINEAR EFFECTS
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 9: Nonlinear Effects"
di "=============================================="

* Quadratic specification
capture drop claim_age_sq
gen claim_age_sq = claim_age_c62^2
reg life_sat claim_age_c62 claim_age_sq $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store rob9_quad

testparm claim_age_sq

* Restricted cubic splines
capture drop claim_rcs*
capture mkspline claim_rcs = rassageb if sample_primary == 1, nknots(4) cubic
if !_rc {
    reg life_sat claim_rcs* $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
    estimates store rob9_spline
    testparm claim_rcs*
}

* ============================================================================
* ROBUSTNESS 10: DROPPING OUTLIERS
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 10: Outlier Sensitivity"
di "=============================================="

sum life_sat, detail
scalar ls_p1 = r(p1)
scalar ls_p99 = r(p99)

di _n "Dropping life satisfaction outliers (1st/99th percentile):"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] ///
    if life_sat >= ls_p1 & life_sat <= ls_p99 & sample_primary == 1, vce(cluster hhidpn)
estimates store rob10

* ============================================================================
* ROBUSTNESS 11: INSTRUMENTAL VARIABLES - SKIPPED
* ============================================================================
* IV analysis dropped because the FRA-distance instrument is weak (F < 1).
* FRA variation does not meaningfully predict claiming age in this sample.

di _n "=============================================="
di "ROBUSTNESS 11: IV Estimation - SKIPPED"
di "=============================================="
di "IV analysis removed due to weak instrument (F-stat = 0.13)"
di "FRA variation does not predict claiming behaviour in this sample"
di "See Technical Appendix for discussion"

* ============================================================================
* ROBUSTNESS 12: OSTER (2019) BOUNDS FOR SELECTION ON UNOBSERVABLES
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 12: Oster Bounds for Selection Bias"
di "=============================================="

capture which psacalc
if _rc {
    di "Installing psacalc package for Oster bounds..."
    ssc install psacalc, replace
}

* Uncontrolled regression on the primary sample
quietly reg life_sat claim_age_c62 [pw=lb_weight] if sample_primary == 1
scalar r2_uncontrolled = e(r2)

* Controlled regression using primary specification on the primary sample
quietly reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1
scalar r2_controlled = e(r2)
scalar beta_controlled = _b[claim_age_c62]

di "R-squared (uncontrolled): " r2_uncontrolled
di "R-squared (controlled): " r2_controlled
di "Beta (controlled): " beta_controlled

* Oster bounds with Rmax = 1.3 * R-squared (conventional)
scalar rmax_val = min(1, 1.3 * r2_controlled)
di "Using Rmax = " rmax_val

capture {
    psacalc beta claim_age_c62, delta(1) rmax(`=rmax_val')

    di _n "Oster (2019) Bounds:"
    di "If selection on unobservables equals selection on observables (delta=1),"
    di "and Rmax = " rmax_val ", the bias-adjusted estimate is shown above."
    di "If the bounded estimate retains the same sign, results are robust to moderate selection."
}
if _rc {
    * Manual Oster calculation if psacalc fails
    di "psacalc failed -- computing bounds manually..."

    quietly reg life_sat claim_age_c62 [pw=lb_weight] if sample_primary == 1
    scalar beta_uncontrolled = _b[claim_age_c62]

    * Oster formula: beta* = beta_c - delta * (beta_u - beta_c) * (Rmax - R2_c) / (R2_c - R2_u)
    scalar delta = 1
    scalar adjustment = delta * (beta_uncontrolled - beta_controlled) * ///
        (rmax_val - r2_controlled) / (r2_controlled - r2_uncontrolled)
    scalar beta_bounded = beta_controlled - adjustment

    di _n "Oster (2019) Bounds (manual calculation):"
    di "Beta (uncontrolled): " beta_uncontrolled
    di "Beta (controlled): " beta_controlled
    di "Beta (bounded, delta=1): " beta_bounded
    di ""
    di "If bounded estimate has same sign as controlled estimate, results"
    di "are robust to selection on unobservables proportional to observables."
}

* ============================================================================
* ROBUSTNESS 13: FORMAL EQUIVALENCE TESTING (TOST)
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS 13: Two One-Sided Tests (TOST) for Equivalence"
di "=============================================="

* TOST procedure: tests whether the effect is practically equivalent to zero
* H0: |effect| >= delta (meaningful effect exists)
* H1: |effect| < delta (effect is practically zero)
* Delta = 0.1 SD, a conventional "small effect" threshold

quietly reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local beta = _b[claim_age_c62]
local se = _se[claim_age_c62]
local n = e(N)
local df = e(df_r)

* Compute SD from data
qui sum life_sat if sample_primary == 1
local sd_lifesat = r(sd)

local beta_sd = `beta' / `sd_lifesat'
local se_sd = `se' / `sd_lifesat'

* For 8-year delay (62 -> 70)
local beta_8yr = `beta' * 8
local se_8yr = `se' * 8
local beta_8yr_sd = `beta_8yr' / `sd_lifesat'

* Equivalence bounds
local delta = $tost_delta
local delta_raw = `delta' * `sd_lifesat'

di _n "Primary specification estimate:"
di "  Beta (per year): " %7.4f `beta' " (SE = " %6.4f `se' ")"
di "  Beta (8-year delay): " %7.4f `beta_8yr' " (SE = " %6.4f `se_8yr' ")"
di "  In SD units (8-year): " %7.4f `beta_8yr_sd' " SD"
di "  N = `n'"

di _n "Equivalence bounds:"
di "  Delta = +/-`delta' SD = +/-" %5.3f `delta_raw' " raw units"
di "  For 8-year delay, equivalence requires: |effect| < " %5.3f `delta' " SD"

* TOST two one-sided t-tests using regression degrees of freedom
local t_lower = (`beta_8yr_sd' - (-`delta')) / (`se_8yr' / `sd_lifesat')
local t_upper = (`beta_8yr_sd' - (`delta')) / (`se_8yr' / `sd_lifesat')

local p_lower = 1 - ttail(`df', `t_lower')
local p_upper = ttail(`df', `t_upper')
local tost_p = max(`p_lower', `p_upper')

di _n "TOST Results (8-year delay vs +/-`delta' SD bounds):"
di "  Test 1 (effect > lower bound): t = " %6.3f `t_lower' ", p = " %6.4f `p_lower'
di "  Test 2 (effect < upper bound): t = " %6.3f `t_upper' ", p = " %6.4f `tost_p'
di "  TOST p-value (max of both): p = " %6.4f `tost_p'

if `tost_p' < 0.05 {
    di _n "  CONCLUSION: Equivalence demonstrated at alpha = 0.05"
    di "  The effect is statistically equivalent to zero (within +/-`delta' SD bounds)"
}
else {
    di _n "  CONCLUSION: Cannot demonstrate equivalence at alpha = 0.05"
    di "  The 90% CI can still inform practical significance"
}

* 90% CI for TOST (corresponds to two one-sided alpha = 0.05 tests)
local ci90_lo = `beta_8yr_sd' - invttail(`df', 0.05) * (`se_8yr' / `sd_lifesat')
local ci90_hi = `beta_8yr_sd' + invttail(`df', 0.05) * (`se_8yr' / `sd_lifesat')

di _n "90% Confidence Interval (8-year effect in SD units):"
di "  [" %6.4f `ci90_lo' ", " %6.4f `ci90_hi' "]"
di "  Equivalence bounds: [-`delta', +`delta']"

if `ci90_lo' > -`delta' & `ci90_hi' < `delta' {
    di "  90% CI falls entirely within equivalence bounds -- equivalence demonstrated"
}
else {
    di "  90% CI extends beyond equivalence bounds -- equivalence not demonstrated"
}

* Export TOST results
file open tost using "$tables/equivalence_tost.csv", write replace
file write tost `"="Equivalence Testing (TOST): 8-Year Delay Effect""' _n
file write tost _n
file write tost `"="Parameter","Value""' _n
file write tost `"="Beta (8-year delay, raw)","`=string(`beta_8yr', "%7.4f")'""' _n
file write tost `"="SE (8-year delay)","`=string(`se_8yr', "%7.4f")'""' _n
file write tost `"="Beta (8-year delay, SD units)","`=string(`beta_8yr_sd', "%7.4f")'""' _n
file write tost `"="Equivalence bounds (SD)","+-`delta'""' _n
file write tost `"="TOST p-value","`=string(`tost_p', "%6.4f")'""' _n
file write tost `"="90% CI lower (SD)","`=string(`ci90_lo', "%6.4f")'""' _n
file write tost `"="90% CI upper (SD)","`=string(`ci90_hi', "%6.4f")'""' _n
file write tost `"="N","`n'""' _n
file write tost _n
file write tost `"="Note: TOST tests whether 8-year delay effect is within +-`delta' SD of zero""' _n
file write tost `"="Equivalence demonstrated if TOST p < 0.05 or 90% CI within bounds""' _n
file close tost

di _n "TOST results exported to: $tables/equivalence_tost.csv"

* ============================================================================
* SUMMARY TABLE
* ============================================================================

di _n "=============================================="
di "ROBUSTNESS SUMMARY"
di "=============================================="

estimates dir

local est_list ""
foreach est in rob3a rob3_total rob3c rob7_unweighted rob8_std rob9_quad rob10 {
    capture estimates describe `est'
    if !_rc {
        local est_list `est_list' `est'
    }
}

if "`est_list'" != "" {
    esttab `est_list' using "$tables/robustness_summary.csv", ///
        keep(claim_age_c62) ///
        se star(* 0.05 ** 0.01 *** 0.001) ///
        stats(N r2_a) ///
        csv replace ///
        title("Robustness Checks: Coefficient on Claiming Age") ///
        addnotes("All models use sample_primary; primary weighted N=`n_primary'" ///
                 "N varies slightly across specifications (unweighted model includes obs with missing weights)" ///
                 "All weighted estimates use person-clustered robust SEs")
}

* ============================================================================
* COMPLETION
* ============================================================================

di _n "=============================================="
di "Robustness Checks Complete"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

di _n "Output files:"
di "  $tables/robustness_categories.csv    - Alternative claiming categorizations"
di "  $tables/robustness_controls.csv      - Control set variations"
di "  $tables/robustness_subsamples.csv    - Subsample analyses"
di "  $tables/robustness_summary.csv       - Summary of all robustness checks"
di "  $tables/equivalence_tost.csv         - Equivalence testing results"

di _n "Key methodological notes:"
di "  1. All models use weighted OLS with person-clustered robust SEs"
di "  2. Total Effect specification (no income/assets) is preferred"
di "  3. Claiming age categories use half-open intervals to avoid gaps"

log close robust

* ============================================================================
* END OF 06_robustness.do
* ============================================================================
