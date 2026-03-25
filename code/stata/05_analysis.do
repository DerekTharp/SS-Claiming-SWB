/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 05_analysis.do
* Purpose: All regression analyses -- selection decomposition, main results,
*          mediation, heterogeneity, alternative outcomes, robustness,
*          MDES calibration, CRE, Oster bounds, domain satisfaction, event study
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
cap log close analysis
log using "$logs/05_analysis.log", replace name(analysis)

di "=============================================="
di "05_analysis.do: Full Analysis"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* Load analysis-ready data
use "$derived/hrs_analysis_ready.dta", clear

* NOTE: Cronbach's alpha for the 5-item SWLS was computed separately from HRS
* Leave-Behind raw item data across all nine waves (range: 0.87 to 0.89).
* Individual items are not available in the RAND harmonized file.

* ============================================================================
* SAMPLE FLOW DOCUMENTATION
* ============================================================================

di _n "=============================================="
di "SAMPLE FLOW DOCUMENTATION"
di "=============================================="

* Full analysis sample (post all restrictions from 02_sample.do)
count
local n_full = r(N)
distinct hhidpn
local n_unique = r(ndistinct)

di _n "FINAL ANALYTIC SAMPLE:"
di "  Total person-wave observations: `n_full'"
di "  Unique respondents: `n_unique'"

* Document sample by key missingness patterns
di _n "SAMPLE AVAILABILITY BY SPECIFICATION:"

* Model 1: Primary (no liv75)
count if !missing(life_sat, claim_age_c62, female, raeduc, shlt, married)
local n_primary = r(N)
distinct hhidpn if !missing(life_sat, claim_age_c62, female, raeduc, shlt, married)
local n_primary_unique = r(ndistinct)
di "  Primary model (excludes liv75): N = `n_primary' (n = `n_primary_unique' unique)"

* Model 2: With survival expectations
count if !missing(life_sat, claim_age_c62, female, raeduc, shlt, married, liv75)
local n_liv75 = r(N)
distinct hhidpn if !missing(life_sat, claim_age_c62, female, raeduc, shlt, married, liv75)
local n_liv75_unique = r(ndistinct)
di "  With liv75 (sensitivity): N = `n_liv75' (n = `n_liv75_unique' unique)"

* Model 3: With income/assets (Direct Effect)
count if !missing(life_sat, claim_age_c62, female, raeduc, shlt, married, ln_hh_income, ihs_hh_assets)
local n_direct = r(N)
di "  Direct Effect model: N = `n_direct'"

* Constant sample (all controls including liv75)
count if !missing(life_sat, claim_age_c62, female, raeduc, shlt, poor_health, conde, ///
    liv75, ln_hh_income, ihs_hh_assets, married, widowed, divorced, age)
local n_constant = r(N)
distinct hhidpn if !missing(life_sat, claim_age_c62, female, raeduc, shlt, poor_health, conde, ///
    liv75, ln_hh_income, ihs_hh_assets, married, widowed, divorced, age)
local n_constant_unique = r(ndistinct)
di "  Constant sample (all controls): N = `n_constant' (n = `n_constant_unique' unique)"

* Mediation sample
count if !missing(life_sat, claim_age_c62, fin_strain, female, raeduc, shlt, married)
local n_mediation = r(N)
di "  Mediation (with fin_strain): N = `n_mediation'"

* Document why samples differ
di _n "KEY DRIVER OF SAMPLE REDUCTION:"
count if missing(liv75)
local n_missing_liv75 = r(N)
local pct_missing_liv75 = round(`n_missing_liv75' / `n_full' * 100, 0.1)
di "  liv75 missing: `n_missing_liv75' observations (`pct_missing_liv75'%)"
di "  Note: liv75 has age-based skip patterns in HRS survey design"

* Store for use in output
scalar n_full_sample = `n_full'
scalar n_unique_persons = `n_unique'
scalar n_primary_sample = `n_primary'
scalar n_liv75_sample = `n_liv75'
scalar n_constant_sample = `n_constant'

* Create sample indicators
gen sample_primary = !missing(life_sat, claim_age_c62, female, raeduc, shlt, poor_health, conde, ///
    married, widowed, divorced, age, black, other_race, hispanic, wave)

gen sample_liv75 = sample_primary & !missing(liv75)

* True intersection of ALL variables in ALL models (including economic)
gen sample_constant = sample_primary & !missing(liv75, ln_hh_income, ihs_hh_assets)

label var sample_primary "Non-missing for primary specification (all controls)"
label var sample_liv75 "Non-missing including liv75"
label var sample_constant "Non-missing for all controls (true constant sample)"

* Compare primary vs restricted samples
di _n "=============================================="
di "SAMPLE COMPARISON: Primary vs liv75-Restricted"
di "=============================================="

di _n "Comparing characteristics to assess selection:"
di "                            Primary    liv75-Restricted"
di "                            (N=`n_primary')      (N=`n_liv75')"

foreach var in female age claim_age_c62 life_sat shlt {
    qui sum `var' if sample_primary == 1
    local mean1 = r(mean)
    qui sum `var' if sample_liv75 == 1
    local mean2 = r(mean)
    di "  `var'" _col(30) %6.2f `mean1' _col(45) %6.2f `mean2'
}

* Compute life satisfaction SD for later use
qui sum life_sat if sample_primary == 1
local sd_lifesat = r(sd)
di _n "Life satisfaction SD: " %6.3f `sd_lifesat'

* ============================================================================
* SECTION 1: SELECTION DECOMPOSITION (Table 2 in manuscript)
* ============================================================================

di _n "=============================================="
di "SECTION 1: SELECTION DECOMPOSITION"
di "=============================================="

di "Showing sequential attenuation of claiming age coefficient"
di "as control blocks are added one at a time."

* Model 1: Raw (no controls)
di _n "Model 1: Unadjusted"
reg life_sat claim_age_c62 [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local b1 = _b[claim_age_c62]
local se1 = _se[claim_age_c62]
local r2_1 = e(r2)
local n1 = e(N)
estimates store decomp_raw

* Model 2: + Demographics + wave FE
di _n "Model 2: + Demographics + wave FE"
reg life_sat claim_age_c62 $demo $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local b2 = _b[claim_age_c62]
local se2 = _se[claim_age_c62]
local r2_2 = e(r2)
local n2 = e(N)
estimates store decomp_demo

* Model 3: + Health only (no demographics)
di _n "Model 3: + Health only"
reg life_sat claim_age_c62 $health [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local b3 = _b[claim_age_c62]
local se3 = _se[claim_age_c62]
local r2_3 = e(r2)
local n3 = e(N)
estimates store decomp_health

* Model 4: + Demographics + Health + wave FE
di _n "Model 4: + Demographics + Health + wave FE"
reg life_sat claim_age_c62 $demo $health $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local b4 = _b[claim_age_c62]
local se4 = _se[claim_age_c62]
local r2_4 = e(r2)
local n4 = e(N)
estimates store decomp_demo_health

* Model 5: Full primary specification (Total Effect)
di _n "Model 5: Full primary specification"
reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local b5 = _b[claim_age_c62]
local se5 = _se[claim_age_c62]
local r2_5 = e(r2)
local n5 = e(N)
estimates store decomp_primary

* Compute percentage of raw coefficient absorbed by each block
di _n "SELECTION DECOMPOSITION:"
di "=============================================="
di "                          Coeff     SE     %Absorbed  R-squared"
di "  (1) Raw              " %7.4f `b1' "  " %6.4f `se1' "     --       " %6.4f `r2_1'
di "  (2) + Demo+WaveFE    " %7.4f `b2' "  " %6.4f `se2' "  " %5.1f (1 - `b2'/`b1') * 100 "%     " %6.4f `r2_2'
di "  (3) + Health only    " %7.4f `b3' "  " %6.4f `se3' "  " %5.1f (1 - `b3'/`b1') * 100 "%     " %6.4f `r2_3'
di "  (4) + Demo+Health    " %7.4f `b4' "  " %6.4f `se4' "  " %5.1f (1 - `b4'/`b1') * 100 "%     " %6.4f `r2_4'
di "  (5) Full primary     " %7.4f `b5' "  " %6.4f `se5' "  " %5.1f (1 - `b5'/`b1') * 100 "%     " %6.4f `r2_5'

di _n "Marginal attenuation from each block:"
di "  Demographics + wave FE: " %5.1f (`b1' - `b2') / `b1' * 100 "% of raw"
di "  Health only: " %5.1f (`b1' - `b3') / `b1' * 100 "% of raw"
di "  Adding health to demo: " %5.1f (`b2' - `b4') / `b1' * 100 "% of raw (beyond demo)"
di "  Adding marital+age to demo+health: " %5.1f (`b4' - `b5') / `b1' * 100 "% of raw (beyond demo+health)"

* Export esttab version
esttab decomp_raw decomp_demo decomp_health decomp_demo_health decomp_primary ///
    using "$tables/selection_decomposition.csv", ///
    keep(claim_age_c62) ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Raw" "+ Demo" "+ Health" "+ Demo+Health" "Primary") ///
    csv replace ///
    title("Selection Decomposition: Sequential Attenuation of Claiming Age Coefficient")

* Detailed CSV
file open decomp using "$tables/selection_decomposition_detailed.csv", write replace
file write decomp "Model,Controls,Coefficient,SE,Pct_Raw_Absorbed,R2,N" _n
file write decomp `"1,None (raw),`=string(`b1', "%7.4f")',`=string(`se1', "%7.4f")',0.0,`=string(`r2_1', "%6.4f")',`n1'"' _n
file write decomp `"2,Demographics + wave FE,`=string(`b2', "%7.4f")',`=string(`se2', "%7.4f")',`=string((1-`b2'/`b1')*100, "%5.1f")',`=string(`r2_2', "%6.4f")',`n2'"' _n
file write decomp `"3,Health only,`=string(`b3', "%7.4f")',`=string(`se3', "%7.4f")',`=string((1-`b3'/`b1')*100, "%5.1f")',`=string(`r2_3', "%6.4f")',`n3'"' _n
file write decomp `"4,Demographics + Health + wave FE,`=string(`b4', "%7.4f")',`=string(`se4', "%7.4f")',`=string((1-`b4'/`b1')*100, "%5.1f")',`=string(`r2_4', "%6.4f")',`n4'"' _n
file write decomp `"5,Full primary (Total Effect),`=string(`b5', "%7.4f")',`=string(`se5', "%7.4f")',`=string((1-`b5'/`b1')*100, "%5.1f")',`=string(`r2_5', "%6.4f")',`n5'"' _n
file close decomp

di _n "Saved: $tables/selection_decomposition.csv"
di "Saved: $tables/selection_decomposition_detailed.csv"

* ============================================================================
* SECTION 2: MAIN RESULTS - CONTINUOUS CLAIMING AGE (Panels A, B, C)
* ============================================================================

di _n "=============================================="
di "SECTION 2: Main Results (Continuous Claiming Age)"
di "=============================================="

* -----------------------------------------------------------------------------
* Panel A: Primary Specification (excludes liv75)
* -----------------------------------------------------------------------------

di _n "PANEL A: PRIMARY SPECIFICATION (N ~ `n_primary')"
di "Note: Excludes liv75 to preserve sample size"

* Model 1: Unadjusted
di _n "Model 1: Unadjusted"
reg life_sat claim_age_c62 [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store t2_unadj

* Model 2: + Demographics
di _n "Model 2: + Demographics"
reg life_sat claim_age_c62 $demo $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store t2_demo

* Model 3: + Health only (no demographics, matches selection decomposition)
di _n "Model 3: + Health only"
reg life_sat claim_age_c62 $health [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store t2_health

* Model 4: Primary total effect (no income/assets, no liv75)
di _n "Model 4: Primary specification (Total Effect)"
reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store t2_primary

scalar b_primary = _b[claim_age_c62]
scalar se_primary = _se[claim_age_c62]
scalar n_primary_actual = e(N)

* Model 5: Direct effect (adds income/assets)
di _n "Model 5: Direct effect (includes income/assets)"
reg life_sat claim_age_c62 $controls_direct $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store t2_direct

* Export Panel A
esttab t2_unadj t2_demo t2_health t2_primary t2_direct using "$tables/table2_panel_a_primary.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Unadjusted" "+ Demographics" "+ Health Only" "Total Effect" "Direct Effect") ///
    csv replace ///
    title("Table 2 Panel A: Primary Specification (Excludes Survival Expectations)") ///
    addnotes("Primary specification to maximize sample size" ///
             "Total Effect excludes income/assets to avoid 'bad controls'")

* -----------------------------------------------------------------------------
* Panel B: Sensitivity with Survival Expectations (Restricted Sample)
* All models on sample_constant for true comparability
* -----------------------------------------------------------------------------

di _n "PANEL B: SENSITIVITY - WITH SURVIVAL EXPECTATIONS"
di "Note: All models on constant sample for fair comparison"

count if sample_constant == 1
local n_constant_actual = r(N)
di "Constant sample N: `n_constant_actual'"

* Model 1: Unadjusted (on constant sample for comparability)
di _n "Model 1: Unadjusted (constant sample)"
reg life_sat claim_age_c62 [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
estimates store t2b_unadj

* Model 2: Total effect with liv75
di _n "Model 2: Total effect with liv75"
reg life_sat claim_age_c62 $controls_with_liv75 $wave_fe [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
estimates store t2b_total_liv75

* Model 3: Full model (all controls)
di _n "Model 3: Full model (all controls)"
reg life_sat claim_age_c62 $controls_full $wave_fe [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
estimates store t2b_full

scalar b_liv75 = _b[claim_age_c62]
scalar se_liv75 = _se[claim_age_c62]
scalar n_liv75_actual = e(N)

* Export Panel B
esttab t2b_unadj t2b_total_liv75 t2b_full using "$tables/table2_panel_b_sensitivity.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Unadjusted" "Total+liv75" "Full Model") ///
    csv replace ///
    title("Table 2 Panel B: Sensitivity Analysis (With Survival Expectations)") ///
    addnotes("All models on constant sample (N=`n_constant_actual') for comparability" ///
             "Results should be interpreted with caution due to small N")

* -----------------------------------------------------------------------------
* Panel C: Constant Sample Analysis
* All models on identical sample to isolate control effects from composition
* -----------------------------------------------------------------------------

count if sample_constant == 1
local n_constant_actual = r(N)
di _n "PANEL C: CONSTANT SAMPLE (N = `n_constant_actual')"
di "All models on identical sample to isolate effect of controls from composition"

reg life_sat claim_age_c62 [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
local n1 = e(N)
estimates store cs_unadj

reg life_sat claim_age_c62 $demo $wave_fe [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
local n2 = e(N)
estimates store cs_demo

reg life_sat claim_age_c62 $demo $health $wave_fe [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
local n3 = e(N)
estimates store cs_health

reg life_sat claim_age_c62 $controls_with_liv75 $wave_fe [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
local n4 = e(N)
estimates store cs_liv75

reg life_sat claim_age_c62 $controls_full $wave_fe [pw=lb_weight] if sample_constant == 1, vce(cluster hhidpn)
local n5 = e(N)
estimates store cs_full

* Verify all N are identical
di _n "PANEL C N VERIFICATION:"
di "  Col 1 (Unadj): `n1'"
di "  Col 2 (+Demo): `n2'"
di "  Col 3 (+Health): `n3'"
di "  Col 4 (+liv75): `n4'"
di "  Col 5 (Full): `n5'"
assert `n1' == `n2' & `n2' == `n3' & `n3' == `n4' & `n4' == `n5'
di "  All N identical -- constant sample verified"

esttab cs_unadj cs_demo cs_health cs_liv75 cs_full using "$tables/table2_panel_c_constant.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Unadjusted" "+ Demo" "+ Health" "+ liv75" "Full") ///
    csv replace ///
    title("Table 2 Panel C: Constant Sample Analysis") ///
    addnotes("All models estimated on identical N=`n1' observations" ///
             "Isolates effect of adding controls from sample composition")

* Combined table for manuscript
esttab t2_primary t2_direct t2b_total_liv75 t2b_full using "$tables/table2_combined.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Total Effect" "Direct Effect" "Total+liv75" "Full Model") ///
    csv replace ///
    title("Table 2: Main Results - Claiming Age and Life Satisfaction") ///
    addnotes("Columns 1-2: Primary specification (N=`=n_primary_actual'), excludes liv75" ///
             "Columns 3-4: Sensitivity with survival expectations (N=`=n_liv75_actual')")

* Categorical claiming age
di _n "=============================================="
di "CATEGORICAL: Claiming Age Categories"
di "=============================================="

di _n "Categorical: Unadjusted"
reg life_sat i.claim_cat3 [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
testparm i.claim_cat3
estimates store cat_unadj

di _n "Categorical: Primary (Total Effect)"
reg life_sat i.claim_cat3 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
testparm i.claim_cat3
estimates store cat_primary

esttab cat_unadj cat_primary using "$tables/table2_categorical_revised.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Unadjusted" "Total Effect") ///
    csv replace ///
    title("Categorical Claiming Age (Primary Specification)")

* ============================================================================
* SECTION 3: MDES CALIBRATION
* ============================================================================

di _n "=============================================="
di "SECTION 3: MDES CALIBRATION"
di "=============================================="

* Income-SWB gradient in our data
di _n "Income-life satisfaction gradient:"
reg life_sat ln_hh_income $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)

local b_income = _b[ln_hh_income]
local se_income = _se[ln_hh_income]
local p_income = 2 * ttail(e(df_r), abs(`b_income' / `se_income'))

di _n "Income coefficient: " %7.4f `b_income' " (SE = " %6.4f `se_income' ", p = " %5.4f `p_income' ")"

* First-stage: claiming age -> income
di _n "First-stage: claiming age -> ln(income):"
reg ln_hh_income claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)

local b_fs_income = _b[claim_age_c62]
local se_fs_income = _se[claim_age_c62]
local p_fs_income = 2 * ttail(e(df_r), abs(`b_fs_income' / `se_fs_income'))

di "First-stage coefficient: " %7.4f `b_fs_income' " (SE = " %6.4f `se_fs_income' ", p = " %5.4f `p_fs_income' ")"
di "Each year of delay associated with " %5.1f (exp(`b_fs_income') - 1) * 100 "% change in income"

* Compute expected effect of 8-year delay
local income_effect_8yr = `b_fs_income' * 8
di _n "8-year delay income effect (log points): " %7.4f `income_effect_8yr'

local expected_swb_effect = `b_income' * `income_effect_8yr'
local expected_swb_sd = `expected_swb_effect' / `sd_lifesat'

di "Expected SWB effect of 8-year delay (via income): " %7.4f `expected_swb_effect' " points (" %7.4f `expected_swb_sd' " SD)"

* MDES from main specification
quietly reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local se_claiming = _se[claim_age_c62]
local b_claiming = _b[claim_age_c62]

local mdes_per_year = 2.8 * `se_claiming'
local mdes_8yr = `mdes_per_year' * 8
local mdes_8yr_sd = `mdes_8yr' / `sd_lifesat'

di _n "MDES at 80% power (per year): " %7.4f `mdes_per_year' " points"
di "MDES at 80% power (8-year delay): " %7.4f `mdes_8yr' " points (" %7.4f `mdes_8yr_sd' " SD)"

* Ratio: expected effect relative to MDES
local ratio = abs(`expected_swb_effect') / `mdes_8yr'

di _n "SUMMARY:"
di "  Expected SWB effect (8-year delay, via income): " %7.4f `expected_swb_effect' " points"
di "  MDES (8-year delay, 80% power): " %7.4f `mdes_8yr' " points"
di "  Ratio (expected / MDES): " %5.2f `ratio'
if `ratio' < 1 {
    di "  Expected effect is SMALLER than MDES -- study underpowered for this channel"
}
else {
    di "  Expected effect is LARGER than MDES -- study powered to detect this channel"
}

* Export
file open mdes using "$tables/mdes_calibration.csv", write replace
file write mdes "Parameter,Value" _n
file write mdes `"Income-SWB gradient (b),`=string(`b_income', "%7.4f")'"' _n
file write mdes `"Income-SWB gradient (SE),`=string(`se_income', "%7.4f")'"' _n
file write mdes `"Income-SWB gradient (p),`=string(`p_income', "%7.4f")'"' _n
file write mdes `"First-stage: claim_age -> ln_income (b),`=string(`b_fs_income', "%7.4f")'"' _n
file write mdes `"First-stage: claim_age -> ln_income (SE),`=string(`se_fs_income', "%7.4f")'"' _n
file write mdes `"First-stage: claim_age -> ln_income (p),`=string(`p_fs_income', "%7.4f")'"' _n
file write mdes `"8-year delay income effect (log points),`=string(`income_effect_8yr', "%7.4f")'"' _n
file write mdes `"Expected SWB effect of 8-year delay (points),`=string(`expected_swb_effect', "%7.4f")'"' _n
file write mdes `"Expected SWB effect of 8-year delay (SD),`=string(`expected_swb_sd', "%7.4f")'"' _n
file write mdes `"Main spec claiming age (b),`=string(`b_claiming', "%7.4f")'"' _n
file write mdes `"Main spec claiming age (SE),`=string(`se_claiming', "%7.4f")'"' _n
file write mdes `"MDES per year (80% power),`=string(`mdes_per_year', "%7.4f")'"' _n
file write mdes `"MDES 8-year delay (80% power),`=string(`mdes_8yr', "%7.4f")'"' _n
file write mdes `"MDES 8-year delay (SD),`=string(`mdes_8yr_sd', "%7.4f")'"' _n
file write mdes `"Ratio (expected / MDES),`=string(`ratio', "%5.2f")'"' _n
file write mdes `"Life satisfaction SD,`=string(`sd_lifesat', "%6.3f")'"' _n
* Use N from the actual primary regression (sample_primary == 1)
local n_primary_strict = n_primary_actual
file write mdes `"N,`n_primary_strict'"' _n
file close mdes

di _n "Saved: $tables/mdes_calibration.csv"

* ============================================================================
* SECTION 4: MEDIATION AND FIRST-STAGE
* ============================================================================

di _n "=============================================="
di "SECTION 4: Mediation through Financial Strain"
di "=============================================="

count if !missing(life_sat, claim_age_c62, fin_strain) & sample_primary == 1
local n_med = r(N)
di "Mediation sample N: `n_med'"

* Path c (Total effect)
di _n "Total Effect (c):"
reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1 & !missing(fin_strain), vce(cluster hhidpn)
scalar c_total = _b[claim_age_c62]
scalar c_se = _se[claim_age_c62]
estimates store med_total

* Path a (Claiming -> Strain)
di _n "Path a (Claiming -> Financial Strain):"
reg fin_strain claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
scalar a_path = _b[claim_age_c62]
scalar a_se = _se[claim_age_c62]
estimates store med_patha

* Path b and c' (With mediator)
di _n "Paths b and c' (With Financial Strain):"
reg life_sat claim_age_c62 fin_strain $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
scalar b_path = _b[fin_strain]
scalar c_prime = _b[claim_age_c62]
estimates store med_full

* Indirect effect
scalar indirect = a_path * b_path
di _n "MEDIATION SUMMARY:"
di "  Path a (Claim -> Strain): " %7.4f a_path " (SE = " %6.4f a_se ")"
di "  Path b (Strain -> LifeSat): " %7.4f b_path
di "  Total effect (c): " %7.4f c_total " (SE = " %6.4f c_se ")"
di "  Direct effect (c'): " %7.4f c_prime
di "  Indirect effect (a x b): " %7.4f indirect

local indirect_str = string(indirect, "%7.4f")

esttab med_total med_patha med_full using "$tables/table3_mediation_revised.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Total Effect" "Path a (Strain)" "With Mediator") ///
    csv replace ///
    title("Table 3: Mediation Analysis") ///
    addnotes("Path a: Claiming age -> Financial strain" ///
             "Path b: Financial strain -> Life satisfaction" ///
             "Indirect effect (a x b) = `indirect_str' (not significant)")

* --- Bootstrap confidence interval for indirect effect ---
* Uncomment to run bootstrap mediation (requires ~5 min).
* This provides a bootstrap CI for the product-of-coefficients indirect effect.
/*
capture program drop boot_indirect
program boot_indirect, rclass
    reg fin_strain claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1
    local a = _b[claim_age_c62]
    reg life_sat claim_age_c62 fin_strain $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1
    local b = _b[fin_strain]
    return scalar indirect = `a' * `b'
end

bootstrap r(indirect), reps(1000) seed(54321) cluster(hhidpn): boot_indirect
estat bootstrap, percentile
*/

* ============================================================================
* SECTION 5: HETEROGENEITY ANALYSIS
* ============================================================================

di _n "=============================================="
di "SECTION 5: Heterogeneity Analysis"
di "=============================================="

* 5a: Health
di _n "Heterogeneity: Poor Health"
reg life_sat c.claim_age_c62##i.poor_health $demo $health_basic $marital $age_controls $wave_fe ///
    [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store het_health

* 5b: Gender
di _n "Heterogeneity: Gender"
reg life_sat c.claim_age_c62##i.female ///
    i.raeduc black other_race hispanic $health $marital $age_controls $wave_fe ///
    [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store het_gender

* 5c: Education
di _n "Heterogeneity: College Education"
reg life_sat c.claim_age_c62##i.educ_col_plus ///
    female black other_race hispanic $health $marital $age_controls $wave_fe ///
    [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)

scalar educ_interaction = _b[1.educ_col_plus#c.claim_age_c62]
scalar educ_int_se = _se[1.educ_col_plus#c.claim_age_c62]
scalar educ_int_p = 2 * ttail(e(df_r), abs(educ_interaction / educ_int_se))

di _n "EDUCATION INTERACTION:"
di "  Coefficient: " %7.4f educ_interaction
di "  SE: " %7.4f educ_int_se
di "  p-value: " %6.4f educ_int_p
if educ_int_p < 0.05 {
    di "  Significant at p < 0.05"
}

estimates store het_educ

* 5d: Wealth
capture confirm variable low_wealth
if _rc {
    gen low_wealth = (asset_q4 == 1) if !missing(asset_q4)
    label var low_wealth "Bottom wealth quartile"
}

di _n "Heterogeneity: Low Wealth"
reg life_sat c.claim_age_c62##i.low_wealth ///
    $demo $health $marital $age_controls $wave_fe ///
    [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store het_wealth

* 5e: Survival expectations (on liv75 sample only)
di _n "Heterogeneity: Low Survival Expectations (liv75 sample)"
reg life_sat c.claim_age_c62##i.low_surv_exp $demo $health ///
    $marital $age_controls $wave_fe [pw=lb_weight] if sample_liv75 == 1, vce(cluster hhidpn)
estimates store het_surv

* Export
local educ_int_str = string(educ_interaction, "%6.3f")
local educ_p_str = string(educ_int_p, "%5.3f")

esttab het_health het_gender het_educ het_wealth het_surv using "$tables/table4_heterogeneity_revised.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Health" "Gender" "Education" "Low Wealth" "Survival") ///
    csv replace ///
    title("Table 4: Heterogeneity Analysis") ///
    addnotes("Survival expectations model uses restricted sample (N~984)" ///
             "Education interaction: b=`educ_int_str', p=`educ_p_str'")

* ============================================================================
* SECTION 6: ALTERNATIVE LEAVE-BEHIND OUTCOMES
* ============================================================================

di _n "=============================================="
di "SECTION 6: Alternative Well-Being Outcomes"
di "=============================================="

reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store out_lifesat

reg pos_affect claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store out_posaff

reg neg_affect claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store out_negaff

reg loneliness claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store out_lonely

esttab out_lifesat out_posaff out_negaff out_lonely using "$tables/table5_outcomes_revised.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Life Sat" "Pos Affect" "Neg Affect" "Loneliness") ///
    csv replace ///
    title("Table 5: Alternative SWB Outcomes (Primary Specification)")

* ============================================================================
* SECTION 7: CES-D CROSS-SECTIONAL
* ============================================================================

di _n "=============================================="
di "SECTION 7: CES-D (Depressive Symptoms) Outcome"
di "=============================================="

preserve

capture confirm file "$derived/hrs_cesd_analysis_ready.dta"
if !_rc {
    use "$derived/hrs_cesd_analysis_ready.dta", clear

    * Sample indicator: mirrors primary controls but uses CES-D and resp_weight
    gen sample_cesd = !missing(cesd, claim_age_c62, female, raeduc, shlt, poor_health, conde, ///
        married, widowed, divorced, age, wave)

    count if sample_cesd == 1
    local n_cesd = r(N)
    di "CES-D sample (full core HRS, no LB restriction): N = `n_cesd'"

    distinct hhidpn if sample_cesd == 1
    local n_cesd_unique = r(ndistinct)
    di "CES-D unique respondents: `n_cesd_unique'"

    di _n "CES-D descriptive statistics:"
    sum cesd if sample_cesd == 1, detail

    qui sum cesd if sample_cesd == 1
    local sd_cesd = r(sd)
    local mean_cesd = r(mean)
    di "CES-D mean: " %5.3f `mean_cesd' ", SD: " %5.3f `sd_cesd'

    * Unadjusted
    di _n "Model 1: Unadjusted CES-D"
    reg cesd claim_age_c62 [pw=resp_weight] if sample_cesd == 1, vce(cluster hhidpn)

    local b_cesd_unadj = _b[claim_age_c62]
    local se_cesd_unadj = _se[claim_age_c62]
    local p_cesd_unadj = 2 * ttail(e(df_r), abs(`b_cesd_unadj' / `se_cesd_unadj'))
    local n_cesd_unadj = e(N)

    di "Unadjusted: b = " %7.4f `b_cesd_unadj' " (SE = " %6.4f `se_cesd_unadj' ", p = " %5.4f `p_cesd_unadj' ")"

    * Adjusted
    di _n "Model 2: Adjusted CES-D (primary controls)"
    reg cesd claim_age_c62 $controls_primary $wave_fe [pw=resp_weight] if sample_cesd == 1, vce(cluster hhidpn)

    local b_cesd_adj = _b[claim_age_c62]
    local se_cesd_adj = _se[claim_age_c62]
    local p_cesd_adj = 2 * ttail(e(df_r), abs(`b_cesd_adj' / `se_cesd_adj'))
    local n_cesd_adj = e(N)
    local b_cesd_sd = `b_cesd_adj' / `sd_cesd'

    di "Adjusted: b = " %7.4f `b_cesd_adj' " (SE = " %6.4f `se_cesd_adj' ", p = " %5.4f `p_cesd_adj' ")"
    di "Effect in SD units: " %7.4f `b_cesd_sd'
    di "N = `n_cesd_adj' person-waves, `n_cesd_unique' unique respondents"

    * Age 70+ subsample
    di _n "Model 3: CES-D, age 70+ subsample"
    count if sample_cesd == 1 & age >= 70
    local n_70plus = r(N)

    if `n_70plus' > 500 {
        reg cesd claim_age_c62 $controls_primary $wave_fe [pw=resp_weight] if sample_cesd == 1 & age >= 70, vce(cluster hhidpn)

        local b_cesd_70 = _b[claim_age_c62]
        local se_cesd_70 = _se[claim_age_c62]
        local p_cesd_70 = 2 * ttail(e(df_r), abs(`b_cesd_70' / `se_cesd_70'))
        local n_cesd_70 = e(N)

        di "Age 70+: b = " %7.4f `b_cesd_70' " (SE = " %6.4f `se_cesd_70' ", p = " %5.4f `p_cesd_70' ", N = `n_cesd_70')"
    }
    else {
        di "Insufficient age 70+ observations (N = `n_70plus')"
        local b_cesd_70 = .
        local se_cesd_70 = .
        local p_cesd_70 = .
        local n_cesd_70 = `n_70plus'
    }

    * Poisson robustness check for CES-D (count outcome)
    di _n "Model 4: Poisson regression of CES-D"
    capture poisson cesd claim_age_c62 $controls_primary $wave_fe [pw=resp_weight] if sample_cesd == 1, vce(cluster hhidpn)
    if !_rc {
        local b_cesd_poisson = _b[claim_age_c62]
        local se_cesd_poisson = _se[claim_age_c62]
        local p_cesd_poisson = 2 * normal(-abs(`b_cesd_poisson' / `se_cesd_poisson'))
        di "Poisson: b = " %7.4f `b_cesd_poisson' " (SE = " %6.4f `se_cesd_poisson' ", p = " %5.4f `p_cesd_poisson' ")"
    }
    else {
        di "Poisson model did not converge; see OLS results above"
        local b_cesd_poisson = .
        local se_cesd_poisson = .
        local p_cesd_poisson = .
    }

    * Export
    file open cesd_out using "$tables/cesd_results.csv", write replace
    file write cesd_out "Model,Outcome,Coefficient,SE,p-value,SD_units,N,N_unique,Weight,Note" _n
    file write cesd_out `"Unadjusted,CES-D (0-8),`=string(`b_cesd_unadj', "%7.4f")',`=string(`se_cesd_unadj', "%7.4f")',`=string(`p_cesd_unadj', "%7.4f")',,`n_cesd_unadj',`n_cesd_unique',resp_weight,No controls"' _n
    file write cesd_out `"Primary (Total Effect),CES-D (0-8),`=string(`b_cesd_adj', "%7.4f")',`=string(`se_cesd_adj', "%7.4f")',`=string(`p_cesd_adj', "%7.4f")',`=string(`b_cesd_sd', "%7.4f")',`n_cesd_adj',`n_cesd_unique',resp_weight,Demographics + health + marital + age + wave FE"' _n
    file write cesd_out `"Age 70+,CES-D (0-8),`=string(`b_cesd_70', "%7.4f")',`=string(`se_cesd_70', "%7.4f")',`=string(`p_cesd_70', "%7.4f")',,`n_cesd_70',,resp_weight,Age 70+ subsample"' _n
    file write cesd_out `"CES-D mean,`=string(`mean_cesd', "%5.3f")',,,,,,,,"' _n
    file write cesd_out `"CES-D SD,`=string(`sd_cesd', "%5.3f")',,,,,,,,"' _n
    file close cesd_out

    di _n "Saved: $tables/cesd_results.csv"
}
else {
    di "CES-D analysis-ready file not found at $derived/hrs_cesd_analysis_ready.dta"
    di "Run 01_data_prep.do, 02_sample.do, 03_derived.do first to create the CES-D sample."

    file open cesd_out using "$tables/cesd_results.csv", write replace
    file write cesd_out "Parameter,Value" _n
    file write cesd_out "Status,CES-D analysis-ready file not found" _n
    file write cesd_out "Action needed,Re-run pipeline (01 -> 02 -> 03) to create hrs_cesd_analysis_ready.dta" _n
    file close cesd_out

    di "Saved: $tables/cesd_results.csv (noting unavailability)"
}

restore

* ============================================================================
* SECTION 8: DOMAIN-SPECIFIC SATISFACTION
* ============================================================================

di _n "=============================================="
di "SECTION 8: Domain-Specific Satisfaction"
di "=============================================="

* Financial satisfaction (satfin)
capture confirm variable satfin
if !_rc {
    di _n "Financial satisfaction descriptives:"
    sum satfin if sample_primary == 1, detail

    qui sum satfin if sample_primary == 1 & !missing(satfin)
    local mean_satfin = r(mean)
    local sd_satfin = r(sd)
    local n_satfin = r(N)

    di "Financial satisfaction: mean = " %5.3f `mean_satfin' ", SD = " %5.3f `sd_satfin' ", N = `n_satfin'"

    * Unadjusted
    di _n "Financial satisfaction: unadjusted"
    reg satfin claim_age_c62 [pw=lb_weight] if sample_primary == 1 & !missing(satfin), vce(cluster hhidpn)
    local b_satfin_unadj = _b[claim_age_c62]
    local se_satfin_unadj = _se[claim_age_c62]
    local p_satfin_unadj = 2 * ttail(e(df_r), abs(`b_satfin_unadj' / `se_satfin_unadj'))
    local n_satfin_unadj = e(N)

    * Adjusted
    di _n "Financial satisfaction: adjusted"
    reg satfin claim_age_c62 $controls_primary i.wave [pw=lb_weight] if sample_primary == 1 & !missing(satfin), vce(cluster hhidpn)
    local b_satfin_adj = _b[claim_age_c62]
    local se_satfin_adj = _se[claim_age_c62]
    local p_satfin_adj = 2 * ttail(e(df_r), abs(`b_satfin_adj' / `se_satfin_adj'))
    local n_satfin_adj = e(N)
}
else {
    di "Financial satisfaction (satfin) not found in dataset"
    local b_satfin_unadj = .
    local se_satfin_unadj = .
    local p_satfin_unadj = .
    local n_satfin_unadj = .
    local b_satfin_adj = .
    local se_satfin_adj = .
    local p_satfin_adj = .
    local n_satfin_adj = .
    local mean_satfin = .
    local sd_satfin = .
}

* Income satisfaction (satinc)
capture confirm variable satinc
if !_rc {
    di _n "Income satisfaction descriptives:"
    sum satinc if sample_primary == 1, detail

    qui sum satinc if sample_primary == 1 & !missing(satinc)
    local mean_satinc = r(mean)
    local sd_satinc = r(sd)
    local n_satinc = r(N)

    di "Income satisfaction: mean = " %5.3f `mean_satinc' ", SD = " %5.3f `sd_satinc' ", N = `n_satinc'"

    * Unadjusted
    di _n "Income satisfaction: unadjusted"
    reg satinc claim_age_c62 [pw=lb_weight] if sample_primary == 1 & !missing(satinc), vce(cluster hhidpn)
    local b_satinc_unadj = _b[claim_age_c62]
    local se_satinc_unadj = _se[claim_age_c62]
    local p_satinc_unadj = 2 * ttail(e(df_r), abs(`b_satinc_unadj' / `se_satinc_unadj'))
    local n_satinc_unadj = e(N)

    * Adjusted
    di _n "Income satisfaction: adjusted"
    reg satinc claim_age_c62 $controls_primary i.wave [pw=lb_weight] if sample_primary == 1 & !missing(satinc), vce(cluster hhidpn)
    local b_satinc_adj = _b[claim_age_c62]
    local se_satinc_adj = _se[claim_age_c62]
    local p_satinc_adj = 2 * ttail(e(df_r), abs(`b_satinc_adj' / `se_satinc_adj'))
    local n_satinc_adj = e(N)
}
else {
    di "Income satisfaction (satinc) not found in dataset"
    local b_satinc_unadj = .
    local se_satinc_unadj = .
    local p_satinc_unadj = .
    local n_satinc_unadj = .
    local b_satinc_adj = .
    local se_satinc_adj = .
    local p_satinc_adj = .
    local n_satinc_adj = .
    local mean_satinc = .
    local sd_satinc = .
}

* Export domain satisfaction results
file open domsat using "$tables/domain_satisfaction.csv", write replace
file write domsat "Variable,Model,Coefficient,SE,p,N,Mean,SD" _n
file write domsat `"Financial satisfaction,Unadjusted,`=string(`b_satfin_unadj', "%7.4f")',`=string(`se_satfin_unadj', "%7.4f")',`=string(`p_satfin_unadj', "%7.4f")',`n_satfin_unadj',`=string(`mean_satfin', "%5.3f")',`=string(`sd_satfin', "%5.3f")'"' _n
file write domsat `"Financial satisfaction,Adjusted,`=string(`b_satfin_adj', "%7.4f")',`=string(`se_satfin_adj', "%7.4f")',`=string(`p_satfin_adj', "%7.4f")',`n_satfin_adj',`=string(`mean_satfin', "%5.3f")',`=string(`sd_satfin', "%5.3f")'"' _n
file write domsat `"Income satisfaction,Unadjusted,`=string(`b_satinc_unadj', "%7.4f")',`=string(`se_satinc_unadj', "%7.4f")',`=string(`p_satinc_unadj', "%7.4f")',`n_satinc_unadj',`=string(`mean_satinc', "%5.3f")',`=string(`sd_satinc', "%5.3f")'"' _n
file write domsat `"Income satisfaction,Adjusted,`=string(`b_satinc_adj', "%7.4f")',`=string(`se_satinc_adj', "%7.4f")',`=string(`p_satinc_adj', "%7.4f")',`n_satinc_adj',`=string(`mean_satinc', "%5.3f")',`=string(`sd_satinc', "%5.3f")'"' _n
file close domsat

di _n "Saved: $tables/domain_satisfaction.csv"

* ============================================================================
* SECTION 9: CRE AND ORDERED PROBIT
* ============================================================================

di _n "=============================================="
di "SECTION 9: CRE and Ordered Probit"
di "=============================================="

* --- Mundlak-Chamberlain Correlated Random Effects ---

* Compute person-means of time-varying covariates
foreach var of varlist shlt poor_health conde married {
    bysort hhidpn: egen mean_`var' = mean(`var')
    label var mean_`var' "Person-mean: `var'"
}

* xtreg does not support pweights; run unweighted with cluster SEs
xtset hhidpn wave

di _n "Standard RE model (for comparison):"
xtreg life_sat claim_age_c62 $controls_primary $wave_fe if sample_primary == 1, re vce(cluster hhidpn)
estimates store cre_standard_re

local b_re = _b[claim_age_c62]
local se_re = _se[claim_age_c62]

di _n "CRE model (RE + person-means of time-varying covariates):"
xtreg life_sat claim_age_c62 $controls_primary mean_shlt mean_poor_health mean_conde mean_married $wave_fe if sample_primary == 1, re vce(cluster hhidpn)
estimates store cre_mundlak

local b_cre = _b[claim_age_c62]
local se_cre = _se[claim_age_c62]
local n_cre = e(N)
local n_groups = e(N_g)

di _n "COMPARISON:"
di "  Standard RE: b = " %7.4f `b_re' " (SE = " %6.4f `se_re' ")"
di "  CRE (Mundlak): b = " %7.4f `b_cre' " (SE = " %6.4f `se_cre' ")"
di "  Attenuation: " %5.1f (1 - `b_cre' / `b_re') * 100 "%"
di "  N = `n_cre' observations, `n_groups' persons"

* Hausman-like test: joint significance of person-means
test mean_shlt mean_poor_health mean_conde mean_married
local hausman_F = r(F)
local hausman_p = r(p)

di _n "Hausman-like test (joint significance of person-means):"
di "  F = " %6.2f `hausman_F' ", p = " %6.4f `hausman_p'
if `hausman_p' < 0.05 {
    di "  Person-means jointly significant -- RE assumptions may be violated"
}
else {
    di "  Person-means not jointly significant -- RE assumptions supported"
}

* Export CRE
* Use primary regression N for consistency
local n_cre_strict = n_primary_actual
file open cre using "$tables/cre_results.csv", write replace
file write cre "Model,Coefficient,SE,N,N_groups,Note" _n
file write cre `"Standard RE,`=string(`b_re', "%7.4f")',`=string(`se_re', "%7.4f")',`n_cre_strict',`n_groups',Unweighted with cluster SE"' _n
file write cre `"CRE (Mundlak),`=string(`b_cre', "%7.4f")',`=string(`se_cre', "%7.4f")',`n_cre_strict',`n_groups',Adds person-means of time-varying covariates"' _n
file write cre `"Hausman test F,`=string(`hausman_F', "%6.2f")',,,,Joint test of person-means"' _n
file write cre `"Hausman test p,`=string(`hausman_p', "%6.4f")',,,,p < 0.05 suggests RE violated"' _n
file write cre `"Note: xtreg does not support pweights; estimates are unweighted with cluster-robust SEs,,,,,"' _n
file close cre

di _n "Saved: $tables/cre_results.csv"

* --- Ordered Probit ---

di _n "Ordered probit model:"

qui sum life_sat if sample_primary == 1
local ls_min = r(min)
local ls_max = r(max)
di "Life satisfaction range: [`ls_min', `ls_max']"
tab life_sat if sample_primary == 1

oprobit life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)

local b_oprobit = _b[claim_age_c62]
local se_oprobit = _se[claim_age_c62]
local p_oprobit = 2 * normal(-abs(`b_oprobit' / `se_oprobit'))
local n_oprobit = e(N)

di _n "ORDERED PROBIT RESULTS:"
di "  Coefficient: " %7.4f `b_oprobit' " (SE = " %6.4f `se_oprobit' ")"
di "  p-value: " %6.4f `p_oprobit'
di "  N: `n_oprobit'"

estimates store oprobit_main

* OLS comparison
di _n "OLS comparison (same sample):"
reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)

local b_ols = _b[claim_age_c62]
local se_ols = _se[claim_age_c62]

di _n "COMPARISON:"
di "  OLS: b = " %7.4f `b_ols' " (SE = " %6.4f `se_ols' ")"
di "  Ordered probit: b = " %7.4f `b_oprobit' " (SE = " %6.4f `se_oprobit' ")"

* Export
file open oprob using "$tables/ordered_probit.csv", write replace
file write oprob "Model,Coefficient,SE,p-value,N" _n
file write oprob `"OLS,`=string(`b_ols', "%7.4f")',`=string(`se_ols', "%7.4f")',`=string(2*ttail(e(df_r), abs(`b_ols'/`se_ols')), "%6.4f")',`n_oprobit'"' _n
file write oprob `"Ordered Probit,`=string(`b_oprobit', "%7.4f")',`=string(`se_oprobit', "%7.4f")',`=string(`p_oprobit', "%6.4f")',`n_oprobit'"' _n
file write oprob `"Note: Life satisfaction scale [`ls_min'-`ls_max']. Both models use primary controls + wave FE with cluster SEs.,,,,"' _n
file close oprob

di _n "Saved: $tables/ordered_probit.csv"

* ============================================================================
* SECTION 10: OSTER BOUNDS
* ============================================================================

di _n "=============================================="
di "SECTION 10: OSTER BOUNDS"
di "=============================================="

* Install psacalc if needed
capture which psacalc
if _rc {
    ssc install psacalc, replace
}

* Uncontrolled regression
quietly reg life_sat claim_age_c62 [pw=lb_weight] if sample_primary == 1
local beta_u = _b[claim_age_c62]
local r2_u = e(r2)

* Controlled regression (primary specification)
quietly reg life_sat claim_age_c62 $controls_primary $wave_fe [pw=lb_weight] if sample_primary == 1
local beta_c = _b[claim_age_c62]
local r2_c = e(r2)
local n_oster = e(N)

di "Beta (uncontrolled): " %7.4f `beta_u'
di "R-squared (uncontrolled): " %6.4f `r2_u'
di "Beta (controlled): " %7.4f `beta_c'
di "R-squared (controlled): " %6.4f `r2_c'

local rmax_val = min(1, 1.3 * `r2_c')
di "Rmax (1.3 * R2_controlled, capped at 1): " %6.4f `rmax_val'

* Try psacalc first
local psacalc_worked = 0
capture {
    psacalc beta claim_age_c62, delta(1) rmax(`rmax_val')
    local beta_bounded = r(beta)
    local delta_val = 1
    local psacalc_worked = 1
}

if !`psacalc_worked' {
    di "psacalc failed -- computing bounds manually..."
    local delta_val = 1
    local adjustment = `delta_val' * (`beta_u' - `beta_c') * (`rmax_val' - `r2_c') / (`r2_c' - `r2_u')
    local beta_bounded = `beta_c' - `adjustment'
}

di _n "OSTER BOUNDS RESULTS:"
di "  Beta (uncontrolled): " %7.4f `beta_u'
di "  Beta (controlled): " %7.4f `beta_c'
di "  Beta (bounded, delta=1, Rmax=" %5.3f `rmax_val' "): " %7.4f `beta_bounded'

* Compute delta* (delta at which beta* = 0)
if (`beta_u' != `beta_c') & (`r2_c' != `r2_u') {
    local delta_star = -`beta_c' * (`r2_c' - `r2_u') / ((`beta_u' - `beta_c') * (`rmax_val' - `r2_c'))
    di "  Delta* (value making beta*=0): " %7.4f `delta_star'
}
else {
    local delta_star = .
    di "  Delta* could not be computed (degenerate case)"
}

* Export
file open oster using "$tables/oster_bounds.csv", write replace
file write oster "Parameter,Value" _n
file write oster `"Beta (uncontrolled),`=string(`beta_u', "%7.4f")'"' _n
file write oster `"R2 (uncontrolled),`=string(`r2_u', "%6.4f")'"' _n
file write oster `"Beta (controlled),`=string(`beta_c', "%7.4f")'"' _n
file write oster `"R2 (controlled),`=string(`r2_c', "%6.4f")'"' _n
file write oster `"Rmax,`=string(`rmax_val', "%6.4f")'"' _n
file write oster `"Delta,`=string(`delta_val', "%4.1f")'"' _n
file write oster `"Beta (bounded),`=string(`beta_bounded', "%7.4f")'"' _n
file write oster `"Delta* (beta*=0),`=string(`delta_star', "%7.4f")'"' _n
file write oster `"N,`n_oster'"' _n
file write oster `"Note: Rmax = min(1.0, 1.3 * R2_controlled). Delta=1 assumes proportional selection."' _n
file close oster

di _n "Saved: $tables/oster_bounds.csv"

* ============================================================================
* SECTION 11: CROSS-SECTIONAL ROBUSTNESS
* ============================================================================

di _n "=============================================="
di "SECTION 11: Cross-Sectional Robustness"
di "=============================================="

bysort hhidpn (wave): gen first_obs = (_n == 1)
count if first_obs == 1 & sample_primary == 1
local n_cross = r(N)
di "Cross-sectional sample (first obs per person): `n_cross'"

di _n "Cross-sectional: First observation per person"
reg life_sat claim_age_c62 $controls_primary $wave_fe ///
    [pw=lb_weight] if first_obs == 1 & sample_primary == 1, vce(robust)
estimates store cross_first

di _n "Comparison: Panel vs Cross-sectional"
esttab t2_primary cross_first, ///
    keep(claim_age_c62) ///
    se star(* 0.05 ** 0.01 *** 0.001) stats(N)

esttab t2_primary cross_first using "$tables/robustness_cross_sectional.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Panel (all obs)" "Cross-section (first obs)") ///
    csv replace ///
    title("Cross-Sectional Robustness Check")

* -----------------------------------------------------------------------------
* Retirement robustness: restrict to retired respondents
* -----------------------------------------------------------------------------

di _n "Retirement robustness: restrict to retired == 1"

count if retired == 1 & sample_primary == 1
local n_retired = r(N)
distinct hhidpn if retired == 1 & sample_primary == 1
local n_retired_unique = r(ndistinct)
di "Retired subsample: `n_retired' person-waves, `n_retired_unique' unique"

reg life_sat claim_age_c62 $controls_primary $wave_fe ///
    [pw=lb_weight] if retired == 1 & sample_primary == 1, ///
    vce(cluster hhidpn)
estimates store retired_only

di _n "Comparison: Full sample vs Retired only"
esttab t2_primary retired_only, ///
    keep(claim_age_c62) ///
    se star(* 0.05 ** 0.01 *** 0.001) stats(N)

esttab t2_primary retired_only using "$tables/robustness_retired.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Full sample" "Retired only") ///
    csv replace ///
    title("Retirement Robustness Check")

* ============================================================================
* SECTION 12: CES-D EVENT STUDY
* ============================================================================

di _n "=============================================="
di "SECTION 12: CES-D Event Study"
di "=============================================="

capture confirm file "$derived/hrs_cesd_full_analysis_ready.dta"
if !_rc {
    preserve

    use "$derived/hrs_cesd_full_analysis_ready.dta", clear
    di "CES-D full sample loaded: `c(N)' observations"

    * Construct event time
    cap drop claim_year year_survey event_time
    gen claim_year = rabyear + rassageb
    gen year_survey = 1990 + wave * 2
    gen event_time = year_survey - claim_year

    count if event_time < 0 & !missing(cesd)
    di "Pre-claim CES-D observations: `r(N)'"
    count if event_time >= 0 & !missing(cesd)
    di "Post-claim CES-D observations: `r(N)'"

    * Create event time bins (2-year bins)
    cap drop event_bin event_bin_pos
    gen event_bin = .
    replace event_bin = -6 if event_time <= -6
    replace event_bin = -4 if event_time == -5 | event_time == -4
    replace event_bin = -2 if event_time == -3 | event_time == -2
    replace event_bin = 0  if event_time == -1 | event_time == 0
    replace event_bin = 2  if event_time == 1  | event_time == 2
    replace event_bin = 4  if event_time == 3  | event_time == 4
    replace event_bin = 6  if event_time >= 5
    gen event_bin_pos = event_bin + 8

    * Require both pre and post observations
    cap drop has_pre has_post
    bysort hhidpn: egen has_pre = max(event_bin < 0 & !missing(cesd))
    bysort hhidpn: egen has_post = max(event_bin >= 0 & !missing(cesd))

    count if has_pre == 1 & has_post == 1 & !missing(cesd) & !missing(event_bin_pos)
    di "CES-D event study sample (pre+post required): `r(N)'"

    if r(N) > 100 {
        * Person FE event study
        * Reference period: k=-2 (event_bin_pos=6), matching 07_event_study.do
        xtset hhidpn wave
        xtreg cesd ib6.event_bin_pos i.wave ///
            if has_pre == 1 & has_post == 1 & !missing(cesd), ///
            fe vce(cluster hhidpn)

        local cesd_es_n = e(N)
        local cesd_es_ng = e(N_g)

        di _n "CES-D Event Study: N = `cesd_es_n', persons = `cesd_es_ng'"

        * Extract coefficients
        * event_bin_pos values: 2=-6, 4=-4, 6=-2(ref), 8=0, 10=+2, 12=+4, 14=+6
        foreach k in 2 4 8 10 12 14 {
            cap di "event_bin_pos `k': b = " %6.4f _b[`k'.event_bin_pos] ///
                ", SE = " %6.4f _se[`k'.event_bin_pos]
        }

        * Pre-trend test (k=-6 and k=-4, relative to k=-2 reference)
        test 2.event_bin_pos 4.event_bin_pos
        di "Pre-trend joint F = " %6.3f r(F) ", p = " %6.4f r(p)

        * Post-claiming test (k=0, k=+2, k=+4, k=+6)
        test 8.event_bin_pos 10.event_bin_pos 12.event_bin_pos 14.event_bin_pos
        di "Post-claiming joint F = " %6.3f r(F) ", p = " %6.4f r(p)

        * Export CES-D event study coefficients
        cap esttab using "$tables/cesd_event_study.csv", replace ///
            cells(b(fmt(4)) se(fmt(4) par)) ///
            title("CES-D Event Study") ///
            addnote("Reference: k=-2 (event_bin_pos=6), matching 07_event_study.do" ///
                "Person FE with wave FE, clustered SEs" ///
                "N=`cesd_es_n', persons=`cesd_es_ng'")
    }
    else {
        di "Insufficient CES-D event study observations (N < 100). Skipping."
        file open fh using "$tables/cesd_event_study.csv", write replace
        file write fh "Status,Insufficient pre-claim CES-D observations" _n
        file write fh "N_available,`r(N)'" _n
        file write fh "Note,CES-D event study requires pre-claim observations which are limited" _n
        file close fh
    }

    * ================================================================
    * SECTION 12b: CES-D STRATIFIED EVENT STUDY (Early vs Later Claimers)
    * ================================================================

    di _n "=============================================="
    di "SECTION 12b: CES-D Stratified Event Study"
    di "=============================================="

    * Define early claimer (claimed before 64) for CES-D sample
    cap drop early_claimer_cesd
    gen early_claimer_cesd = (rassageb < 64) if !missing(rassageb)
    label var early_claimer_cesd "Claimed before age 64 (CES-D sample)"

    count if has_pre == 1 & has_post == 1 & !missing(cesd, event_bin_pos, early_claimer_cesd)
    local cesd_strat_n = r(N)
    di "CES-D stratified event study sample: `cesd_strat_n'"

    if `cesd_strat_n' > 100 {
        * Interaction model: event time × early claimer, person FE
        xtset hhidpn wave
        xtreg cesd ib6.event_bin_pos##i.early_claimer_cesd i.wave ///
            if has_pre == 1 & has_post == 1 & !missing(cesd), ///
            fe vce(cluster hhidpn)

        local cesd_strat_n_reg = e(N)
        local cesd_strat_ng = e(N_g)

        di _n "CES-D Stratified Event Study: N = `cesd_strat_n_reg', persons = `cesd_strat_ng'"

        * Display main effects (later claimers = reference group for early_claimer)
        di _n "Later-claimer coefficients (early_claimer=0, reference category):"
        foreach k in 2 4 8 10 12 14 {
            cap di "  event_bin_pos `k': b = " %6.4f _b[`k'.event_bin_pos] ///
                ", SE = " %6.4f _se[`k'.event_bin_pos]
        }

        di _n "Interaction coefficients (early_claimer × event time):"
        foreach k in 2 4 8 10 12 14 {
            cap di "  k=`k' interaction: b = " %6.4f _b[1.early_claimer_cesd#`k'.event_bin_pos] ///
                ", SE = " %6.4f _se[1.early_claimer_cesd#`k'.event_bin_pos]
        }

        * Joint test of post-claiming interaction terms
        capture test 1.early_claimer_cesd#8.event_bin_pos ///
            1.early_claimer_cesd#10.event_bin_pos ///
            1.early_claimer_cesd#12.event_bin_pos ///
            1.early_claimer_cesd#14.event_bin_pos
        if !_rc {
            local strat_F = r(F)
            local strat_p = r(p)
            di _n "Joint test of post-claiming interaction (early × k=0,+2,+4,+6):"
            di "  F = " %6.3f `strat_F' ", p = " %6.4f `strat_p'
        }
        else {
            local strat_F = .
            local strat_p = .
            di "Joint test of post-claiming interaction failed"
        }

        * Pre-trend interaction test
        capture test 1.early_claimer_cesd#2.event_bin_pos ///
            1.early_claimer_cesd#4.event_bin_pos
        if !_rc {
            local strat_pre_F = r(F)
            local strat_pre_p = r(p)
            di _n "Pre-trend interaction test (early × k=-6, k=-4):"
            di "  F = " %6.3f `strat_pre_F' ", p = " %6.4f `strat_pre_p'
        }

        * Export stratified CES-D event study results
        * event_bin_pos: 2=-6, 4=-4, 6=-2(ref), 8=0, 10=+2, 12=+4, 14=+6
        file open cesd_strat using "$tables/cesd_event_study_stratified.csv", write replace
        file write cesd_strat "event_bin_pos,event_time,coef_later,se_later,coef_early_interaction,se_early_interaction,N,N_persons" _n

        foreach k in 2 4 8 10 12 14 {
            local et = `k' - 8
            local b_main = _b[`k'.event_bin_pos]
            local se_main = _se[`k'.event_bin_pos]
            cap local b_int = _b[1.early_claimer_cesd#`k'.event_bin_pos]
            cap local se_int = _se[1.early_claimer_cesd#`k'.event_bin_pos]
            if _rc {
                local b_int = .
                local se_int = .
            }
            file write cesd_strat `"`k',`et',`=string(`b_main', "%7.4f")',`=string(`se_main', "%7.4f")',`=string(`b_int', "%7.4f")',`=string(`se_int', "%7.4f")',`cesd_strat_n_reg',`cesd_strat_ng'"' _n
        }

        * Add reference row
        file write cesd_strat `"6,-2,0.0000,.,0.0000,.,`cesd_strat_n_reg',`cesd_strat_ng'"' _n
        * Add test statistics
        file write cesd_strat `"post_interaction_F,,`=string(`strat_F', "%7.3f")',,,,,"' _n
        file write cesd_strat `"post_interaction_p,,`=string(`strat_p', "%7.4f")',,,,,"' _n
        file close cesd_strat

        di _n "Saved: $tables/cesd_event_study_stratified.csv"
    }
    else {
        di "Insufficient observations for stratified CES-D event study (N < 100)"
        file open cesd_strat using "$tables/cesd_event_study_stratified.csv", write replace
        file write cesd_strat "Status,Insufficient observations for stratified CES-D event study" _n
        file close cesd_strat
    }

    restore
}
else {
    di "CES-D full sample file not found. Run 02_sample.do and 03_derived.do first."
}

* ============================================================================
* SECTION 13: FIGURES
* ============================================================================

di _n "=============================================="
di "SECTION 13: Figures"
di "=============================================="

* Figure 1: Predicted life satisfaction by claiming age category
di "Generating Figure 1: Life satisfaction by claiming category..."
quietly reg life_sat i.claim_cat3 $controls_primary $wave_fe ///
    [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local n_fig1 = e(N)
di "  Figure 1 sample size: `n_fig1'"
margins claim_cat3
marginsplot, ///
    title("Predicted Life Satisfaction by Claiming Age Category") ///
    subtitle("Primary Specification (N = `n_fig1')") ///
    ytitle("Life Satisfaction (1-7 scale)") ///
    xtitle("Claiming Age Category") ///
    ylabel(4.5 5 5.5, grid) ///
    recast(bar) ///
    plotopts(barwidth(0.5)) ///
    note("Controls: demographics, health, marital status, age. Excludes survival expectations.")
graph export "$figures/fig1_lifesat_by_cat.png", replace width(1200)

* Figure 2: Effect of claiming age by health status
di "Generating Figure 2: Effect by health status..."
quietly reg life_sat c.claim_age_c62##i.poor_health $controls_noph $wave_fe ///
    [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local n_fig2 = e(N)
di "  Figure 2 sample size: `n_fig2'"
margins, dydx(claim_age_c62) at(poor_health=(0 1))
marginsplot, ///
    title("Effect of Claiming Age on Life Satisfaction" "by Health Status") ///
    subtitle("Primary Specification (N = `n_fig2')") ///
    ytitle("Effect on Life Satisfaction (per year of delay)") ///
    xtitle("Health Status") ///
    xlabel(0 "Good/Very Good" 1 "Fair/Poor", labsize(medium)) ///
    yline(0, lpattern(dash) lcolor(gray)) ///
    note("Controls: demographics, marital status, age. Excludes survival expectations.") ///
    xsize(8) ysize(5) ///
    graphregion(margin(r+15))
graph export "$figures/fig2_effect_by_health.png", replace width(1600)

di "Figures generated successfully."

* ============================================================================
* POWER ANALYSIS AND EQUIVALENCE TESTING
* ============================================================================

di _n "=============================================="
di "POWER ANALYSIS AND EQUIVALENCE BOUNDS"
di "=============================================="

local b = b_primary
local se = se_primary
local n = n_primary_actual

local ci_lo = `b' - 1.96 * `se'
local ci_hi = `b' + 1.96 * `se'

* Compute SD from data
qui sum life_sat if sample_primary == 1
local sd_lifesat = r(sd)

* Effect size per year
local d_per_year = `b' / `sd_lifesat'
local d_ci_lo = `ci_lo' / `sd_lifesat'
local d_ci_hi = `ci_hi' / `sd_lifesat'

* Effect for 8-year delay (62 -> 70)
local effect_8yr = `b' * 8
local ci_lo_8yr = `ci_lo' * 8
local ci_hi_8yr = `ci_hi' * 8
local d_8yr = `effect_8yr' / `sd_lifesat'
local d_ci_hi_8yr = `ci_hi_8yr' / `sd_lifesat'

di _n "PRIMARY SPECIFICATION RESULTS:"
di "  Sample size: `n'"
di "  Coefficient: " %7.4f `b' " (SE = " %6.4f `se' ")"
di "  95% CI: [" %7.4f `ci_lo' ", " %7.4f `ci_hi' "]"
di "  Life satisfaction SD: " %6.3f `sd_lifesat'

di _n "EFFECT SIZE (Cohen's d):"
di "  Per year of delay: " %7.4f `d_per_year' " SD"
di "  95% CI: [" %7.4f `d_ci_lo' ", " %7.4f `d_ci_hi' "] SD"

di _n "CUMULATIVE EFFECT (62 -> 70, 8-year delay):"
di "  Point estimate: " %7.3f `effect_8yr' " points"
di "  95% CI: [" %7.3f `ci_lo_8yr' ", " %7.3f `ci_hi_8yr' "] points"
di "  In SD units: " %7.3f `d_8yr' " SD (95% CI upper: " %7.3f `d_ci_hi_8yr' " SD)"

di _n "EQUIVALENCE INTERPRETATION:"
di "  We can rule out POSITIVE effects larger than " %5.3f `ci_hi' " points per year"
di "  For 8-year delay, upper bound is " %5.2f `ci_hi_8yr' " points (" %5.3f `d_ci_hi_8yr' " SD)"
di ""
di "  Benchmark: 0.10 SD is conventionally 'small' in SWB research"
di "  Our upper bound (" %5.3f `d_ci_hi_8yr' " SD) " ///
    cond(`d_ci_hi_8yr' < 0.10, "rules out small effects", "includes small effects")

* Minimum Detectable Effect Size
local mdes = 2.8 * `se'
local mdes_d = `mdes' / `sd_lifesat'

di _n "MINIMUM DETECTABLE EFFECT (80% power, alpha=0.05):"
di "  MDES per year: " %7.4f `mdes' " points (" %6.4f `mdes_d' " SD)"
di "  MDES for 8-year delay: " %7.3f `mdes' * 8 " points (" %6.3f `mdes_d' * 8 " SD)"

scalar mdes_value = `mdes'
scalar upper_bound_8yr = `ci_hi_8yr'
scalar upper_bound_8yr_sd = `d_ci_hi_8yr'

* ============================================================================
* SUMMARY OUTPUT FOR MANUSCRIPT
* ============================================================================

di _n "=============================================="
di "SUMMARY FOR MANUSCRIPT"
di "=============================================="

di _n "KEY SAMPLE SIZES:"
di "  Full analytic sample: " n_full_sample " person-waves"
di "  Unique respondents: " n_unique_persons
di "  Primary specification: " n_primary_sample
di "  With liv75 (sensitivity): " n_liv75_sample
di "  Constant sample: " n_constant_sample

di _n "KEY RESULTS (Primary Specification):"
di "  Coefficient per year: " %7.4f b_primary " (SE = " %6.4f se_primary ")"
di "  95% CI: [" %7.4f (b_primary - 1.96*se_primary) ", " %7.4f (b_primary + 1.96*se_primary) "]"

di _n "EDUCATION INTERACTION:"
di "  Coefficient: " %7.4f educ_interaction " (SE = " %6.4f educ_int_se ", p = " %6.4f educ_int_p ")"

di _n "MEDIATION PATH A (Claiming -> Strain):"
di "  Coefficient: " %7.4f a_path " (SE = " %6.4f a_se ")"

di _n "EQUIVALENCE BOUNDS:"
di "  Upper bound for 8-year delay: " %6.3f upper_bound_8yr " points (" %6.3f upper_bound_8yr_sd " SD)"
di "  MDES (80% power): " %6.4f mdes_value " points per year"

* ============================================================================
* COMPLETION
* ============================================================================

di _n "=============================================="
di "Analysis Complete"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

di _n "Output files generated:"
di "  selection_decomposition.csv            - Sequential attenuation"
di "  selection_decomposition_detailed.csv   - Detailed decomposition"
di "  table2_panel_a_primary.csv             - Main results (large sample)"
di "  table2_panel_b_sensitivity.csv         - Sensitivity with liv75"
di "  table2_panel_c_constant.csv            - Constant sample analysis"
di "  table2_combined.csv                    - Combined for manuscript"
di "  table2_categorical_revised.csv         - Categorical claiming age"
di "  mdes_calibration.csv                   - MDES calibration"
di "  table3_mediation_revised.csv           - Mediation analysis"
di "  table4_heterogeneity_revised.csv       - Heterogeneity analysis"
di "  table5_outcomes_revised.csv            - Alternative outcomes"
di "  cesd_results.csv                       - CES-D results"
di "  domain_satisfaction.csv                - Domain-specific satisfaction"
di "  cre_results.csv                        - CRE model"
di "  ordered_probit.csv                     - Ordered probit"
di "  oster_bounds.csv                       - Oster bounds"
di "  robustness_cross_sectional.csv         - Cross-sectional check"
di "  robustness_retired.csv                 - Retirement robustness check"
di "  cesd_event_study.csv                   - CES-D event study"

log close analysis

* ============================================================================
* END OF 05_analysis.do
* ============================================================================
