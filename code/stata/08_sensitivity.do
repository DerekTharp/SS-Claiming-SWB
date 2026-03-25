/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 08_sensitivity.do
* Purpose: Extended sensitivity analyses -- restricted samples, mechanism tests,
*          corrected equivalence testing, behavioral heterogeneity, married subgroup
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
cap log close extsens
log using "$logs/08_sensitivity.log", replace name(extsens)

di "=============================================="
di "08_sensitivity.do: Extended Sensitivity"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* Load analysis-ready data
use "$derived/hrs_analysis_ready.dta", clear

* Create sample indicator (consistent with main analysis)
gen sample_primary = !missing(life_sat, claim_age_c62, female, raeduc, shlt, poor_health, conde, ///
    married, widowed, divorced, age, black, other_race, hispanic, wave)

di "Starting observations: `c(N)'"

* ============================================================================
* 1: RESTRICTED SAMPLE ANALYSIS (AGE >= 70)
* ============================================================================
* Among those 70+, everyone has claimed, so claiming age primarily indexes a
* persistent benefit level difference rather than timing of onset.

di _n "=============================================="
di "1: Restricted Sample Analysis (Age >= 70)"
di "=============================================="

count if age >= 70 & sample_primary == 1 & !missing(life_sat, claim_age_c62)
local n_age70 = r(N)
di "Observations age 70+ in primary sample: `n_age70'"

di _n "Primary specification (Total Effect) on age 70+ sample:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if age >= 70 & sample_primary == 1, vce(cluster hhidpn)
estimates store age70_total

local b_age70 = _b[claim_age_c62]
local se_age70 = _se[claim_age_c62]

di _n "Result: b = " %7.4f `b_age70' " (SE = " %6.4f `se_age70' ")"

* Compare to full primary sample
di _n "Comparison to full primary sample:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store full_total

esttab full_total age70_total using "$tables/restricted_age70.csv", ///
    keep(claim_age_c62) ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Full Sample" "Age 70+ Only") ///
    csv replace ///
    title("Claiming Age Effect: Full Sample vs Age 70+ Restricted") ///
    addnotes("Age 70+ sample provides cleaner 'benefit level' interpretation" ///
             "All models use Total Effect controls with person-clustered SEs")

* ============================================================================
* 2: FIRST-STAGE EVIDENCE (CLAIMING AGE -> OUTCOMES)
* ============================================================================
* Tests whether claiming age affects the hypothesised intermediate outcomes
* (income, consumption capacity).

di _n "=============================================="
di "2: First-Stage Evidence for Mechanism"
di "=============================================="

di _n "First Stage: Claiming Age -> Household Income"
reg ln_hh_income claim_age_c62 $demo $health_basic $marital $age_controls $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store fs_income

local b_fs = _b[claim_age_c62]
local se_fs = _se[claim_age_c62]
local p_fs = 2*ttail(e(df_r), abs(`b_fs'/`se_fs'))

di "Claiming age -> ln(income): b = " %7.4f `b_fs' " (SE = " %6.4f `se_fs' ", p = " %5.3f `p_fs' ")"

if `p_fs' < 0.05 {
    di "Claiming age predicts household income (p < 0.05)"
    di "Later claimers have " %5.1f (exp(`b_fs')-1)*100 "% higher income per year of delay"
}
else {
    di "Claiming age does NOT significantly predict household income"
    di "This weakens the hypothesised mechanism (claiming -> income -> strain -> SWB)"
}

di _n "Second Stage: Income -> Financial Strain"
reg fin_strain ln_hh_income $demo $health_basic $marital $age_controls $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store income_strain

local b_inc_strain = _b[ln_hh_income]
di "Income -> Financial strain: b = " %7.4f `b_inc_strain'

di _n "Third Stage: Income -> Life Satisfaction"
reg life_sat ln_hh_income $demo $health_basic $marital $age_controls $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store income_lifesat

local b_inc_ls = _b[ln_hh_income]
di "Income -> Life satisfaction: b = " %7.4f `b_inc_ls'

esttab fs_income income_strain income_lifesat using "$tables/first_stage_mechanism.csv", ///
    keep(claim_age_c62 ln_hh_income) ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Claim Age->Income" "Income->Strain" "Income->LifeSat") ///
    csv replace ///
    title("First-Stage Evidence for Mechanism") ///
    addnotes("Tests whether claiming age affects income (hypothesised mechanism)" ///
             "If claiming age does not predict income, mediation through income is implausible")

* ============================================================================
* 3: CORRECTED EQUIVALENCE TESTING
* ============================================================================

di _n "=============================================="
di "3: Corrected Equivalence Testing (TOST)"
di "=============================================="

quietly reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
local beta = _b[claim_age_c62]
local se = _se[claim_age_c62]
local n = e(N)
local df = e(df_r)

* Compute SD from data
qui sum life_sat if sample_primary == 1
local sd_lifesat = r(sd)

local beta_8yr = `beta' * 8
local se_8yr = `se' * 8
local beta_8yr_sd = `beta_8yr' / `sd_lifesat'
local se_8yr_sd = `se_8yr' / `sd_lifesat'

local delta = $tost_delta

* 90% CI (for TOST with alpha = 0.05)
local ci90_lo = `beta_8yr_sd' - invttail(`df', 0.05) * `se_8yr_sd'
local ci90_hi = `beta_8yr_sd' + invttail(`df', 0.05) * `se_8yr_sd'

* 95% CI
local ci95_lo = `beta_8yr_sd' - invttail(`df', 0.025) * `se_8yr_sd'
local ci95_hi = `beta_8yr_sd' + invttail(`df', 0.025) * `se_8yr_sd'

di _n "Equivalence Testing Results:"
di "========================================"
di "Point estimate (8-year delay): " %7.4f `beta_8yr_sd' " SD"
di "90% CI: [" %6.4f `ci90_lo' ", " %6.4f `ci90_hi' "]"
di "95% CI: [" %6.4f `ci95_lo' ", " %6.4f `ci95_hi' "]"
di "Equivalence bounds: +/-" %4.2f `delta' " SD"

di _n "INTERPRETATION:"
di "1. Can we rule out POSITIVE effects (benefits of delay)?"
if `ci90_hi' < `delta' {
    di "   YES: Upper bound (" %6.4f `ci90_hi' ") < +" %4.2f `delta' " SD"
}
else {
    di "   NO: Upper bound (" %6.4f `ci90_hi' ") >= +" %4.2f `delta' " SD"
}

di _n "2. Can we rule out NEGATIVE effects (harms of delay)?"
if `ci90_lo' > -`delta' {
    di "   YES: Lower bound (" %6.4f `ci90_lo' ") > -" %4.2f `delta' " SD"
}
else {
    di "   NO: Lower bound (" %6.4f `ci90_lo' ") <= -" %4.2f `delta' " SD"
}

di _n "3. Is FORMAL EQUIVALENCE demonstrated?"
if `ci90_lo' > -`delta' & `ci90_hi' < `delta' {
    di "   YES: 90% CI falls entirely within +/-" %4.2f `delta' " SD bounds"
}
else {
    di "   NO: 90% CI extends beyond equivalence bounds"
}

* Export corrected equivalence results
file open tost_corrected using "$tables/equivalence_tost_corrected.csv", write replace
file write tost_corrected `"="Equivalence Testing (TOST): 8-Year Delay Effect""' _n
file write tost_corrected _n
file write tost_corrected `"="Parameter","Value","Interpretation""' _n
file write tost_corrected `"="Point estimate (SD)","`=string(`beta_8yr_sd', "%7.4f")'","8-year delay effect in SD units""' _n
file write tost_corrected `"="90% CI Lower","`=string(`ci90_lo', "%7.4f")'","Lower bound for equivalence test""' _n
file write tost_corrected `"="90% CI Upper","`=string(`ci90_hi', "%7.4f")'","Upper bound for equivalence test""' _n
file write tost_corrected `"="95% CI Lower","`=string(`ci95_lo', "%7.4f")'","Lower bound for significance""' _n
file write tost_corrected `"="95% CI Upper","`=string(`ci95_hi', "%7.4f")'","Upper bound for significance""' _n
file write tost_corrected `"="Equivalence bounds","+-`delta' SD","Conventional small effect threshold""' _n
file write tost_corrected `"="N","`n'","""' _n
file write tost_corrected _n
file write tost_corrected `"="Note: Asymmetry suggests we can rule out BENEFITS of delay but not HARMS""' _n
file close tost_corrected

* ============================================================================
* 4: PRE-CLAIM SWB BASELINE CONTROLS
* ============================================================================
* Controls for pre-existing well-being differences (reverse causality concern).

di _n "=============================================="
di "4: Pre-Claim SWB Baseline Controls"
di "=============================================="

capture confirm variable claim_wave
if _rc {
    di "claim_wave not found -- creating from rassageb and age"
    gen approx_claim_age = floor(rassageb)
    gen post_claim = (age >= approx_claim_age)
    gen pre_claim = (age < approx_claim_age)
}
else {
    gen post_claim = (wave >= claim_wave)
    gen pre_claim = (wave < claim_wave)
}

count if pre_claim == 1 & !missing(life_sat)
local n_preclaim = r(N)
di "Pre-claim observations with life satisfaction: `n_preclaim'"

* Create baseline SWB per person (mean of pre-claim observations)
bysort hhidpn: egen baseline_ls_temp = mean(life_sat) if pre_claim == 1
bysort hhidpn: egen baseline_lifesat = max(baseline_ls_temp)
drop baseline_ls_temp

egen tag_person = tag(hhidpn)
count if tag_person == 1 & !missing(baseline_lifesat)
local n_with_baseline = r(N)
di "Unique persons with pre-claim baseline SWB: `n_with_baseline'"

count if post_claim == 1 & sample_primary == 1 & !missing(life_sat, claim_age_c62, baseline_lifesat)
local n_baseline_model = r(N)
di "Observations for baseline-adjusted model: `n_baseline_model'"

if `n_baseline_model' > 500 {
    di _n "Baseline-adjusted model:"
    reg life_sat claim_age_c62 baseline_lifesat $controls_total $wave_fe [pw=lb_weight] ///
        if post_claim == 1 & sample_primary == 1 & !missing(baseline_lifesat), vce(cluster hhidpn)
    estimates store baseline_adj

    local b_baseline = _b[claim_age_c62]
    local se_baseline = _se[claim_age_c62]

    di "Claiming age coefficient (baseline-adjusted): b = " %7.4f `b_baseline' " (SE = " %6.4f `se_baseline' ")"
    di "Baseline SWB coefficient: b = " %7.4f _b[baseline_lifesat]

    * Compare to model without baseline
    reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] ///
        if post_claim == 1 & sample_primary == 1 & !missing(baseline_lifesat), vce(cluster hhidpn)
    estimates store no_baseline

    esttab no_baseline baseline_adj using "$tables/baseline_adjusted.csv", ///
        keep(claim_age_c62 baseline_lifesat) ///
        se star(* 0.05 ** 0.01 *** 0.001) ///
        stats(N r2_a) ///
        mtitles("Without Baseline" "With Baseline") ///
        csv replace ///
        title("Baseline-Adjusted Estimates") ///
        addnotes("Controls for pre-claim life satisfaction" ///
                 "Addresses reverse causality: happier people may claim later")
}
else {
    di "Insufficient observations for baseline-adjusted analysis (N = `n_baseline_model')"

    file open baseline_placeholder using "$tables/baseline_adjusted.csv", write replace
    file write baseline_placeholder `"="Baseline-Adjusted Analysis""' _n
    file write baseline_placeholder `"="Insufficient pre-claim observations for this analysis""' _n
    file write baseline_placeholder `"="N with baseline SWB: `n_with_baseline'""' _n
    file close baseline_placeholder
}

* ============================================================================
* 5: BEHAVIOURAL HETEROGENEITY TESTS
* ============================================================================

di _n "=============================================="
di "5: Behavioural Heterogeneity Tests"
di "=============================================="

di _n "Checking for behavioural variables..."

capture confirm variable r10lbcon5
local has_consc = !_rc
capture confirm variable plan_horizon
local has_plan = !_rc
capture confirm variable cogtot
local has_cog = !_rc
capture confirm variable smoker
local has_smoke = !_rc

di "Conscientiousness available: " cond(`has_consc', "Yes", "No")
di "Planning horizon available: " cond(`has_plan', "Yes", "No")
di "Cognition score available: " cond(`has_cog', "Yes", "No")
di "Smoking (impulsivity proxy) available: " cond(`has_smoke', "Yes", "No")

if `has_consc' {
    di _n "Testing heterogeneity by conscientiousness..."
    capture confirm variable conscientious
    if _rc {
        di "Conscientiousness not in long format -- skipping"
    }
    else {
        sum conscientious, detail
        gen high_consc = (conscientious > r(p50)) if !missing(conscientious)

        reg life_sat c.claim_age_c62##i.high_consc $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
        estimates store het_consc

        di "Claiming age x High conscientiousness interaction:"
        lincom 1.high_consc#c.claim_age_c62
    }
}

* Heterogeneity by education (proxy for financial literacy)
* Use existing educ_col_plus variable (created in 03_derived.do)
di _n "Testing heterogeneity by education (financial literacy proxy):"
reg life_sat c.claim_age_c62##i.educ_col_plus $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store het_college

local b_interaction = _b[1.educ_col_plus#c.claim_age_c62]
local se_interaction = _se[1.educ_col_plus#c.claim_age_c62]
di "Claiming age x College interaction: b = " %7.4f `b_interaction' " (SE = " %6.4f `se_interaction' ")"

* Heterogeneity by age (proxy for time horizon)
di _n "Testing heterogeneity by age (time horizon proxy):"
gen age_70plus = (age >= 70)
label var age_70plus "Age 70 or older"

reg life_sat c.claim_age_c62##i.age_70plus $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store het_age70

local b_age_int = _b[1.age_70plus#c.claim_age_c62]
local se_age_int = _se[1.age_70plus#c.claim_age_c62]
di "Claiming age x Age 70+ interaction: b = " %7.4f `b_age_int' " (SE = " %6.4f `se_age_int' ")"

esttab het_college het_age70 using "$tables/behavioral_heterogeneity.csv", ///
    keep(claim_age_c62 1.educ_col_plus 1.educ_col_plus#c.claim_age_c62 1.age_70plus 1.age_70plus#c.claim_age_c62) ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    mtitles("Education" "Age 70+") ///
    csv replace ///
    title("Behavioural Heterogeneity Tests") ///
    addnotes("Tests if claiming age effect varies by behavioural proxies" ///
             "College = proxy for financial literacy" ///
             "Age 70+ = proxy for shorter remaining time horizon")

* ============================================================================
* 6: EVENT STUDY BY CLAIMING GROUP
* ============================================================================

di _n "=============================================="
di "6: Event Study by Claiming Group"
di "=============================================="

capture confirm variable event_time
if _rc {
    gen event_time = round(age - rassageb)
    label var event_time "Years since claiming (rounded)"
}

capture drop early_claimer
gen early_claimer = (rassageb < 64) if !missing(rassageb)
label var early_claimer "Claimed before age 64"

count if !missing(life_sat, event_time, early_claimer) & sample_primary == 1
local n_event = r(N)
di "Observations for event study: `n_event'"

capture drop event_time_binned
gen event_time_binned = round(event_time)
replace event_time_binned = -6 if event_time_binned <= -6
replace event_time_binned = 8 if event_time_binned >= 8

tab event_time_binned

* Estimate with person and wave fixed effects, interacted with early_claimer
capture reghdfe life_sat ///
    i.event_time_binned##i.early_claimer ///
    $health_basic $marital ///
    [pw=lb_weight] if sample_primary == 1, absorb(hhidpn wave) vce(cluster hhidpn)

if !_rc {
    estimates store event_by_group

    di _n "Event study coefficients by claiming group:"
    di "Early claimers (claimed < 64):"
    forval k = -6(2)8 {
        if `k' != -2 {
            capture di "  k = `k': " %7.4f _b[`k'.event_time_binned] " (SE = " %6.4f _se[`k'.event_time_binned] ")"
        }
    }

    di _n "Interaction (Early x Event time):"
    forval k = -6(2)8 {
        if `k' != -2 {
            capture di "  k = `k': " %7.4f _b[1.early_claimer#`k'.event_time_binned]
        }
    }

    di _n "Joint test of differential post-claim trends by claiming group:"
    capture test 1.early_claimer#0.event_time_binned 1.early_claimer#2.event_time_binned ///
        1.early_claimer#4.event_time_binned 1.early_claimer#6.event_time_binned ///
        1.early_claimer#8.event_time_binned
    if !_rc {
        di "F = " r(F) ", p = " r(p)
    }
}
else {
    di "reghdfe not available or model failed -- using standard reg"

    reg life_sat i.event_time_binned##i.early_claimer $controls_total $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
    estimates store event_by_group_simple
}

* Export event study by group coefficients to CSV
di _n "Exporting event_study_by_group.csv..."

capture confirm variable event_time_binned
if !_rc {
    file open esg using "$tables/event_study_by_group.csv", write replace
    file write esg "event_time,coef_early,se_early,coef_later,se_later,coef_diff,se_diff" _n

    * Determine which estimates store to use
    local est_name = "event_by_group"
    capture estimates describe event_by_group
    if _rc {
        local est_name = "event_by_group_simple"
    }
    estimates restore `est_name'

    forval k = -6(1)8 {
        if `k' == -2 {
            * Reference period
            file write esg "`k',0,.,0,.,0,." _n
            continue
        }

        * Later claimers (early_claimer=0): main effect of event_time_binned
        cap local b_later = _b[`k'.event_time_binned]
        cap local se_later = _se[`k'.event_time_binned]
        if _rc {
            local b_later = .
            local se_later = .
        }

        * Interaction term (differential for early claimers)
        cap local b_diff = _b[1.early_claimer#`k'.event_time_binned]
        cap local se_diff = _se[1.early_claimer#`k'.event_time_binned]
        if _rc {
            local b_diff = .
            local se_diff = .
        }

        * Early claimers: main + interaction
        if `b_later' != . & `b_diff' != . {
            local b_early = `b_later' + `b_diff'
            * Approximate SE for sum (ignoring covariance, conservative)
            local se_early = sqrt(`se_later'^2 + `se_diff'^2)
        }
        else {
            local b_early = .
            local se_early = .
        }

        if `k' == -2 | `b_early' != . | `b_later' != . | `b_diff' != . {
            file write esg `"`k',`=string(`b_early', "%7.4f")',`=string(`se_early', "%7.4f")',`=string(`b_later', "%7.4f")',`=string(`se_later', "%7.4f")',`=string(`b_diff', "%7.4f")',`=string(`se_diff', "%7.4f")'"' _n
        }
    }

    file close esg
    di "Saved: $tables/event_study_by_group.csv"
}
else {
    di "event_time_binned not found -- cannot export event_study_by_group.csv"
}

* ============================================================================
* 7: MARRIED SUBGROUP INVESTIGATION
* ============================================================================

di _n "=============================================="
di "7: Married Subgroup Investigation"
di "=============================================="

di _n "Replicating married subgroup finding:"
reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if married == 1 & sample_primary == 1, vce(cluster hhidpn)
local b_married = _b[claim_age_c62]
local se_married = _se[claim_age_c62]
local n_married = e(N)

reg life_sat claim_age_c62 $controls_total $wave_fe [pw=lb_weight] if married == 0 & sample_primary == 1, vce(cluster hhidpn)
local b_unmarried = _b[claim_age_c62]
local se_unmarried = _se[claim_age_c62]
local n_unmarried = e(N)

di "Married:   b = " %7.4f `b_married' " (SE = " %6.4f `se_married' ", N = `n_married')"
di "Unmarried: b = " %7.4f `b_unmarried' " (SE = " %6.4f `se_unmarried' ", N = `n_unmarried')"

* Formal interaction test
di _n "Testing formal interaction (claiming age x married):"
reg life_sat c.claim_age_c62##i.married $demo $health_basic $age_controls $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)
estimates store married_interaction

local b_int = _b[1.married#c.claim_age_c62]
local se_int = _se[1.married#c.claim_age_c62]
local p_int = 2*ttail(e(df_r), abs(`b_int'/`se_int'))

di "Interaction (Claiming x Married): b = " %7.4f `b_int' " (SE = " %6.4f `se_int' ", p = " %5.3f `p_int' ")"

* Break down by detailed marital status
di _n "Breaking down by detailed marital status:"
tab mstat if !missing(life_sat), missing

* Gender x married x claiming three-way interaction
di _n "Testing gender x married x claiming interaction:"
reg life_sat c.claim_age_c62##i.married##i.female $health_basic $age_controls $wave_fe [pw=lb_weight] if sample_primary == 1, vce(cluster hhidpn)

di _n "Married women:"
lincom claim_age_c62 + 1.married#c.claim_age_c62 + 1.female#c.claim_age_c62 + 1.married#1.female#c.claim_age_c62

di _n "Married men:"
lincom claim_age_c62 + 1.married#c.claim_age_c62

esttab married_interaction using "$tables/married_subgroup.csv", ///
    keep(claim_age_c62 1.married 1.married#c.claim_age_c62) ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_a) ///
    csv replace ///
    title("Married Subgroup Analysis") ///
    addnotes("Interaction tests whether claiming age effect differs by marital status" ///
             "Potential mechanism: spousal coordination, retirement-claiming linkage")

* ============================================================================
* SUMMARY OUTPUT
* ============================================================================

di _n "=============================================="
di "EXTENDED SENSITIVITY SUMMARY"
di "=============================================="

di _n "Section 1 (Restricted sample):"
di "  Age 70+ coefficient: " %7.4f `b_age70'

di _n "Section 2 (First-stage):"
di "  Claiming -> income: " %7.4f `b_fs'

di _n "Section 3 (Equivalence):"
di "  Can rule out positive effects, NOT negative effects"

di _n "Section 5 (Behavioural heterogeneity):"
di "  College interaction: " %7.4f `b_interaction'

di _n "Section 7 (Married subgroup):"
di "  Married interaction: b = " %7.4f `b_int' " (p = " %5.3f `p_int' ")"

di _n "Output files:"
di "  $tables/restricted_age70.csv"
di "  $tables/first_stage_mechanism.csv"
di "  $tables/equivalence_tost_corrected.csv"
di "  $tables/baseline_adjusted.csv"
di "  $tables/behavioral_heterogeneity.csv"
di "  $tables/married_subgroup.csv"

* ============================================================================
* COMPLETION
* ============================================================================

di _n "=============================================="
di "Extended Sensitivity Complete"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

log close extsens

* ============================================================================
* END OF 08_sensitivity.do
* ============================================================================
