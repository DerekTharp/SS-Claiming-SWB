/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 07_event_study.do
* Purpose: Event-study analysis around Social Security claiming transition
* Author: Derek Tharp
* Created: January 2026
*
* KEY METHODOLOGICAL NOTES:
*   This analysis examines within-person changes in life satisfaction around
*   the claiming transition using an event-study design with person fixed effects.
*
*   Unlike the cross-sectional analysis in 05_analysis.do, this approach:
*   1. Uses person fixed effects to control for time-invariant confounders
*   2. Traces the trajectory of well-being before AND after claiming
*   3. Tests for differential pre-trends by claiming group
*   4. Avoids the collinearity issue (age = claiming_age + years_since) that
*      plagued the previous "dynamic effects" specification
*
*   IMPORTANT: This analysis requires the sample to include PRE-CLAIM observations,
*   which the main analysis (05_analysis.do) excludes. We reload the data
*   from an earlier stage of processing.
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
cap log close event
log using "$logs/07_event_study.log", replace name(event)

di "=============================================="
di "07_event_study.do: Event-Study Analysis"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* ============================================================================
* PART 1: LOAD DATA INCLUDING PRE-CLAIM OBSERVATIONS
* ============================================================================

di _n "=============================================="
di "Loading data with pre-claim observations"
di "=============================================="

* We need to reload data before the post-claim restriction was applied
* Load from merged data and apply restrictions EXCEPT the post-claim filter
use "$derived/hrs_merged.dta", clear

* Apply same restrictions as 02_sample.do EXCEPT post-claim
* Restriction 1: Birth cohort 1931-1959
drop if rabyear < 1931 | rabyear > 1959 | missing(rabyear)

* Restriction 2: Claimed Social Security (must have valid claiming age)
drop if missing(rassageb)
drop if rassageb < 62 | rassageb > 70

di "After cohort and claiming restrictions: `c(N)'"

* ============================================================================
* PART 2: RESHAPE TO LONG FORMAT (INCLUDING PRE-CLAIM WAVES)
* ============================================================================

di _n "Reshaping to person-wave long format..."

* Fix Wave 8 affect variable names for reshape compatibility
capture confirm variable r8lbposaffect6
if !_rc {
    rename r8lbposaffect6 r8lbposaffect
    rename r8lbnegaffect6 r8lbnegaffect
}

* Keep relevant variables for event study
local vars_to_keep hhidpn rabyear ragender raeduc raracem rahispan rassageb

* Life satisfaction and affects across waves
forvalues w = 8/16 {
    local vars_to_keep `vars_to_keep' r`w'lbsatwlf r`w'lbwgtr
    capture confirm variable r`w'lbposaffect
    if !_rc {
        local vars_to_keep `vars_to_keep' r`w'lbposaffect r`w'lbnegaffect
    }
    * Age at interview
    capture confirm variable r`w'agey_b
    if !_rc {
        local vars_to_keep `vars_to_keep' r`w'agey_b
    }
    * Self-rated health
    capture confirm variable r`w'shlt
    if !_rc {
        local vars_to_keep `vars_to_keep' r`w'shlt
    }
    * Proxy indicator
    capture confirm variable r`w'proxy
    if !_rc {
        local vars_to_keep `vars_to_keep' r`w'proxy
    }
}

keep `vars_to_keep'

* Reshape to long
reshape long r@lbsatwlf r@lbwgtr r@lbposaffect r@lbnegaffect r@agey_b r@shlt r@proxy, ///
    i(hhidpn) j(wave)

* Rename for clarity
rename rlbsatwlf life_sat
rename rlbwgtr lb_weight
rename rlbposaffect pos_affect
rename rlbnegaffect neg_affect
rename ragey_b age
rename rshlt shlt
rename rproxy proxy

* Create year from wave (wave 8 = 2006, ..., wave 16 = 2022)
gen year = 1990 + wave * 2
label var year "Survey year"

* Verify wave-year mapping
di "Wave-year mapping verification:"
di "  Wave 8 -> Year " 1990 + (8 * 2) " (should be 2006)"
di "  Wave 16 -> Year " 1990 + (16 * 2) " (should be 2022)"

di "After reshape: `c(N)' person-wave observations"

* ============================================================================
* PART 3: APPLY REMAINING SAMPLE RESTRICTIONS
* ============================================================================

* Restriction 3: LB eligibility and completion (need non-missing life_sat)
drop if missing(life_sat)

* Restriction 4: Non-proxy
drop if proxy == 1

di "After LB and non-proxy restrictions: `c(N)'"

* ============================================================================
* PART 4: CREATE EVENT TIME VARIABLE
* ============================================================================

di _n "Creating event time variable..."

* Calculate claiming year (approximately: birth year + claiming age)
gen claim_year = rabyear + rassageb
label var claim_year "Year started receiving SS (approximate)"

* Event time: years relative to claiming
* k = 0 is the first interview at or after claiming
* k < 0 is pre-claim, k > 0 is post-claim
gen event_time = year - claim_year
label var event_time "Years since claiming (event time)"

* Summary of event time distribution
tab event_time, missing

* Count pre-claim and post-claim observations
count if event_time < 0
local n_pre = r(N)
count if event_time >= 0
local n_post = r(N)
di _n "Pre-claim observations: `n_pre'"
di "Post-claim observations: `n_post'"

* ============================================================================
* PART 5: CREATE EVENT TIME BINS
* ============================================================================

di _n "Creating event time bins..."

* Bin event time for tractable estimation
* Given LB is biennial, use 2-year bins: -6+, -4, -2, 0, +2, +4, +6+
gen event_bin = .
replace event_bin = -6 if event_time <= -6                  // 6+ years before
replace event_bin = -4 if event_time == -5 | event_time == -4
replace event_bin = -2 if event_time == -3 | event_time == -2  // Reference period
replace event_bin = 0  if event_time == -1 | event_time == 0   // Claiming period
replace event_bin = 2  if event_time == 1 | event_time == 2
replace event_bin = 4  if event_time == 3 | event_time == 4
replace event_bin = 6  if event_time >= 5                   // 5+ years after

label define event_bin_lbl -6 "≤-6 yrs" -4 "-4 yrs" -2 "-2 yrs (ref)" 0 "0 yrs" 2 "+2 yrs" 4 "+4 yrs" 6 "≥+6 yrs"
label values event_bin event_bin_lbl
label var event_bin "Event time bin (2-year intervals)"

tab event_bin, missing

* ============================================================================
* PART 6: CREATE ADDITIONAL VARIABLES
* ============================================================================

* Demographics
gen female = (ragender == 2) if !missing(ragender)

* Education categories
gen educ_col_plus = (raeduc == 5) if !missing(raeduc)

* Claiming age categories (for heterogeneous event study)
gen early_claimer = (rassageb >= 62 & rassageb < 64)
gen delayed_claimer = (rassageb >= 67 & rassageb <= 70)
label var early_claimer "Claimed at 62-63"
label var delayed_claimer "Claimed at 67-70"

* Claiming age centered at 62
gen claim_age_c62 = rassageb - 62
label var claim_age_c62 "Claiming age (centered at 62)"

* Panel setup
xtset hhidpn wave

* Sample indicator for event study (need at least one pre and one post observation)
bysort hhidpn: egen has_pre = max(event_time < 0)
bysort hhidpn: egen has_post = max(event_time >= 0)
gen sample_event = (has_pre == 1 & has_post == 1)
label var sample_event "Has both pre- and post-claim observations"

count if sample_event == 1
local n_event = r(N)
distinct hhidpn if sample_event == 1
local n_persons_event = r(ndistinct)
di _n "Event study sample: `n_event' person-waves from `n_persons_event' respondents"

* ============================================================================
* PART 7: EVENT STUDY ESTIMATION
* ============================================================================

di _n "=============================================="
di "EVENT STUDY ESTIMATION"
di "=============================================="

* Model 1: Basic event study with person and wave FE
* Reference period: event_bin = -2 (two years before claiming)
di _n "Model 1: Basic event study (person FE + wave FE)"
di "Reference period: 2 years before claiming"

* Create event_bin dummies manually (Stata has issues with negative factor bases)
* Recode event_bin to positive values for factor variable syntax
gen event_bin_pos = event_bin + 8  // Now ranges from 2 to 14 instead of -6 to 6
* Reference will be event_bin_pos = 6 (corresponding to event_bin = -2)

xtreg life_sat ib6.event_bin_pos i.wave if sample_event == 1, fe vce(cluster hhidpn)
estimates store event_basic

* Store coefficients for plotting
* event_bin_pos values: 2=-6, 4=-4, 6=-2(ref), 8=0, 10=+2, 12=+4, 14=+6
scalar coef_m6 = _b[2.event_bin_pos]
scalar coef_m4 = _b[4.event_bin_pos]
scalar coef_m2 = 0  // Reference
scalar coef_0 = _b[8.event_bin_pos]
scalar coef_p2 = _b[10.event_bin_pos]
scalar coef_p4 = _b[12.event_bin_pos]
scalar coef_p6 = _b[14.event_bin_pos]

scalar se_m6 = _se[2.event_bin_pos]
scalar se_m4 = _se[4.event_bin_pos]
scalar se_m2 = 0
scalar se_0 = _se[8.event_bin_pos]
scalar se_p2 = _se[10.event_bin_pos]
scalar se_p4 = _se[12.event_bin_pos]
scalar se_p6 = _se[14.event_bin_pos]

* Display event-time coefficients
di _n "Event-time coefficients (relative to k=-2):"
di "  k = -6: " %6.3f coef_m6 " (SE = " %5.3f se_m6 ")"
di "  k = -4: " %6.3f coef_m4 " (SE = " %5.3f se_m4 ")"
di "  k = -2: 0.000 (reference)"
di "  k =  0: " %6.3f coef_0 " (SE = " %5.3f se_0 ")"
di "  k = +2: " %6.3f coef_p2 " (SE = " %5.3f se_p2 ")"
di "  k = +4: " %6.3f coef_p4 " (SE = " %5.3f se_p4 ")"
di "  k = +6: " %6.3f coef_p6 " (SE = " %5.3f se_p6 ")"

* Test for pre-trends
di _n "Pre-trend test (H0: pre-claiming coefficients = 0):"
test 2.event_bin_pos 4.event_bin_pos
local pretrend_p = r(p)
di "  Joint F-test p-value: " %6.4f `pretrend_p'
if `pretrend_p' < 0.05 {
    di "  WARNING: Pre-trends detected (p < 0.05)"
}
else {
    di "  No significant pre-trends (p >= 0.05)"
}

* ============================================================================
* PART 8: HETEROGENEOUS EVENT STUDY BY CLAIMING GROUP
* ============================================================================

di _n "=============================================="
di "HETEROGENEOUS EVENT STUDY: Early vs Delayed Claimers"
di "=============================================="

* Model 2: Interact event time with early_claimer
di _n "Model 2: Event study × Early claimer interaction"
xtreg life_sat ib6.event_bin_pos##i.early_claimer i.wave if sample_event == 1, fe vce(cluster hhidpn)
estimates store event_early

* Test for differential pre-trends by claiming group
di _n "Differential pre-trend test (early vs mid-late claimers):"
test 2.event_bin_pos#1.early_claimer 4.event_bin_pos#1.early_claimer
local diff_pretrend_p = r(p)
di "  Joint F-test p-value: " %6.4f `diff_pretrend_p'
if `diff_pretrend_p' < 0.05 {
    di "  WARNING: Differential pre-trends by claiming group (p < 0.05)"
    di "  Selection story: groups had different trajectories before claiming"
}
else {
    di "  No significant differential pre-trends (p >= 0.05)"
    di "  Groups had parallel trajectories before claiming"
}

* ============================================================================
* PART 9: CREATE EVENT STUDY FIGURE
* ============================================================================

di _n "=============================================="
di "GENERATING EVENT STUDY FIGURE"
di "=============================================="

* Restore basic event study estimates
estimates restore event_basic

* Create coefficient dataset for plotting using stored scalars
preserve

clear
set obs 7
gen event_bin = -6 + (_n - 1) * 2
gen coef = .
gen se = .
gen ci_lo = .
gen ci_hi = .

* Fill in coefficients using stored scalars
replace coef = coef_m6 in 1
replace coef = coef_m4 in 2
replace coef = 0 in 3  // Reference k=-2
replace coef = coef_0 in 4
replace coef = coef_p2 in 5
replace coef = coef_p4 in 6
replace coef = coef_p6 in 7

replace se = se_m6 in 1
replace se = se_m4 in 2
replace se = 0 in 3
replace se = se_0 in 4
replace se = se_p2 in 5
replace se = se_p4 in 6
replace se = se_p6 in 7

replace ci_lo = coef - 1.96*se
replace ci_hi = coef + 1.96*se

* Create the event study plot
twoway (rcap ci_lo ci_hi event_bin, lcolor(navy)) ///
       (scatter coef event_bin, mcolor(navy) msymbol(circle)), ///
    xline(-0.5, lpattern(dash) lcolor(gray)) ///
    yline(0, lpattern(solid) lcolor(gray)) ///
    xlabel(-6 "≤-6" -4 "-4" -2 "-2" 0 "0" 2 "+2" 4 "+4" 6 "≥+6") ///
    ylabel(, grid) ///
    xtitle("Years Relative to SS Claiming") ///
    ytitle("Life Satisfaction (relative to k=-2)") ///
    title("Event Study: Life Satisfaction Around SS Claiming") ///
    subtitle("Person fixed effects, wave fixed effects") ///
    note("Reference period: 2 years before claiming." ///
         "Vertical line at k=0 indicates claiming transition." ///
         "Bars show 95% confidence intervals, clustered by person.") ///
    legend(off) ///
    scheme(s2color)

graph export "$figures/fig3_event_study.png", replace width(1200)

di "Event study figure saved to: $figures/fig3_event_study.png"

restore

* ============================================================================
* PART 10: EXPORT RESULTS
* ============================================================================

di _n "=============================================="
di "EXPORTING RESULTS"
di "=============================================="

* Label event_bin_pos with human-readable event-time values for export
label define event_bin_pos_lbl 2 "k=-6" 4 "k=-4" 6 "k=-2 (ref)" 8 "k=0" 10 "k=+2" 12 "k=+4" 14 "k=+6"
label values event_bin_pos event_bin_pos_lbl

* Export event study coefficients table
esttab event_basic event_early using "$tables/event_study_coefficients.csv", ///
    se star(* 0.05 ** 0.01 *** 0.001) ///
    stats(N r2_w, labels("Observations" "Within R-sq")) ///
    mtitles("Basic Event Study" "× Early Claimer") ///
    coeflabels(2.event_bin_pos "k=-6" 4.event_bin_pos "k=-4" ///
        6.event_bin_pos "k=-2 (ref)" 8.event_bin_pos "k=0" ///
        10.event_bin_pos "k=+2" 12.event_bin_pos "k=+4" ///
        14.event_bin_pos "k=+6" ///
        2.event_bin_pos#1.early_claimer "k=-6 × Early" ///
        4.event_bin_pos#1.early_claimer "k=-4 × Early" ///
        6.event_bin_pos#1.early_claimer "k=-2 × Early" ///
        8.event_bin_pos#1.early_claimer "k=0 × Early" ///
        10.event_bin_pos#1.early_claimer "k=+2 × Early" ///
        12.event_bin_pos#1.early_claimer "k=+4 × Early" ///
        14.event_bin_pos#1.early_claimer "k=+6 × Early") ///
    drop(*wave* 0.early_claimer 1.early_claimer ///
        2.event_bin_pos#0.early_claimer 4.event_bin_pos#0.early_claimer ///
        6.event_bin_pos#0.early_claimer 8.event_bin_pos#0.early_claimer ///
        10.event_bin_pos#0.early_claimer 12.event_bin_pos#0.early_claimer ///
        14.event_bin_pos#0.early_claimer) ///
    csv replace ///
    title("Event Study: Life Satisfaction Around SS Claiming") ///
    addnotes("Reference period: k=-2 (2 years before claiming)" ///
             "All models include person FE and wave FE" ///
             "Standard errors clustered by person")

* ============================================================================
* PART 11: SUMMARY FOR MANUSCRIPT
* ============================================================================

di _n "=============================================="
di "SUMMARY FOR MANUSCRIPT"
di "=============================================="

di _n "EVENT STUDY SAMPLE:"
di "  Person-wave observations: `n_event'"
di "  Unique respondents with pre and post: `n_persons_event'"
di "  Pre-claim observations: `n_pre'"
di "  Post-claim observations: `n_post'"

di _n "KEY FINDINGS:"
di "  Pre-trend test p-value: " %6.4f `pretrend_p'
di "  Differential pre-trend test p-value: " %6.4f `diff_pretrend_p'

if `pretrend_p' >= 0.05 & `diff_pretrend_p' >= 0.05 {
    di _n "  INTERPRETATION: No evidence of pre-trends or differential pre-trends."
    di "  Groups had parallel trajectories before claiming, supporting causal interpretation."
}
else if `pretrend_p' < 0.05 {
    di _n "  INTERPRETATION: Pre-trends detected - life satisfaction was changing"
    di "  before claiming, potentially confounding the post-claim pattern."
}
else if `diff_pretrend_p' < 0.05 {
    di _n "  INTERPRETATION: Early claimers had different pre-claim trajectories,"
    di "  consistent with selection story (different groups, different paths)."
}

* ============================================================================
* COMPLETION
* ============================================================================

di _n "=============================================="
di "Event Study Analysis Complete"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

di _n "Output files:"
di "  $tables/event_study_coefficients.csv"
di "  $figures/fig3_event_study.png"

log close event

* ============================================================================
* END OF 07_event_study.do
* ============================================================================
