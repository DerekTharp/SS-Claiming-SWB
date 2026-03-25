/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 04_descriptive.do
* Purpose: Generate descriptive statistics for analysis sample
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
cap log close descriptive
log using "$logs/04_descriptive.log", replace name(descriptive)

di "=============================================="
di "04_descriptive.do: Descriptive Statistics"
di "`c(current_date)' `c(current_time)'"
di "=============================================="

* Load analysis-ready data
use "$derived/hrs_analysis_ready.dta", clear

di "Observations: `c(N)'"

* ============================================================================
* PART 1: SAMPLE DESCRIPTION
* ============================================================================

di _n "=============================================="
di "PART 1: SAMPLE DESCRIPTION"
di "=============================================="

* Unique respondents
capture noisily distinct hhidpn
local n_persons = r(ndistinct)
di "Unique respondents: `n_persons'"

* Person-waves
di "Total person-wave observations: `c(N)'"

* Observations per person
bysort hhidpn: gen n_obs = _N
tab n_obs

* Waves in sample
tab wave

* ============================================================================
* PART 2: TABLE 1 - SAMPLE CHARACTERISTICS BY CLAIMING AGE
* ============================================================================

di _n "=============================================="
di "PART 2: TABLE 1 - Sample Characteristics"
di "=============================================="

* Create a cross-sectional version for Table 1 (first observation per person)
preserve

bysort hhidpn (wave): keep if _n == 1
di "Cross-sectional sample (first obs per person): `c(N)'"

* -----------------------------------------------------------------------------
* Panel A: Demographics
* -----------------------------------------------------------------------------

di _n "Panel A: Demographics"
di "============================================================"

* Overall and by claiming category
foreach cat in 1 2 3 {
    local catlab: label claim_cat3_lbl `cat'
    di _n "`catlab'"
    di "----------------------------------------"

    * N in category
    count if claim_cat3 == `cat'
    di "N = `r(N)'"

    * Age at claiming
    sum rassageb if claim_cat3 == `cat'
    di "Claiming age: " %4.1f r(mean) " (" %4.1f r(sd) ")"

    * Female
    sum female if claim_cat3 == `cat'
    di "Female: " %4.1f r(mean)*100 "%"

    * Race
    sum black if claim_cat3 == `cat'
    di "Black: " %4.1f r(mean)*100 "%"

    sum hispanic if claim_cat3 == `cat'
    di "Hispanic: " %4.1f r(mean)*100 "%"

    * Education
    sum educ_col_plus if claim_cat3 == `cat'
    di "College+: " %4.1f r(mean)*100 "%"

    * Married
    sum married if claim_cat3 == `cat'
    di "Married: " %4.1f r(mean)*100 "%"
}

* Test for differences across groups
di _n "Tests for differences across claiming groups:"

* Age (continuous) - Kruskal-Wallis
kwallis rassageb, by(claim_cat3)

* Female - Chi-square
tab claim_cat3 female, chi2

* College - Chi-square
tab claim_cat3 educ_col_plus, chi2

* -----------------------------------------------------------------------------
* Panel B: Health and Survival Expectations
* -----------------------------------------------------------------------------

di _n "Panel B: Health and Survival Expectations"
di "============================================================"

foreach cat in 1 2 3 {
    local catlab: label claim_cat3_lbl `cat'
    di _n "`catlab'"
    di "----------------------------------------"

    * Self-rated health (1=excellent to 5=poor)
    sum shlt if claim_cat3 == `cat'
    di "Self-rated health (1-5): " %4.2f r(mean) " (" %4.2f r(sd) ")"

    * Poor/fair health
    sum poor_health if claim_cat3 == `cat'
    di "Fair/poor health: " %4.1f r(mean)*100 "%"

    * Health conditions
    sum conde if claim_cat3 == `cat'
    di "Health conditions: " %4.2f r(mean) " (" %4.2f r(sd) ")"

    * Survival expectations
    sum liv75 if claim_cat3 == `cat'
    di "P(survive to 75): " %4.1f r(mean) " (" %4.1f r(sd) ")"
}

* Tests
tab claim_cat3 poor_health, chi2
kwallis liv75, by(claim_cat3)

* -----------------------------------------------------------------------------
* Panel C: Economic Status
* -----------------------------------------------------------------------------

di _n "Panel C: Economic Status"
di "============================================================"

foreach cat in 1 2 3 {
    local catlab: label claim_cat3_lbl `cat'
    di _n "`catlab'"
    di "----------------------------------------"

    * Household income
    sum hh_income if claim_cat3 == `cat', detail
    di "HH income mean: $" %12.0fc r(mean)
    di "HH income median: $" %12.0fc r(p50)

    * Household assets
    sum hh_assets if claim_cat3 == `cat', detail
    di "HH assets mean: $" %12.0fc r(mean)
    di "HH assets median: $" %12.0fc r(p50)
}

restore

* ============================================================================
* PART 3: OUTCOME VARIABLE DISTRIBUTIONS
* ============================================================================

di _n "=============================================="
di "PART 3: OUTCOME VARIABLE DISTRIBUTIONS"
di "=============================================="

* Life satisfaction
di _n "Life Satisfaction:"
sum life_sat, detail
* Note: life_sat is continuous, histogram instead of tab
histogram life_sat, freq title("Life Satisfaction Distribution")
graph export "$figures/life_sat_histogram.png", replace

* By claiming category
table claim_cat3, stat(mean life_sat) stat(sd life_sat) stat(n life_sat)

* Positive affect
di _n "Positive Affect:"
sum pos_affect, detail

table claim_cat3, stat(mean pos_affect) stat(sd pos_affect) stat(n pos_affect)

* Negative affect
di _n "Negative Affect:"
sum neg_affect, detail

table claim_cat3, stat(mean neg_affect) stat(sd neg_affect) stat(n neg_affect)

* Financial strain (mediator)
di _n "Financial Strain (Mediator):"
sum fin_strain, detail

table claim_cat3, stat(mean fin_strain) stat(sd fin_strain) stat(n fin_strain)

* ============================================================================
* PART 4: BIVARIATE RELATIONSHIPS
* ============================================================================

di _n "=============================================="
di "PART 4: BIVARIATE RELATIONSHIPS"
di "=============================================="

* Claiming age vs. life satisfaction
di _n "Correlation: Claiming age and Life satisfaction"
pwcorr rassageb life_sat, sig

* Claiming age vs. financial strain
di _n "Correlation: Claiming age and Financial strain"
pwcorr rassageb fin_strain, sig

* Financial strain vs. life satisfaction
di _n "Correlation: Financial strain and Life satisfaction"
pwcorr fin_strain life_sat, sig

* Correlation matrix
di _n "Correlation matrix of key variables:"
pwcorr rassageb life_sat pos_affect neg_affect fin_strain, sig star(.05)

* ============================================================================
* PART 5: OUTCOME MEANS BY CLAIMING AGE (UNADJUSTED)
* ============================================================================

di _n "=============================================="
di "PART 5: OUTCOME MEANS BY CLAIMING AGE"
di "=============================================="

* Life satisfaction by claiming age (continuous)
table rassageb, stat(mean life_sat) stat(sd life_sat) stat(n life_sat) nformat(%5.2f)

* Life satisfaction by claiming category
di _n "Life Satisfaction by Claiming Category (with tests):"
oneway life_sat claim_cat3, bonferroni tabulate

* Financial strain by claiming category
di _n "Financial Strain by Claiming Category:"
oneway fin_strain claim_cat3, bonferroni tabulate

* ============================================================================
* PART 6: WEIGHTED ESTIMATES
* ============================================================================

di _n "=============================================="
di "PART 6: WEIGHTED ESTIMATES (LB weights)"
di "=============================================="

* Set survey design
svyset [pweight=lb_weight]

* Weighted means by claiming category
di _n "Weighted Life Satisfaction by Claiming Category:"
svy: mean life_sat, over(claim_cat3)

* Test for differences
svy: reg life_sat i.claim_cat3
testparm i.claim_cat3

* Weighted financial strain
di _n "Weighted Financial Strain by Claiming Category:"
svy: mean fin_strain, over(claim_cat3)

svy: reg fin_strain i.claim_cat3
testparm i.claim_cat3

* ============================================================================
* PART 7: EXPORT TABLE 1 TO FILE
* ============================================================================

di _n "=============================================="
di "PART 7: EXPORTING TABLE 1"
di "=============================================="

* Create summary statistics for export

preserve
bysort hhidpn (wave): keep if _n == 1

* Overall statistics
estpost summarize rassageb age female black hispanic educ_col_plus ///
    married shlt poor_health conde liv75 ///
    hh_income hh_assets life_sat fin_strain, detail

* Export
esttab using "$tables/table1_overall.csv", ///
    cells("count mean sd min max") ///
    csv replace label

* By claiming category
estpost tabstat rassageb age female black hispanic educ_col_plus ///
    married shlt poor_health conde liv75 ///
    hh_income hh_assets life_sat fin_strain, ///
    by(claim_cat3) statistics(mean sd n) columns(statistics)

esttab using "$tables/table1_by_claiming.csv", ///
    main(mean) aux(sd) ///
    csv replace label

restore

di "Tables exported to $tables/"

* ============================================================================
* PART 8: SAMPLE FLOWCHART NUMBERS
* ============================================================================

di _n "=============================================="
di "PART 8: SAMPLE FLOWCHART"
di "=============================================="

* These numbers come from the restrictions applied in 02_sample.do
* Reproduce key counts here for documentation

di _n "Sample Selection Summary:"
di "1. Starting sample (RAND HRS 2022): [Load from 01_data_prep.do]"
di "2. After birth cohort restriction (1931-1959): [From 02_sample.do]"
di "3. After requiring SS claiming: [From 02_sample.do]"
di "4. After reshaping to person-wave: [From 02_sample.do]"
di "5. After LB eligibility/completion: [From 02_sample.do]"
di "6. After excluding proxy respondents: [From 02_sample.do]"
di "7. Final analytic sample (person-waves): `c(N)'"

capture noisily distinct hhidpn
di "8. Final analytic sample (unique persons): `r(ndistinct)'"

* ============================================================================
* PART 9: APPENDIX - VARIABLE DISTRIBUTIONS
* ============================================================================

di _n "=============================================="
di "PART 9: APPENDIX - DETAILED DISTRIBUTIONS"
di "=============================================="

* Claiming age distribution
di _n "Claiming Age Distribution:"
histogram rassageb, discrete freq ///
    title("Distribution of Social Security Claiming Age") ///
    xtitle("Claiming Age") ytitle("Frequency")
graph export "$figures/claiming_age_dist.png", replace

* Life satisfaction by claiming age
di _n "Life Satisfaction by Claiming Age:"
graph bar (mean) life_sat, over(rassageb) ///
    title("Mean Life Satisfaction by Claiming Age") ///
    ytitle("Life Satisfaction")
graph export "$figures/lifesat_by_claimage.png", replace

* Financial strain by claiming age
graph bar (mean) fin_strain, over(rassageb) ///
    title("Mean Financial Strain by Claiming Age") ///
    ytitle("Financial Strain")
graph export "$figures/finstrain_by_claimage.png", replace

* Scatter: Life satisfaction vs claiming age
twoway (scatter life_sat rassageb, msize(small) jitter(5)) ///
    (lfit life_sat rassageb, lwidth(thick)), ///
    title("Life Satisfaction vs. Claiming Age") ///
    xtitle("Claiming Age") ytitle("Life Satisfaction") ///
    legend(off)
graph export "$figures/lifesat_vs_claimage_scatter.png", replace

* ============================================================================
* COMPLETION
* ============================================================================

di _n "=============================================="
di "Descriptive Statistics Complete"
di "`c(current_date)' `c(current_time)'"
di "=============================================="
di _n "Output files:"
di "  Tables: $tables/table1_overall.csv"
di "  Tables: $tables/table1_by_claiming.csv"
di "  Figures: $figures/claiming_age_dist.png"
di "  Figures: $figures/lifesat_by_claimage.png"
di "  Figures: $figures/finstrain_by_claimage.png"

log close descriptive

* ============================================================================
* END OF 04_descriptive.do
* ============================================================================
