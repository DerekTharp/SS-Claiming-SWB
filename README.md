# Social Security Claiming Timing and Subjective Well-Being

This repository contains the replication code for a study examining the relationship between Social Security claiming age and subjective well-being among older Americans.

## Overview

This project investigates whether the timing of Social Security benefit claiming affects life satisfaction and other measures of subjective well-being. Using data from the Health and Retirement Study (HRS) Leave-Behind questionnaire (2006-2022), we employ a selection-on-observables design with progressive covariate adjustment, event-study analysis, Oster (2019) bounds, correlated random effects, and equivalence testing.

## Data Requirements

This study uses **public-use data** from the Health and Retirement Study (HRS):

- **RAND HRS Longitudinal File 2022 (V1)** - The harmonized RAND file containing core demographic, health, economic, and Leave-Behind psychosocial variables

### Data Access

1. Register at the [HRS website](https://hrsdata.isr.umich.edu/user/register)
2. Download the RAND HRS Longitudinal File 2022 (Stata format)
3. Place `randhrs1992_2022v1.dta` in the `data/raw/` directory

**Note:** Raw and derived data files are excluded from this repository via `.gitignore` due to data use agreements.

## Repository Structure

```
SS Claiming SWB Project/
├── code/
│   └── stata/
│       ├── 00_config.do              # Centralized paths, control macros, constants
│       ├── 00_master.do              # Master script to run full pipeline
│       ├── 00_test_syntax.do         # Syntax testing with simulated data
│       ├── 01_data_prep.do           # Data extraction and merging
│       ├── 02_sample.do              # Sample restrictions
│       ├── 03_derived.do             # Derived variables (FRA, categories, scales)
│       ├── 04_descriptive.do         # Descriptive statistics (Table 1)
│       ├── 05_analysis.do             # All analysis (Tables 2-5, MDES, CRE, Oster, CES-D)
│       ├── 06_robustness.do          # Robustness checks (Oster, TOST, subsamples)
│       ├── 07_event_study.do         # Event study with person FE
│       └── 08_sensitivity.do         # Age 70+, mechanism, behavioral heterogeneity
├── data/
│   ├── raw/                          # Raw HRS data (not tracked)
│   └── derived/                      # Analysis-ready datasets (not tracked)
├── output/
│   ├── tables/                       # Regression tables (CSV format)
│   ├── figures/                      # Graphs and visualizations
│   └── logs/                         # Stata log files (not tracked)
└── README.md
```

## Running the Analysis

### Prerequisites

- **Stata 17** or later (uses `asinh()` function, `bootstrap cluster()` options)
- Required Stata packages (automatically installed if missing):
  - `estout` - For formatted regression tables
  - `distinct` - For counting unique observations
  - `psacalc` - For Oster (2019) bounds

### Quick Start

1. Clone this repository
2. Download and place HRS data in `data/raw/`
3. Open Stata and run the full analysis:
   ```stata
   do "code/stata/00_master.do"
   ```

### Testing Without Real Data

To test the code syntax without HRS data:
```stata
do "code/stata/00_test_syntax.do"
```
This creates simulated data and runs all scripts (01-08) to verify syntax validity.

## Key Variables

### Outcome Variables
- **Life Satisfaction** (SWLS) - Primary outcome, 5-item scale (1-7)
- **Positive/Negative Affect** - Emotional well-being measures
- **Loneliness** - UCLA 3-item scale
- **CES-D** - Depressive symptoms (8-item, 0-8), full core HRS sample
- **Financial/Income Satisfaction** - Domain-specific financial well-being
- **Financial Strain** - Frequency of financial problems (mediator)

### Treatment Variable
- **Claiming Age** - Age at first receipt of Social Security benefits (62-70)
- Operationalized continuously (centered at 62) and categorically

## Identification Strategy

1. **Progressive Covariate Adjustment**: Sequential addition of demographics, health, marital status, and economic controls to decompose selection
2. **Event Study**: Person fixed-effects analysis tracing within-person trajectories around the claiming transition
3. **Oster (2019) Bounds**: Assess robustness to selection on unobservables
4. **Correlated Random Effects**: Mundlak-Chamberlain estimator controlling for time-invariant unobserved heterogeneity
5. **Equivalence Testing (TOST)**: Bound the maximum plausible effect size
6. **MDES Calibration**: Calibrate expected income-channel effect against statistical power

## Output Files

Key tables produced by the analysis:
- `table2_panel_a_primary.csv` - Main effects (primary specification, N = 19,032)
- `table2_panel_b_sensitivity.csv` - Sensitivity with survival expectations (N = 984)
- `table2_panel_c_constant.csv` - Constant-sample analysis
- `table3_mediation_revised.csv` - Mediation through financial strain
- `table4_heterogeneity_revised.csv` - Effect heterogeneity
- `table5_outcomes_revised.csv` - Alternative well-being outcomes
- `event_study_coefficients.csv` - Event study coefficients
- `mdes_calibration.csv` - MDES calibration results
- `oster_bounds.csv` - Oster bounds for selection bias
- `selection_decomposition.csv` - Sequential attenuation decomposition

## Citation

If you use this code, please cite:

Tharp, D. T. (2026). Does delayed Social Security claiming improve well-being? Evidence from the Health and Retirement Study. Working paper.

## License

This code is provided for academic research purposes. The HRS data are subject to their own data use agreements.

## Contact

Derek Tharp - University of Southern Maine

---

*Last updated: March 2026*
