# Replication Code: Does Delayed Social Security Claiming Improve Well-Being?

Stata replication code for "Does Delayed Social Security Claiming Improve Well-Being? Evidence from the Health and Retirement Study."

## Requirements

- Stata SE or MP (tested with Stata 18)
- RAND HRS Longitudinal File 2022 (V1): freely available at https://hrsdata.isr.umich.edu/

## Setup

1. Download `randhrs1992_2022v1.dta` from the HRS Data Portal
2. Place it in `../data/raw/`
3. In Stata, `cd` to the project root directory
4. Run the full pipeline: `do code/stata/00_master.do`

The project path is auto-detected from the working directory. If auto-detection fails, edit the manual override in `stata/00_config.do`.

## Pipeline

| Script | Purpose |
|--------|---------|
| `00_config.do` | Centralized paths, control variable macros, constants |
| `00_master.do` | Runs all scripts sequentially |
| `01_data_prep.do` | Extract and merge RAND HRS variables |
| `02_sample.do` | Apply sample restrictions, reshape to person-wave |
| `03_derived.do` | Create claiming age categories, derived variables |
| `04_descriptive.do` | Table 1: Descriptive statistics |
| `05_analysis.do` | All analysis: selection decomposition, Tables 2-5, MDES, CRE, Oster, domain satisfaction, CES-D |
| `06_robustness.do` | Oster bounds, specification checks |
| `07_event_study.do` | Event study with person fixed effects |
| `08_sensitivity.do` | Age 70+, first-stage, behavioral heterogeneity |

## Contact

Derek Tharp — derek.tharp@maine.edu
