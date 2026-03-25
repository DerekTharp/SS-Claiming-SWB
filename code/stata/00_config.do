/*******************************************************************************
* Project: SS Claiming Timing and Subjective Well-Being
* File: 00_config.do
* Purpose: Centralized configuration -- paths, control variable macros, constants
*
* INSTRUCTIONS:
*   1. cd to the project root in Stata (the path is auto-detected)
*   2. Run 00_master.do, which sources this file before calling other scripts
*   If auto-detection fails, edit the manual override below.
*******************************************************************************/

* ============================================================================
* PROJECT PATH
* ============================================================================

* Auto-detect the project root when Stata is launched from either:
*   1. the project root, or
*   2. the code/stata directory
* Otherwise fall back to the manual override below.

local cwd `"`c(pwd)'"'
local cwd : subinstr local cwd "\" "/", all
local project_root ""

capture confirm file "`cwd'/code/stata/00_config.do"
if !_rc {
    local project_root "`cwd'"
}
else {
    capture confirm file "`cwd'/00_config.do"
    if !_rc & regexm("`cwd'", "/code/stata$") {
        local project_root = subinstr("`cwd'", "/code/stata", "", 1)
    }
}

if "`project_root'" != "" {
    global project "`project_root'"
}
else {
    * Manual override -- edit this if auto-detection does not work
    global project "/path/to/your/project/directory"
}

global config_loaded 1

* ============================================================================
* DERIVED PATHS
* ============================================================================

global raw_data   "$project/data/raw"
global derived    "$project/data/derived"
global output     "$project/output"
global tables     "$output/tables"
global figures    "$output/figures"
global logs       "$output/logs"
global code       "$project/code/stata"

* ============================================================================
* CONTROL VARIABLE MACROS
* ============================================================================

* --- Building blocks ---
global demo          female i.raeduc black other_race hispanic
global health        shlt poor_health conde
global health_basic  shlt conde
global survival      liv75
global econ          ln_hh_income ihs_hh_assets
global marital       married widowed divorced
global age_controls  age age_sq
global wave_fe       i.wave

* --- Composite specifications ---

* PRIMARY (Total Effect): excludes liv75 and income/assets to preserve sample
global controls_primary  $demo $health $marital $age_controls

* Alias for backward compatibility
global controls_total    $controls_primary

* For health-interaction figures (excludes poor_health, the interaction variable)
global controls_noph     $demo $health_basic $marital $age_controls

* DIRECT EFFECT: includes income/assets but not liv75
global controls_direct   $demo $health $econ $marital $age_controls

* WITH SURVIVAL EXPECTATIONS (sensitivity, restricted sample)
global controls_with_liv75  $demo $health $survival $marital $age_controls

* FULL MODEL: all controls (smallest sample)
global controls_full     $demo $health $survival $econ $marital $age_controls

* Legacy alias used in some robustness code
global controls          $controls_full

* ============================================================================
* CONSTANTS AND THRESHOLDS
* ============================================================================

* Birth cohort bounds for sample restriction
global cohort_lo  1931
global cohort_hi  1959

* Valid claiming age range
global claim_age_lo  62
global claim_age_hi  70

* Equivalence testing: effect size threshold (in SD units)
global tost_delta  0.10

* Note: sd_lifesat is computed at runtime from the analytic sample rather than
* hardcoded. Scripts that need it should run:
*   qui sum life_sat if sample_primary == 1
*   local sd_lifesat = r(sd)

* ============================================================================
* END OF 00_config.do
* ============================================================================
