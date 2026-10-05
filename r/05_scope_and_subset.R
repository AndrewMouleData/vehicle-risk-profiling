# ------------------------------------------------------------------------------
# 05_scope_and_subset.R
#
# Purpose:
# - Apply explicit analytical scoping decisions to the full risk profile dataset
#   and produce two derived subsets for downstream reporting and visualisation.
# - Preserve a clear separation between exploratory analysis (04), business-aligned
#   scoping (05), and final communication outputs (06).
#
# Design choices:
# - Two output objects are produced:
#     risk_profiles_scoped
#       Applies the minimum vehicle count threshold (>= 500) and excludes vehicle
#       types that are not motor vehicles or cannot form vehicle-characteristic 
#       profiles. Profiles with undefined analytical dimensions are retained at 
#       this stage so aggregate coverage and residual data-quality limitations
#       remain visible.
#
#     risk_profiles_reporting
#       Derived from risk_profiles_scoped, with profiles additionally excluded
#       where propulsion, engine capacity band, and vehicle age band are all
#       simultaneously undefined. This object is used exclusively for ranked
#       outputs, visualisation, and README-facing interpretation.
#
# - risk_profiles is never modified. All scoping is applied to derived subsets
#   so the full analytical dataset remains available for reference.
# - Scoping decisions are implemented as named parameters and clearly separated
#   filter blocks so they remain transparent, auditable, and easy to revise.
# - A coverage summary is printed at the end of the script to quantify the size
#   and vehicle representation of each subset relative to the full dataset.
#
# Scoping decisions applied and rationale:
#
# 1. Minimum vehicle count threshold: 500
#    Profiles below this threshold are excluded from scoped/reporting subsets
#    because very small samples can produce unstable severity estimates and
#    uninformative rankings. Sensitivity analysis in 04_exploratory_analysis.R
#    showed that a threshold of 500 retains the overwhelming majority of vehicle
#    involvements while removing profiles that contribute little coverage but
#    disproportionate instability.
#
# 2. Profiles without defined vehicle characteristics:
#    The framework profiles vehicles by propulsion, engine capacity band and
#    vehicle age band. Records lacking these details cannot form interpretable
#    profiles, and are handled in two steps:
#
#    a) Non-profilable vehicle types (applied in risk_profiles_scoped):
#       Pedal cycle (1), Ridden horse (16), Tram (18) and Mobility scooter (22)
#       appear in STATS19 because it records a broad set of road users, but have
#       no propulsion, engine capacity or vehicle age recorded (100% undefined
#       for pedal cycles, horses and trams; at least 98.7% for mobility
#       scooters). They are excluded by type so they do not distort the coverage
#       figures for the scoped dataset.
#
#    b) Simultaneously undefined profiles (applied in risk_profiles_reporting only):
#       Profiles where propulsion, engine capacity band and vehicle age band are
#       all unknown/undefined are retained in risk_profiles_scoped, so the residual
#       data-quality limitation stays visible in the coverage figures. They are
#       excluded from risk_profiles_reporting because they cannot support
#       characteristic-level interpretation.
# ------------------------------------------------------------------------------

source("r/01_connect_db.R")
source("r/02_pull_mart_data.R")

# ------------------------------------------------------------------------------
# Scoping parameters
# Defined once here so all decisions are visible and adjustable in one place.
# ------------------------------------------------------------------------------

MIN_VEHICLE_COUNT <- 500

NON_ELIGIBLE_TYPES <- c(
  1,   # Pedal cycle      — no propulsion, engine or vehicle age recorded
  16,  # Ridden horse     — no propulsion, engine or vehicle age recorded
  18,  # Tram             — no propulsion, engine or vehicle age recorded
  22   # Mobility scooter — propulsion, engine and age almost always unrecorded
)

# ------------------------------------------------------------------------------
# risk_profiles_scoped
# Applies threshold and non-profilable vehicle type exclusions.
# Simultaneously undefined profiles are retained at this stage.
# ------------------------------------------------------------------------------

risk_profiles_scoped <- risk_profiles |>
  filter(
    vehicle_count >= MIN_VEHICLE_COUNT,
    !vehicle_type %in% NON_PROFILABLE_TYPES
  )

# ------------------------------------------------------------------------------
# risk_profiles_reporting
# Extends risk_profiles_scoped by additionally excluding profiles where all
# three analytical dimensions are simultaneously undefined.
# ------------------------------------------------------------------------------

risk_profiles_reporting <- risk_profiles_scoped |>
  filter(
    !(propulsion_label %in% c("Undefined", "Unknown") &
        engine_capacity_band == "Unknown" &
        vehicle_age_band == "Unknown")
  )

# ------------------------------------------------------------------------------
# Coverage summary
# Confirms the size and vehicle coverage of each output object relative to
# the full dataset. Printed for transparency and pipeline auditability.
# Note: coverage percentages here use the full dataset (including non-profilable
# vehicle types) as the denominator. Figure 6 in 06_visualisation.R uses the
# profilable total instead, so its percentages differ.
# ------------------------------------------------------------------------------

total_vehicles_full <- sum(risk_profiles$vehicle_count, na.rm = TRUE)
vehicles_scoped <- sum(risk_profiles_scoped$vehicle_count, na.rm = TRUE)
vehicles_reporting <- sum(risk_profiles_reporting$vehicle_count, na.rm = TRUE)

message("--- Scoping summary ---")
message(
  "Full dataset:              ",
  nrow(risk_profiles), " profiles, ",
  format(total_vehicles_full, big.mark = ","), " vehicles"
)
message(
  "risk_profiles_scoped:      ",
  nrow(risk_profiles_scoped), " profiles, ",
  format(vehicles_scoped, big.mark = ","), " vehicles (",
  round(vehicles_scoped / total_vehicles_full * 100, 1),
  "% coverage)"
)
message(
  "risk_profiles_reporting:   ",
  nrow(risk_profiles_reporting), " profiles, ",
  format(vehicles_reporting, big.mark = ","), " vehicles (",
  round(vehicles_reporting / total_vehicles_full * 100, 1),
  "% coverage)"
)