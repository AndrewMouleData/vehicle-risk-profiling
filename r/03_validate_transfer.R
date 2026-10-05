# ------------------------------------------------------------------------------
# 03_validate_transfer.R
#
# Purpose:
# - Confirm that the data pulled from mart.vehicle_risk_profiles_2015_2024 into R
#   is structurally sound and consistent with the SQL mart quality checks.
# - Validate key mathematical properties of the contribution metrics before any
#   analytical or visualisation work begins.
# - Surface data quality issues explicitly rather than allowing them to propagate
#   into downstream analyses.
# - Prompt investigation of the SQL pipeline in cases where checks fail.
#
# Design choices:
# - Validation is performed entirely in R against the in-memory dataframe.
# - Checks mirror the SQL mart quality checks (05_mart_quality_checks.sql) where
#   applicable, providing an independent cross-layer confirmation rather than
#   simply re-running the same logic in a different language.
# - Results are printed as named, human-readable statements rather than raw
#   values so the output is immediately interpretable when the script is run.
# ------------------------------------------------------------------------------

source("r/01_connect_db.R")
source("r/02_pull_mart_data.R")

# ----------------------------------------------------------
# Check 1: Risk proxy score formula is consistent
# Expected result: Count of rows exceeding tolerance is 0
# ----------------------------------------------------------

message("--- Check 1: risk_proxy_score formula integrity ---")
risk_proxy_mismatch <- sum(
  abs(
    risk_profiles$risk_proxy_score -
      (risk_profiles$frequency_share * risk_profiles$avg_weighted_severity_per_vehicle)
  ) > 1e-6
)
message("Rows failing risk_proxy_score check: ", risk_proxy_mismatch)

# ----------------------------------------------------------
# Check 2: avg_weighted_severity_per_vehicle formula integrity
# Expected result: Count of rows with mismatch > tolerance is 0
# ----------------------------------------------------------

message("--- Check 2: avg_weighted_severity_per_vehicle formula integrity ---")
severity_avg_mismatch <- sum(
  abs(
    risk_profiles$avg_weighted_severity_per_vehicle -
      (risk_profiles$weighted_severity_total / risk_profiles$vehicle_count)
  ) > 1e-6, 
  na.rm = TRUE
)
message("Rows failing severity average check: ", severity_avg_mismatch)

# ----------------------------------------------------------
# Check 3: No duplicate business grain rows
# Expected result: 0 rows - each combination of the four business-grain dimensions
# should appear exactly once.
# ----------------------------------------------------------

message("--- Check 3: duplicate business-grain rows ---")
duplicate_profiles <- risk_profiles |>
  count(vehicle_type, propulsion_code, engine_capacity_band, vehicle_age_band) |>
  filter(n > 1)

message("Duplicate business-grain rows: ", nrow(duplicate_profiles))

# ----------------------------------------------------------
# Check 4: Row and column count
# Expected result: row count consistent with SQL QA
# ----------------------------------------------------------

message("--- Check 4: Dimensions ---")
message("Rows:    ", nrow(risk_profiles))
message("Columns: ", ncol(risk_profiles))

# ----------------------------------------------------------
# Check 5: No NAs in key metric and dimension columns
# Expected result: all counts = 0
# ----------------------------------------------------------

message("--- Check 5: NA counts in key columns ---")

risk_profiles |>
  summarise(
    across(
      c(
        vehicle_type,
        vehicle_type_label,
        propulsion_code,
        propulsion_label,
        engine_capacity_band,
        vehicle_age_band,
        vehicle_count,
        slight_count,
        serious_count,
        fatal_count,
        weighted_severity_total,
        avg_weighted_severity_per_vehicle,
        frequency_share,
        risk_proxy_score,
        risk_rank
      ),
      ~ sum(is.na(.x)),
      .names = "na_in_{.col}"
    )
  ) |>
  glimpse()

# ----------------------------------------------------------
# Check 6: frequency_share sums to 1.0
# Expected result: result within floating point tolerance of 1e-6
# ----------------------------------------------------------

message("--- Check 6: frequency_share sum ---")
freq_share_sum <- sum(risk_profiles$frequency_share)
message("Sum of frequency_share: ", round(freq_share_sum, 10))
message("Validation: ", ifelse(abs(freq_share_sum - 1.0) < 1e-6, "PASS", "FAIL"))

# ----------------------------------------------------------
# Check 7: No zero or negative vehicle counts or metric values
# Expected result: all counts = 0
# ----------------------------------------------------------

message("--- Check 7: Zero or negative values ---")
message("Zero or negative vehicle_count:    ",
        sum(risk_profiles$vehicle_count <= 0))
message("Zero or negative frequency_share:  ",
        sum(risk_profiles$frequency_share <= 0))
message("Negative risk_proxy_score:         ",
        sum(risk_profiles$risk_proxy_score < 0))

# ----------------------------------------------------------
# Check 8: risk_rank is populated and sequential from 1
# Expected result: min = 1, max < row count because tied scores share a dense rank
# ----------------------------------------------------------

message("--- Check 8: risk_rank integrity ---")
message("Min rank:  ", min(risk_profiles$risk_rank, na.rm = TRUE))
message("Max rank:  ", max(risk_profiles$risk_rank, na.rm = TRUE))
message("NA ranks:  ", sum(is.na(risk_profiles$risk_rank)))

# ----------------------------------------------------------
# Check 9: Column types are as expected
# Expected result: appropriate classes for relevant cols
# ----------------------------------------------------------

message("--- Check 9: Column types ---")
risk_profiles |>
  summarise(across(everything(), ~ class(.x))) |>
  glimpse()

# ----------------------------------------------------------
# Check 10: Unknown and N/A dimension group volumes
# Expected result: sizable but not dominant
# ----------------------------------------------------------

message("--- Check 10: Unknown and N/A dimension volumes ---")
unknown_summary <- risk_profiles |>
  filter(
    is.na(vehicle_type_label) | toupper(trimws(vehicle_type_label)) %in% c("UNKNOWN", "N/A") | 
    is.na(engine_capacity_band) | toupper(trimws(engine_capacity_band)) %in% c("UNKNOWN", "N/A") | 
    is.na(vehicle_age_band) | toupper(trimws(vehicle_age_band)) %in% c("UNKNOWN", "N/A")
  ) |>
  group_by(vehicle_type_label, engine_capacity_band, vehicle_age_band) |>
  summarise(vehicle_count = sum(vehicle_count, na.rm = TRUE)) |>
  arrange(desc(vehicle_count))

total_vehicles <- sum(risk_profiles$vehicle_count, na.rm = TRUE)
unknown_total  <- sum(unknown_summary$vehicle_count, na.rm = TRUE)
unknown_pct    <- (unknown_total / total_vehicles) * 100

message("Unknown or N/A groups as % of total vehicles: ", round(unknown_pct, 2), "%")