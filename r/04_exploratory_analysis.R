# ------------------------------------------------------------------------------
# 04_exploratory_analysis.R
#
# Purpose:
# - Produce structured exploratory outputs aligned to the business task:
#   "to what extent can open UK road safety data profile vehicles by collision
#   involvement and injury severity, and where do its limitations prevent
#   reliable profiling?"
# - Rank profiles by risk_proxy_score and decompose the contribution score into
#   its frequency and average-severity components.
# - Identify high-severity / low-frequency profiles and high-frequency /
#   lower-severity profiles.
#
# Design choices:
# - All exploratory analysis is performed on the full unscoped dataset. No 
#   profiles are filtered or excluded at this stage. Scoping decisions are 
#   deferred to 05_scope_and_subset.R, informed by the threshold sensitivity 
#   analysis in section 6 of this script.
# - Exploratory outputs are printed to the console as tibbles. Visualisation
#   is deferred to 06_visualisation.R.
# - Columns are rounded for display and readability only. Source columns in
#   risk_profiles remain unrounded.
# - Analysis follows the frequency x severity contribution framework of this project:
#   1. Ranked profiles by risk_proxy_score                    (headline contribution view)
#   2. Frequency vs severity decomposition                    (structural interpretation)
#   3. High-severity / low-frequency profiles                 (severity-led profiles)
#   4. High-frequency / lower-severity profiles               (volume-led profiles)
#   5. Vehicle count distribution                             (collision-involvement volume)
#   6. Threshold sensitivity analysis                         (informs 05_scope_and_subset.R)
# ------------------------------------------------------------------------------

source("r/01_connect_db.R")
source("r/02_pull_mart_data.R")

# ------------------------------------------------------------------------------
# 1. Ranked profiles by risk_proxy_score
# ------------------------------------------------------------------------------

message("\n--- 1a: Top 10 profiles by contribution score ---")
risk_profiles |>
  arrange(risk_rank) |>
  mutate(
    frequency_pct            = round(frequency_share * 100, 2),
    avg_severity_rounded     = round(avg_weighted_severity_per_vehicle, 2),
    contribution_score_pct   = round(risk_proxy_score * 100, 2)
  ) |>
  select(
    risk_rank,
    vehicle_type_label,
    propulsion_label,
    engine_capacity_band,
    vehicle_age_band,
    vehicle_count,
    avg_severity_rounded,
    frequency_pct,
    contribution_score_pct
  ) |>
  slice_head(n = 10) |>
  print()

message("\n--- 1b: Bottom 10 profiles by contribution score ---")
risk_profiles |>
  arrange(desc(risk_rank)) |>
  mutate(
    frequency_pct            = round(frequency_share * 100, 2),
    avg_severity_rounded     = round(avg_weighted_severity_per_vehicle, 2),
    contribution_score_pct   = round(risk_proxy_score * 100, 2)
  ) |>
  select(
    risk_rank,
    vehicle_type_label,
    propulsion_label,
    engine_capacity_band,
    vehicle_age_band,
    vehicle_count,
    avg_severity_rounded,
    frequency_pct,
    contribution_score_pct
  ) |>
  slice_head(n = 10) |>
  print()

message("\n--- 1c: Distribution of contribution scores ---")
summary(risk_profiles$risk_proxy_score)

risk_profiles |>
  summarise(
    profiles     = n(),
    mean_score   = round(mean(risk_proxy_score), 6),
    median_score = round(median(risk_proxy_score), 6),
    sd_score     = round(sd(risk_proxy_score), 6),
    p90_score    = round(quantile(risk_proxy_score, 0.90), 6),
    p95_score    = round(quantile(risk_proxy_score, 0.95), 6),
    p99_score    = round(quantile(risk_proxy_score, 0.99), 6)
  ) |>
  print()

# ------------------------------------------------------------------------------
# 2. Frequency vs severity decomposition
# Reveals whether a high risk_proxy_score is driven by frequency, severity, or both.
# ------------------------------------------------------------------------------

message("\n--- 2: Frequency vs severity decomposition ---")

freq_median     <- median(risk_profiles$frequency_share)
severity_median <- median(risk_profiles$avg_weighted_severity_per_vehicle)

risk_profiles_quadrants <- risk_profiles |>
  mutate(
    risk_quadrant = case_when(
      frequency_share >= freq_median &
        avg_weighted_severity_per_vehicle >= severity_median ~ "High frequency / High severity",
      frequency_share >= freq_median &
        avg_weighted_severity_per_vehicle <  severity_median ~ "High frequency / Low severity",
      frequency_share <  freq_median &
        avg_weighted_severity_per_vehicle >= severity_median ~ "Low frequency / High severity",
      frequency_share <  freq_median &
        avg_weighted_severity_per_vehicle <  severity_median ~ "Low frequency / Low severity"
    )
  )

risk_profiles_quadrants |>
  count(risk_quadrant) |>
  arrange(desc(n)) |>
  print()

# ------------------------------------------------------------------------------
# 3. High-severity / low-frequency profiles
# These appear less often in the collision-involved dataset but have higher
# average recorded injury-burden signals.
# ------------------------------------------------------------------------------

message("\n--- 3: High-severity / low-frequency profiles ---")
risk_profiles_quadrants |>
  filter(risk_quadrant == "Low frequency / High severity") |>
  arrange(desc(avg_weighted_severity_per_vehicle)) |>
  mutate(
    frequency_pct            = round(frequency_share * 100, 2),
    avg_severity_rounded     = round(avg_weighted_severity_per_vehicle, 2),
    contribution_score_pct   = round(risk_proxy_score * 100, 2)
  ) |>
  select(
    vehicle_type_label,
    propulsion_label,
    engine_capacity_band,
    vehicle_age_band,
    vehicle_count,
    avg_severity_rounded,
    frequency_pct,
    contribution_score_pct
  ) |>
  slice_head(n = 15) |>
  print()

# ------------------------------------------------------------------------------
# 4. High-frequency / lower-severity profiles
# These appear frequently in the collision-involved dataset and can still make a
# meaningful aggregate contribution because of their volume.
# ------------------------------------------------------------------------------

message("\n--- 4: High-frequency / lower-severity profiles ---")
risk_profiles_quadrants |>
  filter(risk_quadrant == "High frequency / Low severity") |>
  arrange(desc(frequency_share)) |>
  mutate(
    frequency_pct            = round(frequency_share * 100, 2),
    avg_severity_rounded     = round(avg_weighted_severity_per_vehicle, 2),
    contribution_score_pct   = round(risk_proxy_score * 100, 2)
  ) |>
  select(
    vehicle_type_label,
    propulsion_label,
    engine_capacity_band,
    vehicle_age_band,
    vehicle_count,
    avg_severity_rounded,
    frequency_pct,
    contribution_score_pct
  ) |>
  slice_head(n = 15) |>
  print()

# ------------------------------------------------------------------------------
# 5. Vehicle count distribution
# Contextualises the evidence base across the full profile space and supports
# interpretation of the threshold sensitivity analysis in section 6.
# ------------------------------------------------------------------------------

message("\n--- 5: Vehicle count distribution ---")
summary(risk_profiles$vehicle_count)

risk_profiles |>
  summarise(
    total_profiles = n(),
    total_vehicles = sum(vehicle_count),
    mean_count     = round(mean(vehicle_count), 1),
    median_count   = median(vehicle_count),
    p90_count      = quantile(vehicle_count, 0.90),
    max_count      = max(vehicle_count)
  ) |>
  print()

# ------------------------------------------------------------------------------
# 6. Threshold sensitivity analysis
# Tests the trade-off between minimum vehicle count thresholds and profile
# retention. No threshold is committed to here. Results directly inform the
# scoping decisions made in 05_scope_and_subset.R.
# ------------------------------------------------------------------------------

message("\n--- 6: Threshold sensitivity analysis ---")

thresholds <- c(100, 200, 500, 750, 1000, 1500, 2000)

for (t in thresholds) {
  retained <- risk_profiles |> filter(vehicle_count >= t)
  message(
    "Threshold ", t, ": ",
    nrow(retained), " profiles retained, ",
    round(sum(retained$vehicle_count) / sum(risk_profiles$vehicle_count) * 100, 1),
    "% of vehicles covered"
  )
}