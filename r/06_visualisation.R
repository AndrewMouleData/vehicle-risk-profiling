# ------------------------------------------------------------------------------
# 06_visualisation.R
#
# Purpose
# - Produce all charts that communicate the analytical findings of this project
#   in a format suitable for inclusion in the README.
# - Save figure outputs to outputs/figures/.
#
# Design choices:
# - All interpretive visualisations use risk_profiles_reporting. The coverage
#   summary chart compares eligible vehicle-category profiles before thresholding,
#   the scoped dataset, and the reporting subset to quantify the trade-off
#   between interpretability and coverage.
# - risk_quadrant is derived here on risk_profiles_reporting as it is a
#   visualisation-layer construct, not a scoping decision.
# - The frequency vs severity chart is presented first because it most directly
#   communicates the structure of the frequency x severity proxy across vehicle
#   profiles.
# - A minimal ggplot2 theme is applied consistently across all charts via a
#   single shared theme object defined once at the top of the script, ensuring
#   visual consistency without repetition.
# - A save_plot() helper function encapsulates ggsave() defaults, ensuring
#   consistent output dimensions and resolution across all charts from a single
#   point of control.
# - This script assumes required tidyverse packages are already loaded upstream
#   via sourced scripts in the pipeline.
# - Plots are both printed to the plot pane and saved to outputs/figures/ as
#   PNG files.
# - No modelling, probabilistic inference, or causal claims are made in any
#   output.
#
# Outputs:
#   figures/
#     01_frequency_severity_quadrants.png
#     02_top_10_risk_profiles.png
#     03_risk_proxy_distribution.png
#     04_grouped_vehicle_type_risk_contribution.png
#     05_vehicle_count_distribution.png
#     06_coverage_summary.png
# ------------------------------------------------------------------------------

source("r/01_connect_db.R")
source("r/02_pull_mart_data.R")
source("r/05_scope_and_subset.R")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Shared theme
# Applied consistently across all charts.
# ------------------------------------------------------------------------------

viz_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey50"),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# ------------------------------------------------------------------------------
# Shared reporting caption
# Stores the reporting-scope caption once so it can be reused consistently
# across all plots built from risk_profiles_reporting.
# ------------------------------------------------------------------------------

reporting_caption <- "Source: UK STATS19, 2015–2024. Reporting subset - see methodology for scoping details."

# ------------------------------------------------------------------------------
# save_plot() helper
# Wraps ggsave() with project defaults. All charts use consistent dimensions
# and resolution unless explicitly overridden in the call.
# ------------------------------------------------------------------------------

save_plot <- function(plot, filename, width = 10, height = 6, dpi = 150) {
  ggsave(
    filename = file.path("outputs/figures", filename),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi
  )
}

# ------------------------------------------------------------------------------
# Quadrant classification
# Derived here on risk_profiles_reporting for use in chart 1.
# Median splits on frequency_share and avg_weighted_severity_per_vehicle.
# ------------------------------------------------------------------------------

freq_median <- median(
  risk_profiles_reporting$frequency_share,
  na.rm = TRUE
)

severity_median <- median(
  risk_profiles_reporting$avg_weighted_severity_per_vehicle,
  na.rm = TRUE
)

risk_profiles_reporting <- risk_profiles_reporting |>
  mutate(
    risk_quadrant = case_when(
      frequency_share >= freq_median &
        avg_weighted_severity_per_vehicle >= severity_median ~ "High frequency / High severity",
      frequency_share >= freq_median &
        avg_weighted_severity_per_vehicle < severity_median ~ "High frequency / Low severity",
      frequency_share < freq_median &
        avg_weighted_severity_per_vehicle >= severity_median ~ "Low frequency / High severity",
      frequency_share < freq_median &
        avg_weighted_severity_per_vehicle < severity_median ~ "Low frequency / Low severity"
    )
  )

# ------------------------------------------------------------------------------
# Chart 1: Frequency vs severity scatter with quadrant overlay
# Shows the structure of the frequency x severity proxy across reporting profiles.
# ------------------------------------------------------------------------------

frequency_severity_quadrants_plot <- ggplot(
  risk_profiles_reporting,
  aes(
    x      = frequency_share,
    y      = avg_weighted_severity_per_vehicle,
    colour = risk_quadrant
  )
) +
  geom_point(alpha = 0.75, size = 3) +
  geom_vline(
    xintercept = freq_median,
    linetype   = "dashed",
    colour     = "grey50"
  ) +
  geom_hline(
    yintercept = severity_median,
    linetype   = "dashed",
    colour     = "grey50"
  ) +
  scale_colour_manual(
    values = c(
      "High frequency / High severity" = "firebrick",
      "High frequency / Low severity"  = "darkorange3",
      "Low frequency / High severity"  = "darkorchid1",
      "Low frequency / Low severity"   = "steelblue"
    )
  ) +
  labs(
    title    = "Frequency vs Severity by Vehicle Risk Profile",
    subtitle = "Each point represents one vehicle profile. Dashed lines show reporting medians.",
    x        = "Frequency share (share of all collision involvements)",
    y        = "Avg weighted severity per vehicle",
    colour   = NULL,
    caption  = reporting_caption
  ) +
  viz_theme

print(frequency_severity_quadrants_plot)
save_plot(
  frequency_severity_quadrants_plot,
  "01_frequency_severity_quadrants.png",
  height = 7
)

# ------------------------------------------------------------------------------
# Chart 2: Top 10 profiles by risk_proxy_score
# ------------------------------------------------------------------------------

top_10_risk_profiles <- risk_profiles_reporting |>
  arrange(risk_rank) |>
  slice_head(n = 10) |>
  mutate(
    profile_label = paste(
      vehicle_type_label,
      propulsion_label,
      engine_capacity_band,
      vehicle_age_band,
      sep = " | "
    ),
    profile_label = fct_reorder(profile_label, risk_proxy_score)
  )

top_10_risk_profiles_plot <- ggplot(
  top_10_risk_profiles,
  aes(x = profile_label, y = risk_proxy_score)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title    = "Top 10 Vehicle Profiles by Contribution Score",
    subtitle = "Ranked by risk proxy score (frequency share × average weighted severity), 2015–2024",
    x        = NULL,
    y        = "Risk proxy score",
    caption  = reporting_caption
  ) +
  viz_theme

print(top_10_risk_profiles_plot)
save_plot(top_10_risk_profiles_plot, "02_top_10_risk_profiles.png")

# ------------------------------------------------------------------------------
# Chart 3: Risk proxy score distribution
# ------------------------------------------------------------------------------

risk_proxy_distribution_plot <- ggplot(
  risk_profiles_reporting,
  aes(x = risk_proxy_score)
) +
  geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
  labs(
    title    = "Distribution of Risk Proxy Scores",
    subtitle = "Across all vehicle profiles in the reporting dataset, 2015–2024",
    x        = "Risk proxy score",
    y        = "Number of profiles",
    caption  = reporting_caption
  ) +
  viz_theme

print(risk_proxy_distribution_plot)
save_plot(
  risk_proxy_distribution_plot,
  "03_risk_proxy_distribution.png",
  width = 9
)

# ------------------------------------------------------------------------------
# Chart 4: Aggregate risk contribution by grouped vehicle type
# Harmonises STATS19 vehicle_type_label values into broader grouped vehicle types
# before aggregation, so categories are compared at a more consistent level than
# the source vehicle-type labels allow.
# ------------------------------------------------------------------------------

grouped_vehicle_type_risk_contribution <- risk_profiles_reporting |>
  mutate(
    grouped_vehicle_type_label = case_when(
      vehicle_type_label == "Car" ~ "Car",
      str_detect(vehicle_type_label, regex("Motorcycle", ignore_case = TRUE)) ~ "Motorcycle",
      vehicle_type_label %in% c(
        "Van / Goods 3.5 tonnes mgw or under",
        "Goods over 3.5t. and under 7.5t",
        "Goods 7.5 tonnes mgw and over"
      ) ~ "Van / goods vehicle",
      vehicle_type_label == "Bus or coach (17 or more pass seats)" ~ "Bus / coach",
      vehicle_type_label == "Taxi/Private hire car" ~ "Taxi / private hire",
      vehicle_type_label == "Agricultural vehicle" ~ "Agricultural vehicle",
      TRUE ~ "Other"
    )
  ) |>
  group_by(grouped_vehicle_type_label) |>
  summarise(
    total_risk_contribution = sum(risk_proxy_score, na.rm = TRUE),
    vehicle_count           = sum(vehicle_count, na.rm = TRUE),
    weighted_severity_total = sum(weighted_severity_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_risk_contribution)) |>
  mutate(
    avg_severity_per_vehicle = weighted_severity_total / vehicle_count,
    pct_total_risk = total_risk_contribution /
      sum(total_risk_contribution) * 100,
    grouped_vehicle_type_label = fct_reorder(
      grouped_vehicle_type_label,
      pct_total_risk
    )
  )

print(grouped_vehicle_type_risk_contribution)

grouped_vehicle_type_risk_contribution_plot <- ggplot(
  grouped_vehicle_type_risk_contribution,
  aes(x = grouped_vehicle_type_label, y = pct_total_risk)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title    = "Aggregate Risk Contribution by Grouped Vehicle Type",
    subtitle = "Percentage contribution to total risk proxy score, 2015–2024",
    x        = NULL,
    y        = "% of total risk proxy score",
    caption  = reporting_caption
  ) +
  viz_theme

print(grouped_vehicle_type_risk_contribution_plot)
save_plot(
  grouped_vehicle_type_risk_contribution_plot,
  "04_grouped_vehicle_type_risk_contribution.png",
  height = 7
)

# ------------------------------------------------------------------------------
# Chart 5: Vehicle count distribution with threshold marker
# ------------------------------------------------------------------------------

vehicle_count_distribution_plot <- ggplot(
  risk_profiles_reporting,
  aes(x = vehicle_count)
) +
  geom_histogram(bins = 84, fill = "steelblue", colour = "white") +
  geom_vline(
    xintercept = MIN_VEHICLE_COUNT,
    linetype   = "dashed",
    colour     = "firebrick"
  ) +
  annotate(
    "text",
    x      = MIN_VEHICLE_COUNT,
    y      = Inf,
    label  = paste("Threshold:", MIN_VEHICLE_COUNT),
    hjust  = -0.1,
    vjust  = 2,
    size   = 3.5,
    colour = "firebrick"
  ) +
  labs(
    title    = "Vehicle Count Distribution Across Profiles",
    subtitle = "Reporting dataset only. Dashed line shows minimum vehicle count threshold.",
    x        = "Vehicle count",
    y        = "Number of profiles",
    caption  = reporting_caption
  ) +
  viz_theme

print(vehicle_count_distribution_plot)
save_plot(
  vehicle_count_distribution_plot,
  "05_vehicle_count_distribution.png",
  width = 9
)

message("--- Reporting subset depth ---")
risk_profiles_reporting |>
  summarise(
    profiles        = n(),
    under_1000      = sum(vehicle_count < 1000),
    under_5000      = sum(vehicle_count < 5000),
    under_10000     = sum(vehicle_count < 10000),
    pct_under_1000  = round(under_1000 / profiles * 100, 1),
    pct_under_5000  = round(under_5000 / profiles * 100, 1),
    pct_under_10000 = round(under_10000 / profiles * 100, 1)
  ) |>
  print()

# ------------------------------------------------------------------------------
# Chart 6: Coverage summary
# Compares eligible vehicle-category profiles before thresholding, the scoped
# dataset, and the reporting subset to quantify the trade-off between
# interpretability and coverage.
# ------------------------------------------------------------------------------

eligible_vehicle_profiles <- risk_profiles |>
  filter(
    !vehicle_type %in% NON_PROFILABLE_TYPES
  )

total_vehicles_eligible <- sum(
  eligible_vehicle_profiles$vehicle_count,
  na.rm = TRUE
)

coverage_summary <- tibble(
  dataset = c(
    "All profilable\ngrouped profiles",
    "Profiles with\n≥ 500 vehicles",
    "Final reporting\nsubset"
  ),
  profiles = c(
    nrow(eligible_vehicle_profiles),
    nrow(risk_profiles_scoped),
    nrow(risk_profiles_reporting)
  ),
  vehicles = c(
    total_vehicles_eligible,
    sum(risk_profiles_scoped$vehicle_count, na.rm = TRUE),
    sum(risk_profiles_reporting$vehicle_count, na.rm = TRUE)
  ),
  pct_vehicles = c(
    100,
    round(
      sum(risk_profiles_scoped$vehicle_count, na.rm = TRUE) /
        total_vehicles_eligible * 100,
      1
    ),
    round(
      sum(risk_profiles_reporting$vehicle_count, na.rm = TRUE) /
        total_vehicles_eligible * 100,
      1
    )
  )
) |>
  mutate(dataset = fct_inorder(dataset))

print(coverage_summary)

coverage_summary_long <- coverage_summary |>
  pivot_longer(
    cols      = c(profiles, pct_vehicles),
    names_to  = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = recode(
      metric,
      profiles     = "Profiles retained",
      pct_vehicles = "Vehicle coverage (%)"
    )
  )

coverage_summary_plot <- ggplot(
  coverage_summary_long,
  aes(x = dataset, y = value, fill = dataset)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "All profilable\ngrouped profiles" = "grey70",
      "Profiles with\n≥ 500 vehicles"  = "steelblue",
      "Final reporting\nsubset"        = "darkorange3"
    )
  ) +
  labs(
    title    = "Dataset Coverage at Each Scoping Stage",
    subtitle = "Profiles retained and vehicle coverage after count thresholding and undefined-profile removal",
    x        = NULL,
    y        = NULL,
    caption  = "Source: UK STATS19, 2015–2024. Profilable total excludes vehicle types with no recorded characteristics (pedal cycles, horses, trams, mobility scooters)."
  ) +
  viz_theme

print(coverage_summary_plot)
save_plot(coverage_summary_plot, "06_coverage_summary.png")

# ------------------------------------------------------------------------------
# Close database connection
# ------------------------------------------------------------------------------

if (exists("con") && DBI::dbIsValid(con)) {
  dbDisconnect(con)
  message("DB connection closed.")
}