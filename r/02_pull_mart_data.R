# ------------------------------------------------------------------------------
# 02_pull_mart_data.R
#
# Purpose:
# - Pull the analysis-ready mart table from PostgreSQL into R.
# - Expose a single clean dataframe (risk_profiles) for use by all downstream
#   analysis and validation scripts.
# - Confirm successful data transfer with a brief structural summary.
#
# Design choices:
# - Only mart.vehicle_risk_profiles_2015_2024 is pulled at this stage. This is
#   the final business-grain table and the primary input for all R analysis.
#   Supporting mart tables are available but will only be pulled if a specific
#   analytical need arises in later scripts.
# - dbGetQuery() is used in preference to dbReadTable() to make the source
#   schema and table explicit, avoiding ambiguity if multiple schemas exist.
# - No transformation, filtering, or column renaming is applied here. The
#   dataframe received in R should be a faithful reflection of the mart table.
#   All analytical decisions are deferred to downstream scripts.
# - A structural summary (glimpse + row/column count) is printed on load as a
#   lightweight confirmation that the pull succeeded and the shape is as expected.
#   This is not a substitute for the formal validation in 03_validate_transfer.R.
# ------------------------------------------------------------------------------

source("r/01_connect_db.R")

risk_profiles <- dbGetQuery(con, "
  SELECT *
  FROM mart.vehicle_risk_profiles_2015_2024
")

# Structural confirmation
message("Rows pulled: ", nrow(risk_profiles))
message("Columns:     ", ncol(risk_profiles))
glimpse(risk_profiles)
