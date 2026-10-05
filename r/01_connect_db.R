# ------------------------------------------------------------------------------
# 01_connect_db.R
#
# Purpose:
# - Establish a reusable connection to the vehicle_risk_profiling PostgreSQL
#   database using credentials stored in .Renviron at the project root.
# - Create a single connection object (con) to be used by all downstream scripts.
# - Call packages necessary for downstream functional programming.
#
# Design choices:
# - Credentials are read exclusively from .Renviron via Sys.getenv() so that no
#   sensitive values appear in committed code.
# - Connection is defined once here and sourced by downstream scripts rather than
#   each script managing its own connection, keeping credentials and driver
#   configuration in a single auditable location.
# - dbConnect() is called at source time so that a failed connection surfaces
#   immediately when a downstream script is run, before any analysis executes.
# - con is left open intentionally; each downstream script is responsible for
#   calling dbDisconnect(con) on exit.
# ------------------------------------------------------------------------------

library(DBI)
library(RPostgres)
library(tidyverse)

con <- dbConnect(
  drv      = Postgres(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)
