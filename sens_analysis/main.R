# sensitivity analysis entry point
#
# run from the repository root with:
# Rscript sens_analysis/main.R

start_time <- Sys.time()
MODEL_OUTPUT_DIR <- "sens_analysis/output"
message("=== starting sensitivity analyses ===")

source("scripts/00_setup.R")
source("scripts/01_data_cleaning.R")
source("scripts/03_model_expanded_titleix.R")
source("sens_analysis/03_model_continuous_exposure.R")
source("sens_analysis/04_model_18category_discrete.R")
source("sens_analysis/05_model_trend_since1975.R")
source("sens_analysis/06_model_school_stage.R")
source("sens_analysis/07_model_generational_cohorts.R")
source("sens_analysis/08_cross_validation.R")

plan(sequential)

duration <- difftime(Sys.time(), start_time, units = "mins")
message("=== sensitivity analyses complete ===")
message("total duration: ", round(duration, 2), " minutes")
