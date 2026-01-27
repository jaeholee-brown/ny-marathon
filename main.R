# main: orchestrates all analysis scripts
#
# this script sources all component scripts in sequence to reproduce
# the complete analysis pipeline
#
# performance optimizations applied:
# - method = "fn" (frisch-newton) for reliable multi-tau fitting
# - cross-validation parallelized across all 25 tau-fold combinations
# - CV uses "pfn" method for 3x faster individual fits

# record start time
start_time <- Sys.time()

message("=== starting ny marathon analysis ===")
message("timestamp: ", start_time)

# setup: libraries, parallel backend, helper functions
message("\n--- sourcing setup ---")
t0 <- Sys.time()
source("00_setup.R")
message("setup time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# data cleaning: load and prepare the marathon dataset
message("\n--- sourcing data cleaning ---")
t0 <- Sys.time()
source("01_data_cleaning.R")
message("data cleaning time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# model 1: base title ix (3-level factor)
message("\n--- sourcing model 1: base title ix ---")
t0 <- Sys.time()
source("02_model_base_titleix.R")
message("model 1 time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# model 2: continuous exposure (0-17 years)
message("\n--- sourcing model 2: continuous exposure ---")
t0 <- Sys.time()
source("03_model_continuous_exposure.R")
message("model 2 time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# model 3: 18-category discrete
# note: depends on df_m2 from model 2
message("\n--- sourcing model 3: 18-category discrete ---")
t0 <- Sys.time()
source("04_model_18category_discrete.R")
message("model 3 time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# model 4: continuous trend since 1975
message("\n--- sourcing model 4: trend since 1975 ---")
t0 <- Sys.time()
source("05_model_trend_since1975.R")
message("model 4 time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# model 5: school stage (5-level factor)
message("\n--- sourcing model 5: school stage ---")
t0 <- Sys.time()
source("06_model_school_stage.R")
message("model 5 time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# model 6: generational cohorts (4-level factor)
message("\n--- sourcing model 6: generational cohorts ---")
t0 <- Sys.time()
source("07_model_generational_cohorts.R")
message("model 6 time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# cross-validation: compare all models
message("\n--- sourcing cross-validation ---")
t0 <- Sys.time()
source("08_cross_validation.R")
message("cross-validation time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# participation trends: descriptive visualization
message("\n--- sourcing participation trends ---")
t0 <- Sys.time()
source("09_participation_trends.R")
message("participation trends time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# sample summary: final sample sizes
message("\n--- sourcing sample summary ---")
t0 <- Sys.time()
source("10_sample_summary.R")
message("sample summary time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# teardown: reset parallel backend
message("\n--- cleanup ---")
plan(sequential)

# record end time and duration
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

message("\n=== analysis complete ===")
message("total duration: ", round(duration, 2), " minutes")
message("rq method used: ", RQ_METHOD)
message("parallel workers: ", workers)
