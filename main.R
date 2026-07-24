# main: reproduce the primary analyses and figures
#
# run from the repository root with:
# Rscript main.R

# record start time
start_time <- Sys.time()
MODEL_OUTPUT_DIR <- "output"

message("=== starting NYC Marathon Title IX analysis ===")
message("timestamp: ", start_time)

# setup: libraries, parallel backend, helper functions
message("\n--- sourcing setup ---")
t0 <- Sys.time()
source("scripts/00_setup.R")
message("setup time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# data cleaning: load and prepare the marathon dataset
message("\n--- sourcing data cleaning ---")
t0 <- Sys.time()
source("scripts/01_data_cleaning.R")
message("data cleaning time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# baseline model: no versus yes (2-level factor)
message("\n--- sourcing baseline model: no versus yes ---")
t0 <- Sys.time()
source("scripts/02_model_simple_base.R")
message("baseline model time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# expanded model: no versus partial versus full (3-level factor)
message("\n--- sourcing expanded model: no, partial, or full exposure ---")
t0 <- Sys.time()
source("scripts/03_model_expanded_titleix.R")
message("expanded model time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# descriptive visualizations
message("\n--- sourcing descriptive figures ---")
t0 <- Sys.time()
source("scripts/09_participation_trends.R")
message("descriptive figure time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# sample summary: final sample sizes
message("\n--- sourcing sample summary ---")
t0 <- Sys.time()
source("scripts/10_sample_summary.R")
message("sample summary time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s")

# teardown: reset parallel backend
message("\n--- cleanup ---")
plan(sequential)
writeLines(capture.output(sessionInfo()), "output/session_info.txt")

# record end time and duration
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

message("\n=== analysis complete ===")
message("total duration: ", round(duration, 2), " minutes")
message("rq method used: ", RQ_METHOD)
