# sample summary: final sample size calculations
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: console summary of sample sizes

# calculate the total number of finishers
N_total <- nrow(df)

# calculate the number of women finishers
N_women <- df %>%
  filter(Gender == "W") %>%
  nrow()

# print the results
cat(
  "Final analytic sample: N = ", format(N_total, big.mark = ","),
  " finishes\nWomen: N = ", format(N_women, big.mark = ","),
  " (", round(100 * N_women / N_total, 1), "%)\n",
  sep = ""
)

message("\nsample summary complete")
