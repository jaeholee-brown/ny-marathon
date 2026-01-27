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
  "Final analytic sample: N=", N_total,
  "\nN=", N_total, " finishers, of which NW=", N_women,
  "\nNW = ", N_women, " women."
)

message("\nsample summary complete")
