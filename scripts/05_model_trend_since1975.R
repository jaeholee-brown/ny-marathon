# model 4: continuous trend since 1975
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: df_m4, qr_fit_since1975, coef_since1975

# create trend variable:
# - 0 for men or pre-1975
# - (year - 1975) for women post-1975
# this captures time-varying title ix exposure effect for women only
df_m4 <- df %>%
  mutate(
    TitleIX_since1975 = dplyr::case_when(
      Gender == "W" & Year >= 1975 ~ Year - 1975,
      TRUE ~ 0
    )
  )

# fit quantile regression with trend variable
qr_fit_since1975 <- rq(
  FinishSeconds ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    TitleIX_since1975 +
    Gender +
    bs(Age, knots = c(21, 45, 60)):Gender,
  tau = taus,
  data = df_m4,
  method = RQ_METHOD
)

# extract coefficients
coef_since1975 <- GetQrCoefs(qr_fit_since1975, taus)
write.csv(coef_since1975, "output/coef_m4_trend.csv", row.names = FALSE)

# print only the relevant trend term
coef_since1975 %>%
  filter(term == "TitleIX_since1975") %>%
  print()

message("model 4 complete")
