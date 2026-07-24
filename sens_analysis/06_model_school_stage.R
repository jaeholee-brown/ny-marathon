# model 5: school stage (5-level factor)
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: df_m5, qr_fit_5, coef_5, visualization

# categorize by school stage during title ix implementation
# - none: males or females born <= 1953
# - elementary: females born 1962-1969 (in elementary school in 1972)
# - middle: females born 1959-1961
# - high: females born 1954-1958
# - yes: females born >= 1970 (full k-12 exposure)
df_m5 <- df %>%
  mutate(
    TitleIX_5 = dplyr::case_when(
      Gender == "M" ~ "None",
      `Year of Birth` <= 1953 ~ "None",
      `Year of Birth` >= 1954 & `Year of Birth` <= 1958 ~ "High",
      `Year of Birth` >= 1959 & `Year of Birth` <= 1961 ~ "Middle",
      `Year of Birth` >= 1962 & `Year of Birth` <= 1969 ~ "Elementary",
      `Year of Birth` >= 1970 ~ "Yes",
      TRUE ~ NA_character_
    ),
    TitleIX_5 = factor(
      TitleIX_5,
      levels = c("None", "Elementary", "Middle", "High", "Yes")
    )
  ) %>%
  filter(!is.na(TitleIX_5))

# fit quantile regression with 5-level school stage factor
qr_fit_5 <- rq(
  FinishMinutes ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    TitleIX_5 +
    Gender +
    bs(Age, knots = c(21, 45, 60)):Gender,
  tau = taus,
  data = df_m5,
  method = RQ_METHOD
)

# extract coefficients
coef_5 <- GetQrCoefs(qr_fit_5, taus)
print(coef_5)
write.csv(
  coef_5,
  "sens_analysis/output/coef_m5_school_stage.csv",
  row.names = FALSE
)

# prepare plot data
plot_data_5 <- coef_5 %>%
  filter(grepl("^TitleIX_5", term)) %>%
  mutate(
    label = str_remove(term, "^TitleIX_5"),
    effect_min = estimate,
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error
  )

# visualization: effects by school stage
p_m5 <- ggplot(
  plot_data_5,
  aes(x = tau, y = effect_min, color = label, group = label)
) +
  geom_ribbon(
    aes(ymin = lo, ymax = hi, fill = label),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Quantile (tau)",
    y = "Effect vs None (minutes)",
    color = "School Stage",
    fill = "School Stage",
    title = "Title IX effect by school stage"
  ) +
  theme_minimal(base_size = 12)

if (interactive()) print(p_m5)
ggsave(
  "sens_analysis/output/m5_school_stage.png",
  p_m5,
  width = 10,
  height = 6,
  dpi = 300
)

message("model 5 complete")
