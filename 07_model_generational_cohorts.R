# model 6: generational cohorts (4-level factor)
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: df_m6, qr_fit_4, coef_4, visualization

# categorize by broad generational cohorts
# - none: males or females born <= 1953
# - early: females born 1954-1969 (partial exposure)
# - middle: females born 1970-1988 (full exposure, early adopters)
# - late: females born >= 1989 (full exposure, later generation)
df_m6 <- df %>%
  mutate(
    TitleIX_4 = dplyr::case_when(
      Gender == "M" ~ "None",
      `Year of Birth` <= 1953 ~ "None",
      `Year of Birth` >= 1954 & `Year of Birth` <= 1969 ~ "Early",
      `Year of Birth` >= 1970 & `Year of Birth` <= 1988 ~ "Middle",
      `Year of Birth` >= 1989 ~ "Late",
      TRUE ~ NA_character_
    ),
    TitleIX_4 = factor(
      TitleIX_4,
      levels = c("None", "Early", "Middle", "Late")
    )
  ) %>%
  filter(!is.na(TitleIX_4))

# fit quantile regression with 4-level generational factor
qr_fit_4 <- rq(
  FinishSeconds ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    TitleIX_4 +
    Gender +
    bs(Age, knots = c(21, 45, 60)):Gender,
  tau = taus,
  data = df_m6,
  method = RQ_METHOD
)

# extract coefficients
coef_4 <- GetQrCoefs(qr_fit_4, taus)
print(coef_4)

# prepare plot data
plot_data_4 <- coef_4 %>%
  filter(grepl("^TitleIX_4", term)) %>%
  mutate(
    label = str_remove(term, "^TitleIX_4"),
    effect_min = estimate / 60,
    lo = (estimate - 1.96 * std.error) / 60,
    hi = (estimate + 1.96 * std.error) / 60
  )

# visualization: effects by generation
p_m6 <- ggplot(
  plot_data_4,
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
    color = "Cohort",
    fill = "Cohort",
    title = "Title IX effect by generation"
  ) +
  theme_minimal(base_size = 12)

print(p_m6)
ggsave("m6_generational.png", p_m6, width = 10, height = 6, dpi = 300)

message("model 6 complete")
