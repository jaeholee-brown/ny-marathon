# model 2: continuous exposure (0-17 years)
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: df_m2, qr_fit_cont, coef_cont, visualization

# map birth years to title ix exposure duration (0-17 years)
# - no = 0 years
# - some = 1-16 years (based on birth year)
# - yes = 17 years (full k-12 exposure)
df_m2 <- df %>%
  mutate(
    # recreate the base variable for logic mapping
    TitleIX_Cat = dplyr::case_when(
      Gender == "M" ~ "No",
      `Year of Birth` >= 1970 ~ "Yes",
      `Year of Birth` >= 1954 & `Year of Birth` <= 1969 ~ "Some",
      TRUE ~ "No"
    ),
    TitleIX_years = dplyr::case_when(
      TitleIX_Cat == "No" ~ 0,
      TitleIX_Cat == "Yes" ~ 17,
      TitleIX_Cat == "Some" ~ pmin(pmax(`Year of Birth` - 1953, 1), 16),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(TitleIX_years))

# fit quantile regression with continuous exposure variable
qr_fit_cont <- rq(
  FinishSeconds ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    TitleIX_years +
    Gender +
    bs(Age, knots = c(21, 45, 60)):Gender,
  tau = taus,
  data = df_m2,
  method = RQ_METHOD
)

# extract coefficients
coef_cont <- GetQrCoefs(qr_fit_cont, taus)
write.csv(coef_cont, "output/coef_m2_continuous.csv", row.names = FALSE)

# print only the relevant continuous term
coef_cont %>%
  filter(term == "TitleIX_years") %>%
  print()

# prepare plot data
coef_cont_titleix <- coef_cont %>%
  filter(term == "TitleIX_years")

# visualization: coefficients by quantile
p_m2 <- ggplot(
  coef_cont_titleix,
  aes(x = tau, y = estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.03,
    linewidth = 0.8
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = coef_cont_titleix$tau) +
  labs(
    x = "Quantile",
    y = "Seconds (s)",
    title = "Coefficients for TitleIX_years, by Quantile"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 20)
  )

print(p_m2)
ggsave("output/continuous_plot.png", p_m2, width = 10, height = 7, dpi = 300)

message("model 2 complete")
