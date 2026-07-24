# model 3: 18-category discrete (no, some1-16, yes)
#
# requires: 00_setup.R, 01_data_cleaning.R, and
# sens_analysis/03_model_continuous_exposure.R
# outputs: df_m3, qr_fit_18, coef_18, visualization

# create granular factor levels for every year of exposure
# uses df_m2 which already has TitleIX_years calculated
df_m3 <- df_m2 %>%
  mutate(
    TitleIX_years_disc = as.integer(TitleIX_years),
    TitleIX_18 = dplyr::case_when(
      TitleIX_years_disc == 0L ~ "No",
      TitleIX_years_disc == 17L ~ "Yes",
      TitleIX_years_disc >= 1L & TitleIX_years_disc <= 16L ~
        paste0("Some", TitleIX_years_disc),
      TRUE ~ NA_character_
    ),
    # ensure correct ordering: no -> some1...some16 -> yes
    TitleIX_18 = factor(
      TitleIX_18,
      levels = c("No", paste0("Some", 1:16), "Yes")
    )
  ) %>%
  filter(!is.na(TitleIX_18))

# fit quantile regression with 18-level factor
qr_fit_18 <- rq(
  FinishMinutes ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    TitleIX_18 +
    Gender +
    bs(Age, knots = c(21, 45, 60)):Gender,
  tau = taus,
  data = df_m3,
  method = RQ_METHOD
)

# extract coefficients
coef_18 <- GetQrCoefs(qr_fit_18, taus)
print(coef_18)
write.csv(
  coef_18,
  "sens_analysis/output/coef_m3_18category.csv",
  row.names = FALSE
)

# prepare plot data: effects by exposure year
plot_data_18 <- coef_18 %>%
  filter(grepl("^TitleIX_18Some|TitleIX_18Yes", term)) %>%
  mutate(
    exposure = dplyr::case_when(
      grepl("Some", term) ~ as.integer(str_remove(term, "^TitleIX_18Some")),
      grepl("Yes", term) ~ 17L
    ),
    effect_min = estimate
  )

# visualization: effects by exposure duration
p_m3 <- ggplot(
  plot_data_18,
  aes(x = exposure, y = effect_min, color = factor(tau))
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = 1:17,
    labels = c(as.character(1:16), "Yes")
  ) +
  labs(
    x = "Exposure (Years)",
    y = "Effect vs No (minutes)",
    color = "Quantile",
    title = "Title IX effect by exposure duration"
  ) +
  theme_minimal(base_size = 12)

if (interactive()) print(p_m3)
ggsave(
  "sens_analysis/output/m3_18category.png",
  p_m3,
  width = 10,
  height = 6,
  dpi = 300
)

message("model 3 complete")
