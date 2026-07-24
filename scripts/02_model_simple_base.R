# baseline model: simple Title IX exposure (2-level factor)
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: df_m0, qr_fit_simp, coef_simp, and visualizations

if (!exists("MODEL_OUTPUT_DIR")) MODEL_OUTPUT_DIR <- "output"
dir.create(MODEL_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# create 2-level Title IX variable
# - no: men, or women born in 1953 or earlier
# - yes: women born in 1954 or later
df_m0 <- df %>%
  mutate(
    TitleIX = dplyr::case_when(
      Gender == "M" ~ "No",
      `Year of Birth` >= 1954 ~ "Yes",
      `Year of Birth` <= 1953 ~ "No",
      TRUE ~ NA_character_
    ),
    TitleIX = factor(TitleIX, levels = c("No", "Yes"))
  ) %>%
  filter(!is.na(TitleIX))

# fit quantile regression across all taus
qr_fit_simp <- rq(
  FinishMinutes ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    TitleIX +
    Gender +
    bs(Age, knots = c(21, 45, 60)):Gender,
  tau = taus,
  data = df_m0,
  method = RQ_METHOD
)

# extract coefficients
coef_simp <- GetQrCoefs(qr_fit_simp, taus)
print(coef_simp)
write.csv(
  coef_simp,
  file.path(MODEL_OUTPUT_DIR, "coef_m0_simp_base.csv"),
  row.names = FALSE
)

# prepare plot data for exposure versus no exposure
plot_data_simp <- coef_simp %>%
  filter(term == "TitleIXYes") %>%
  mutate(
    label = fct_recode(
      term,
      "Title IX Exposure" = "TitleIXYes"
    ),
    effect_min = estimate,
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error
  )

# visualization: ribbon plot
p_m0_ribbon <- ggplot(
  plot_data_simp,
  aes(x = tau, y = effect_min, color = label, group = label)
) +
  geom_ribbon(
    aes(ymin = lo, ymax = hi, fill = label),
    alpha = 0.20,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(name = "Title IX status") +
  scale_fill_discrete(name = "Title IX status") +
  labs(
    x = "Quantile",
    y = "Effect on finish time (minutes)",
    title = "Estimated Title IX Effect (Baseline Model)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 20)
  )

if (interactive()) print(p_m0_ribbon)
ggsave(
  file.path(MODEL_OUTPUT_DIR, "m0_simp_base.png"),
  p_m0_ribbon,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# visualization: error-bar plot
p_m0_errorbar <- ggplot(
  plot_data_simp,
  aes(x = tau, y = effect_min, color = label, group = label)
) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    width = 0.02,
    linewidth = 0.8
  ) +
  geom_point(size = 1.8) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Title IX Exposure" = "forestgreen")) +
  labs(
    x = "Quantile",
    y = "Effect on finish time (minutes)",
    title = "Estimated Title IX Effect (Baseline Model)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(1.07, 1.45),
    legend.background = element_rect(fill = "white", color = "black")
  )

if (interactive()) print(p_m0_errorbar)
ggsave(
  file.path(MODEL_OUTPUT_DIR, "base_model_plot.png"),
  p_m0_errorbar,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

message("baseline model complete")
