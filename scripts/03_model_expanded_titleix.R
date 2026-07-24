# expanded model: Title IX exposure (3-level factor)
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: df_m1, qr_fit_base, coef_base, and visualizations

if (!exists("MODEL_OUTPUT_DIR")) MODEL_OUTPUT_DIR <- "output"
dir.create(MODEL_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# create 3-level Title IX variable
# - no: men, or women born in 1953 or earlier
# - some: women born from 1954 through 1969 (partial exposure)
# - yes: women born in 1970 or later (full exposure)
df_m1 <- df %>%
  mutate(
    TitleIX = dplyr::case_when(
      Gender == "M" ~ "No",
      `Year of Birth` >= 1970 ~ "Yes",
      `Year of Birth` >= 1954 & `Year of Birth` <= 1969 ~ "Some",
      `Year of Birth` <= 1953 ~ "No",
      TRUE ~ NA_character_
    ),
    TitleIX = factor(TitleIX, levels = c("No", "Some", "Yes"))
  ) %>%
  filter(!is.na(TitleIX))

# fit quantile regression across all taus
qr_fit_base <- rq(
  FinishMinutes ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    TitleIX +
    Gender +
    bs(Age, knots = c(21, 45, 60)):Gender,
  tau = taus,
  data = df_m1,
  method = RQ_METHOD
)

# extract coefficients
coef_base <- GetQrCoefs(qr_fit_base, taus)
print(coef_base)
write.csv(
  coef_base,
  file.path(MODEL_OUTPUT_DIR, "coef_m1_base_titleix.csv"),
  row.names = FALSE
)

# prepare plot data for "some" and "yes" vs "no"
plot_data_base <- coef_base %>%
  filter(term %in% c("TitleIXSome", "TitleIXYes")) %>%
  mutate(
    label = fct_recode(
      term,
      "Partial vs No Exposure" = "TitleIXSome",
      "Full vs No Exposure" = "TitleIXYes"
    ),
    effect_min = estimate,
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error
  )

# visualization: ribbon plot
p_m1_ribbon <- ggplot(
  plot_data_base,
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
    title = "Estimated Title IX Effect (Expanded Model)"
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

if (interactive()) print(p_m1_ribbon)
ggsave(
  file.path(MODEL_OUTPUT_DIR, "m1_base_titleix.png"),
  p_m1_ribbon,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# visualization: error bar plot with custom colors
p_m1_errorbar <- ggplot(
  plot_data_base,
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
  scale_color_manual(
    values = c(
      "Partial vs No Exposure" = "goldenrod1",
      "Full vs No Exposure" = "forestgreen"
    )
  ) +
  labs(
    x = "Quantile",
    y = "Effect on finish time (minutes)",
    title = "Estimated Title IX Effect (Expanded Model)",
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

if (interactive()) print(p_m1_errorbar)
ggsave(
  file.path(MODEL_OUTPUT_DIR, "someyesplot.png"),
  p_m1_errorbar,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

message("expanded model complete")
