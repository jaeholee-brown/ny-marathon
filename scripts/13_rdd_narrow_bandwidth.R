# model 9: regression discontinuity with narrow bandwidth
#
# uses birth year as the running variable with cutoff at 1958
# (women born 1958 were 17 when title ix took effect in 1975)
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first

# -----------------------------------------------------------------------------
# 1. prepare data for RDD
# -----------------------------------------------------------------------------

# cutoff: women born >= 1958 were in school when title ix took effect
CUTOFF <- 1958
BANDWIDTH <- 5  # +/- 5 years from cutoff

# filter to women only (RDD doesn't need men as control)
df_rdd <- df %>%
  filter(
    Gender == "W",
    `Year of Birth` >= (CUTOFF - BANDWIDTH),
    `Year of Birth` <= (CUTOFF + BANDWIDTH)
  ) %>%
  mutate(
    # center the running variable at cutoff
    BirthYearCentered = `Year of Birth` - CUTOFF,
    # treatment indicator
    PostCutoff = as.integer(`Year of Birth` >= CUTOFF)
  )

message("RDD sample (bandwidth = +/- ", BANDWIDTH, " years): ", nrow(df_rdd))
message("birth year range: ", min(df_rdd$`Year of Birth`), " to ",
        max(df_rdd$`Year of Birth`))
message("pre-cutoff: ", sum(df_rdd$PostCutoff == 0),
        ", post-cutoff: ", sum(df_rdd$PostCutoff == 1))

# -----------------------------------------------------------------------------
# 2. visualize the discontinuity
# -----------------------------------------------------------------------------

# aggregate by birth year
rdd_summary <- df_rdd %>%
  group_by(`Year of Birth`) %>%
  summarize(
    mean_finish = mean(FinishSeconds, na.rm = TRUE),
    median_finish = median(FinishSeconds, na.rm = TRUE),
    p10_finish = quantile(FinishSeconds, 0.10, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    BirthYearCentered = `Year of Birth` - CUTOFF,
    PostCutoff = `Year of Birth` >= CUTOFF
  )

# plot mean finish times by birth year
p_rdd_mean <- ggplot(rdd_summary, aes(x = `Year of Birth`, y = mean_finish / 60)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_vline(xintercept = CUTOFF - 0.5, linetype = "dashed", color = "red") +
  geom_smooth(
    data = filter(rdd_summary, `Year of Birth` < CUTOFF),
    method = "lm", se = TRUE, color = "gray50"
  ) +
  geom_smooth(
    data = filter(rdd_summary, `Year of Birth` >= CUTOFF),
    method = "lm", se = TRUE, color = "steelblue"
  ) +
  annotate("text", x = CUTOFF, y = max(rdd_summary$mean_finish / 60) * 0.98,
           label = "Title IX\nCutoff", hjust = 0.5, color = "red", size = 3.5) +
  scale_size_continuous(name = "N runners", range = c(2, 8)) +
  labs(
    x = "Birth Year",
    y = "Mean Finish Time (minutes)",
    title = "RDD: Women's Mean Finish Time by Birth Year",
    subtitle = paste0("Bandwidth: ", CUTOFF - BANDWIDTH, "-", CUTOFF + BANDWIDTH)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

print(p_rdd_mean)
ggsave("output/rdd_mean_finish.png", p_rdd_mean, width = 10, height = 7, dpi = 300)

# plot 10th percentile (top performers)
p_rdd_p10 <- ggplot(rdd_summary, aes(x = `Year of Birth`, y = p10_finish / 60)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_vline(xintercept = CUTOFF - 0.5, linetype = "dashed", color = "red") +
  geom_smooth(
    data = filter(rdd_summary, `Year of Birth` < CUTOFF),
    method = "lm", se = TRUE, color = "gray50"
  ) +
  geom_smooth(
    data = filter(rdd_summary, `Year of Birth` >= CUTOFF),
    method = "lm", se = TRUE, color = "steelblue"
  ) +
  labs(
    x = "Birth Year",
    y = "10th Percentile Finish Time (minutes)",
    title = "RDD: Women's Top 10% Finish Time by Birth Year",
    subtitle = paste0("Bandwidth: ", CUTOFF - BANDWIDTH, "-", CUTOFF + BANDWIDTH)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(p_rdd_p10)
ggsave("output/rdd_p10_finish.png", p_rdd_p10, width = 10, height = 7, dpi = 300)

# -----------------------------------------------------------------------------
# 3. local linear RDD estimation
# -----------------------------------------------------------------------------

message("\n--- local linear RDD ---")

# model: allow different slopes on each side of cutoff
rdd_fit <- lm(
  FinishSeconds ~
    PostCutoff +
    BirthYearCentered +
    PostCutoff:BirthYearCentered +
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)),
  data = df_rdd
)

rdd_coefs <- summary(rdd_fit)$coefficients
discontinuity <- rdd_coefs["PostCutoff", ]

message("RDD estimate (discontinuity at cutoff):")
message("  estimate: ", round(discontinuity["Estimate"], 1), " seconds (",
        round(discontinuity["Estimate"] / 60, 2), " minutes)")
message("  std error: ", round(discontinuity["Std. Error"], 1))
message("  p-value: ", format(discontinuity["Pr(>|t|)"], digits = 4))
message("  interpretation: ",
        ifelse(discontinuity["Estimate"] < 0,
               "women born after cutoff are FASTER",
               "women born after cutoff are SLOWER"))

# -----------------------------------------------------------------------------
# 4. sensitivity to bandwidth choice
# -----------------------------------------------------------------------------

message("\n--- bandwidth sensitivity ---")

bandwidths <- c(3, 4, 5, 6, 7, 8, 10)
sensitivity <- tibble(
  bandwidth = integer(),
  estimate = numeric(),
  std_error = numeric(),
  p_value = numeric(),
  n_obs = integer()
)

for (bw in bandwidths) {
  df_bw <- df %>%
    filter(
      Gender == "W",
      `Year of Birth` >= (CUTOFF - bw),
      `Year of Birth` <= (CUTOFF + bw)
    ) %>%
    mutate(
      BirthYearCentered = `Year of Birth` - CUTOFF,
      PostCutoff = as.integer(`Year of Birth` >= CUTOFF)
    )

  fit_bw <- lm(
    FinishSeconds ~
      PostCutoff +
      BirthYearCentered +
      PostCutoff:BirthYearCentered +
      factor(Year) +
      bs(Age, knots = c(21, 45, 60)),
    data = df_bw
  )

  coefs_bw <- summary(fit_bw)$coefficients["PostCutoff", ]

  sensitivity <- bind_rows(sensitivity, tibble(
    bandwidth = bw,
    estimate = coefs_bw["Estimate"],
    std_error = coefs_bw["Std. Error"],
    p_value = coefs_bw["Pr(>|t|)"],
    n_obs = nrow(df_bw)
  ))
}

sensitivity <- sensitivity %>%
  mutate(
    lo = estimate - 1.96 * std_error,
    hi = estimate + 1.96 * std_error
  )

print(sensitivity)
write.csv(sensitivity, "output/rdd_bandwidth_sensitivity.csv", row.names = FALSE)

# plot sensitivity
p_sensitivity <- ggplot(sensitivity, aes(x = bandwidth, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3, linewidth = 0.8) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue", alpha = 0.5) +
  labs(
    x = "Bandwidth (+/- years from cutoff)",
    y = "RDD Estimate (seconds)",
    title = "Sensitivity of RDD Estimate to Bandwidth Choice",
    subtitle = "Negative = Title IX cohorts faster"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(p_sensitivity)
ggsave("output/rdd_bandwidth_sensitivity.png", p_sensitivity,
       width = 9, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# 5. placebo cutoffs (should show no discontinuity)
# -----------------------------------------------------------------------------

message("\n--- placebo cutoffs ---")

placebo_cutoffs <- c(1950, 1952, 1954, 1956, 1958, 1960, 1962, 1964, 1966)
placebo_rdd <- tibble(
  cutoff = integer(),
  estimate = numeric(),
  std_error = numeric(),
  p_value = numeric()
)

for (cut in placebo_cutoffs) {
  df_placebo <- df %>%
    filter(
      Gender == "W",
      `Year of Birth` >= (cut - BANDWIDTH),
      `Year of Birth` <= (cut + BANDWIDTH)
    ) %>%
    mutate(
      BirthYearCentered = `Year of Birth` - cut,
      PostCutoff = as.integer(`Year of Birth` >= cut)
    )

  if (nrow(df_placebo) < 100) next

  fit_placebo <- lm(
    FinishSeconds ~
      PostCutoff +
      BirthYearCentered +
      PostCutoff:BirthYearCentered +
      factor(Year) +
      bs(Age, knots = c(21, 45, 60)),
    data = df_placebo
  )

  coefs_placebo <- summary(fit_placebo)$coefficients["PostCutoff", ]

  placebo_rdd <- bind_rows(placebo_rdd, tibble(
    cutoff = cut,
    estimate = coefs_placebo["Estimate"],
    std_error = coefs_placebo["Std. Error"],
    p_value = coefs_placebo["Pr(>|t|)"]
  ))
}

placebo_rdd <- placebo_rdd %>%
  mutate(
    is_real = cutoff == 1958,
    lo = estimate - 1.96 * std_error,
    hi = estimate + 1.96 * std_error
  )

print(placebo_rdd)
write.csv(placebo_rdd, "output/rdd_placebo_cutoffs.csv", row.names = FALSE)

# plot placebo cutoffs
p_placebo_rdd <- ggplot(placebo_rdd, aes(x = factor(cutoff), y = estimate,
                                          color = is_real)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("FALSE" = "gray50", "TRUE" = "red"),
    labels = c("Placebo", "Real (1958)"),
    name = NULL
  ) +
  labs(
    x = "Cutoff Birth Year",
    y = "RDD Estimate (seconds)",
    title = "RDD Placebo Test: Estimates at Different Cutoffs",
    subtitle = "Only 1958 should show significant discontinuity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

print(p_placebo_rdd)
ggsave("output/rdd_placebo_cutoffs.png", p_placebo_rdd,
       width = 10, height = 7, dpi = 300)

message("\nmodel 9 (RDD narrow bandwidth) complete")
