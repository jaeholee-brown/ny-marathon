# model 7: difference-in-differences with event study
#
# this script implements a formal causal inference framework for title ix:
# - difference-in-differences using men as control group
# - event study visualization by birth cohort
# - pre-treatment parallel trends test
# - placebo tests at fake cutoff years
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: did results, event study plot, parallel trends test

# -----------------------------------------------------------------------------
# 1. prepare data with birth cohort bins
# -----------------------------------------------------------------------------

# define birth cohort bins (5-year bins for cleaner visualization)
# reference cohort: 1950-1954 (pre-title ix, women in this cohort were 21-25 in 1975)
df_did <- df %>%
  mutate(
    BirthCohort = cut(
      `Year of Birth`,
      breaks = c(-Inf, 1939, 1944, 1949, 1954, 1959, 1964, 1969, 1974, 1979, 1984, 1989, Inf),
      labels = c("<=1939", "1940-44", "1945-49", "1950-54", "1955-59",
                 "1960-64", "1965-69", "1970-74", "1975-79", "1980-84",
                 "1985-89", ">=1990"),
      right = TRUE
    ),
    # binary post-treatment indicator (born >= 1958 means in school when title ix took effect)
    PostTitleIX = as.integer(`Year of Birth` >= 1958),
    # female indicator
    Female = as.integer(Gender == "W"),
    # DiD interaction term
    DiD = Female * PostTitleIX
  ) %>%
  filter(!is.na(BirthCohort))

message("DiD sample size: ", nrow(df_did), " observations")
message("birth cohort distribution:")
print(table(df_did$BirthCohort, df_did$Gender))

# -----------------------------------------------------------------------------
# 2. basic difference-in-differences model
# -----------------------------------------------------------------------------

message("\n--- fitting basic DiD model ---")

# DiD model: FinishSeconds ~ Female + PostTitleIX + Female:PostTitleIX + controls
# the coefficient on Female:PostTitleIX (DiD) is the treatment effect
qr_did <- rq(
  FinishSeconds ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    Female +
    PostTitleIX +
    DiD,
  tau = taus,
  data = df_did,
  method = RQ_METHOD
)

coef_did <- GetQrCoefs(qr_did, taus)
print(coef_did %>% filter(term %in% c("Female", "PostTitleIX", "DiD")))
write.csv(coef_did, "output/coef_m7_did.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. event study: cohort-specific gender gaps
# -----------------------------------------------------------------------------

message("\n--- fitting event study model ---")

# set reference cohort (1950-54: pre-title ix)
df_did <- df_did %>%
  mutate(BirthCohort = relevel(BirthCohort, ref = "1950-54"))

# event study model: interact gender with each birth cohort
# this estimates the gender gap for each cohort relative to the reference
qr_event <- rq(
  FinishSeconds ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    Gender * BirthCohort,
  tau = 0.5,  # focus on median for cleaner visualization
  data = df_did,
  method = RQ_METHOD
)

# extract coefficients
coef_event <- GetQrCoefs(qr_event, 0.5)

# get the interaction terms (gender gap relative to reference cohort)
event_study_data <- coef_event %>%
  filter(grepl("^GenderM:BirthCohort", term)) %>%
  mutate(
    cohort = gsub("GenderM:BirthCohort", "", term),
    cohort = factor(cohort, levels = c("<=1939", "1940-44", "1945-49",
                                        "1955-59", "1960-64", "1965-69",
                                        "1970-74", "1975-79", "1980-84",
                                        "1985-89", ">=1990")),
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error,
    # title ix exposure: born >= 1958 had some exposure in school
    title_ix_exposed = cohort %in% c("1960-64", "1965-69", "1970-74",
                                      "1975-79", "1980-84", "1985-89", ">=1990")
  ) %>%
  filter(!is.na(cohort))

# add the reference cohort (1950-54) with zero effect
reference_row <- tibble(
  term = "GenderM:BirthCohort1950-54",
  tau = 0.5,
  estimate = 0,
  std.error = 0,
  p.value = NA,
  cohort = factor("1950-54", levels = levels(event_study_data$cohort)),
  lo = 0,
  hi = 0,
  title_ix_exposed = FALSE
)

# need to add 1950-54 to factor levels
all_cohorts <- c("<=1939", "1940-44", "1945-49", "1950-54", "1955-59",
                 "1960-64", "1965-69", "1970-74", "1975-79", "1980-84",
                 "1985-89", ">=1990")

event_study_data <- event_study_data %>%
  mutate(cohort = factor(as.character(cohort), levels = all_cohorts))

reference_row <- reference_row %>%
  mutate(cohort = factor("1950-54", levels = all_cohorts))

event_study_data <- bind_rows(event_study_data, reference_row) %>%
  arrange(cohort)

write.csv(event_study_data, "output/event_study_coefficients.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 4. event study visualization
# -----------------------------------------------------------------------------

message("\n--- creating event study plot ---")

# vertical line at the cutoff (between 1955-59 and 1960-64)
# women born 1958+ were in school when title ix took effect in 1975

p_event_study <- ggplot(
  event_study_data,
  aes(x = cohort, y = estimate, color = title_ix_exposed)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 5.5, linetype = "dotted", color = "red", linewidth = 1) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), linewidth = 0.8, alpha = 0.5) +
  scale_color_manual(
    values = c("FALSE" = "gray40", "TRUE" = "steelblue"),
    labels = c("FALSE" = "Pre-Title IX", "TRUE" = "Title IX Exposed"),
    name = "Cohort Status"
  ) +
  annotate(
    "text", x = 5.5, y = max(event_study_data$hi, na.rm = TRUE) * 0.9,
    label = "Title IX\nTakes Effect", hjust = 0.5, vjust = 0, color = "red", size = 3.5
  ) +
  labs(
    x = "Birth Cohort",
    y = "Change in Gender Gap (seconds)\n(relative to 1950-54 cohort)",
    title = "Event Study: Gender Gap by Birth Cohort",
    subtitle = "Negative values = women improving relative to men"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_event_study)
ggsave("output/event_study_plot.png", p_event_study, width = 12, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# 5. parallel trends test (pre-treatment cohorts)
# -----------------------------------------------------------------------------

message("\n--- testing parallel trends assumption ---")

# test whether pre-treatment cohorts show a trend
# if parallel trends holds, pre-treatment cohorts should have similar gender gaps
pre_treatment_data <- event_study_data %>%
  filter(cohort %in% c("<=1939", "1940-44", "1945-49", "1950-54", "1955-59")) %>%
  mutate(cohort_num = as.numeric(cohort))

# simple linear test: is there a trend in gender gap before treatment?
if (nrow(pre_treatment_data) > 2) {
  pre_trend_test <- lm(estimate ~ cohort_num, data = pre_treatment_data)
  pre_trend_summary <- summary(pre_trend_test)

  message("pre-treatment trend test:")
  message("  slope coefficient: ", round(coef(pre_trend_test)[2], 2))
  message("  p-value: ", round(pre_trend_summary$coefficients[2, 4], 4))
  message("  interpretation: ",
          ifelse(pre_trend_summary$coefficients[2, 4] > 0.05,
                 "no significant pre-trend (parallel trends supported)",
                 "WARNING: significant pre-trend detected"))
}

# -----------------------------------------------------------------------------
# 6. placebo tests at fake cutoff years
# -----------------------------------------------------------------------------

message("\n--- running placebo tests ---")

# test fake cutoffs at 1948 and 1963 (should show no effect)
placebo_results <- tibble(
  cutoff_year = integer(),
  did_estimate = numeric(),
  std_error = numeric(),
  p_value = numeric()
)

for (fake_cutoff in c(1948, 1953, 1963, 1968)) {
  df_placebo <- df_did %>%
    mutate(
      FakeTreatment = as.integer(`Year of Birth` >= fake_cutoff),
      FakeDiD = Female * FakeTreatment
    )

  qr_placebo <- rq(
    FinishSeconds ~
      factor(Year) +
      bs(Age, knots = c(21, 45, 60)) +
      Female +
      FakeTreatment +
      FakeDiD,
    tau = 0.5,
    data = df_placebo,
    method = RQ_METHOD
  )

  coef_placebo <- GetQrCoefs(qr_placebo, 0.5)
  did_coef <- coef_placebo %>% filter(term == "FakeDiD")

  placebo_results <- bind_rows(
    placebo_results,
    tibble(
      cutoff_year = fake_cutoff,
      did_estimate = did_coef$estimate,
      std_error = did_coef$std.error,
      p_value = did_coef$p.value
    )
  )
}

# add the real cutoff (1958)
real_did <- coef_did %>% filter(term == "DiD", tau == 0.5)
placebo_results <- bind_rows(
  placebo_results,
  tibble(
    cutoff_year = 1958,
    did_estimate = real_did$estimate,
    std_error = real_did$std.error,
    p_value = real_did$p.value
  )
) %>%
  arrange(cutoff_year) %>%
  mutate(
    is_real = cutoff_year == 1958,
    lo = did_estimate - 1.96 * std_error,
    hi = did_estimate + 1.96 * std_error
  )

print(placebo_results)
write.csv(placebo_results, "output/placebo_test_results.csv", row.names = FALSE)

# placebo visualization
p_placebo <- ggplot(
  placebo_results,
  aes(x = factor(cutoff_year), y = did_estimate, color = is_real)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 0.8) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("FALSE" = "gray50", "TRUE" = "red"),
    labels = c("FALSE" = "Placebo Cutoff", "TRUE" = "Real Cutoff (1958)"),
    name = NULL
  ) +
  labs(
    x = "Cutoff Year (Birth Year)",
    y = "DiD Estimate (seconds)",
    title = "Placebo Tests: DiD Estimates at Different Cutoffs",
    subtitle = "Only the real cutoff (1958) should show significant effect"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_placebo)
ggsave("output/placebo_tests_plot.png", p_placebo, width = 10, height = 7, dpi = 300)

# -----------------------------------------------------------------------------
# 7. summary statistics
# -----------------------------------------------------------------------------

message("\n--- DiD summary ---")

did_summary <- coef_did %>%
  filter(term == "DiD") %>%
  mutate(
    effect_minutes = estimate / 60,
    lo_minutes = (estimate - 1.96 * std.error) / 60,
    hi_minutes = (estimate + 1.96 * std.error) / 60
  )

message("DiD estimates (Title IX effect on women's finish times):")
message("  negative values = women improved relative to men")
for (i in seq_len(nrow(did_summary))) {
  row <- did_summary[i, ]
  message(sprintf(
    "  tau = %.2f: %.1f seconds (%.1f to %.1f), p = %.4f",
    row$tau, row$estimate, row$estimate - 1.96 * row$std.error,
    row$estimate + 1.96 * row$std.error, row$p.value
  ))
}

message("\nmodel 7 (DiD event study) complete")
