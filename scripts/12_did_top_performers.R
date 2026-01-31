# model 8: DiD on top performers
#
# compares top n% of female runners vs top n% of male runners
# within each year to reduce composition/selection effects
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first

# -----------------------------------------------------------------------------
# 1. create dataset of top performers by gender and year
# -----------------------------------------------------------------------------

TOP_PCT <- 0.05  # top 5% within each gender-year group

df_top <- df %>%
  group_by(Year, Gender) %>%
  mutate(
    pct_rank = percent_rank(FinishSeconds),  # 0 = fastest, 1 = slowest
    is_top = pct_rank <= TOP_PCT
  ) %>%
  filter(is_top) %>%
  ungroup()

message("top ", TOP_PCT * 100, "% sample: ", nrow(df_top), " observations")
message("by gender: ")
print(table(df_top$Gender))

# -----------------------------------------------------------------------------
# 2. compute yearly averages for top performers by gender
# -----------------------------------------------------------------------------

yearly_avg <- df_top %>%
  group_by(Year, Gender) %>%
  summarize(
    mean_finish = mean(FinishSeconds, na.rm = TRUE),
    median_finish = median(FinishSeconds, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Gender,
    values_from = c(mean_finish, median_finish, n),
    names_sep = "_"
  ) %>%
  mutate(
    gender_gap_mean = mean_finish_W - mean_finish_M,
    gender_gap_median = median_finish_W - median_finish_M
  )

write.csv(yearly_avg, "output/yearly_top_performers.csv", row.names = FALSE)

# plot gender gap over time
p_gap_time <- ggplot(yearly_avg, aes(x = Year, y = gender_gap_mean / 60)) +
  geom_point(size = 2) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +
  annotate("text", x = 1976, y = max(yearly_avg$gender_gap_mean / 60) * 0.95,
           label = "Title IX\nEffective", hjust = 0, color = "red", size = 3) +
  labs(
    x = "Year",
    y = "Gender Gap (minutes)\n(Women - Men)",
    title = paste0("Gender Gap Among Top ", TOP_PCT * 100, "% Finishers"),
    subtitle = "Lower = women closer to men"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(p_gap_time)
ggsave("output/gender_gap_top_performers_by_year.png", p_gap_time,
       width = 10, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# 3. DiD analysis on top performers
# -----------------------------------------------------------------------------

message("\n--- DiD on top performers ---")

df_top_did <- df_top %>%
  mutate(
    BirthCohort = cut(
      `Year of Birth`,
      breaks = c(-Inf, 1939, 1944, 1949, 1954, 1959, 1964, 1969, 1974, 1979,
                 1984, 1989, Inf),
      labels = c("<=1939", "1940-44", "1945-49", "1950-54", "1955-59",
                 "1960-64", "1965-69", "1970-74", "1975-79", "1980-84",
                 "1985-89", ">=1990"),
      right = TRUE
    ),
    PostTitleIX = as.integer(`Year of Birth` >= 1958),
    Female = as.integer(Gender == "W"),
    DiD = Female * PostTitleIX
  ) %>%
  filter(!is.na(BirthCohort))

# simple OLS DiD (since we're looking at means now)
lm_did <- lm(
  FinishSeconds ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    Female +
    PostTitleIX +
    DiD,
  data = df_top_did
)

did_coef <- summary(lm_did)$coefficients
did_results <- tibble(
  term = c("Female", "PostTitleIX", "DiD"),
  estimate = did_coef[c("Female", "PostTitleIX", "DiD"), "Estimate"],
  std_error = did_coef[c("Female", "PostTitleIX", "DiD"), "Std. Error"],
  p_value = did_coef[c("Female", "PostTitleIX", "DiD"), "Pr(>|t|)"]
)

message("DiD results (top ", TOP_PCT * 100, "% performers):")
print(did_results)

message("\nDiD estimate: ", round(did_results$estimate[3], 1), " seconds (",
        round(did_results$estimate[3] / 60, 2), " minutes)")
message("p-value: ", format(did_results$p_value[3], scientific = TRUE, digits = 3))

# -----------------------------------------------------------------------------
# 4. event study on top performers
# -----------------------------------------------------------------------------

message("\n--- event study on top performers ---")

df_top_did <- df_top_did %>%
  mutate(BirthCohort = relevel(BirthCohort, ref = "1950-54"))

lm_event <- lm(
  FinishSeconds ~
    factor(Year) +
    bs(Age, knots = c(21, 45, 60)) +
    Gender * BirthCohort,
  data = df_top_did
)

# extract interaction coefficients
coef_tbl <- summary(lm_event)$coefficients
interaction_terms <- rownames(coef_tbl)[grepl("^GenderM:BirthCohort", rownames(coef_tbl))]

event_data <- tibble(
  term = interaction_terms,
  cohort = gsub("GenderM:BirthCohort", "", interaction_terms),
  estimate = coef_tbl[interaction_terms, "Estimate"],
  std_error = coef_tbl[interaction_terms, "Std. Error"],
  p_value = coef_tbl[interaction_terms, "Pr(>|t|)"]
) %>%
  mutate(
    lo = estimate - 1.96 * std_error,
    hi = estimate + 1.96 * std_error
  )

# add reference cohort
all_cohorts <- c("<=1939", "1940-44", "1945-49", "1950-54", "1955-59",
                 "1960-64", "1965-69", "1970-74", "1975-79", "1980-84",
                 "1985-89", ">=1990")

event_data <- bind_rows(
  event_data,
  tibble(term = "ref", cohort = "1950-54", estimate = 0, std_error = 0,
         p_value = NA, lo = 0, hi = 0)
) %>%
  mutate(
    cohort = factor(cohort, levels = all_cohorts),
    title_ix_exposed = cohort %in% c("1960-64", "1965-69", "1970-74",
                                      "1975-79", "1980-84", "1985-89", ">=1990")
  ) %>%
  arrange(cohort)

write.csv(event_data, "output/event_study_top_performers.csv", row.names = FALSE)

# plot
p_event_top <- ggplot(event_data, aes(x = cohort, y = estimate, color = title_ix_exposed)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 5.5, linetype = "dotted", color = "red", linewidth = 1) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), linewidth = 0.8, alpha = 0.5) +
  scale_color_manual(
    values = c("FALSE" = "gray40", "TRUE" = "steelblue"),
    labels = c("Pre-Title IX", "Title IX Exposed"),
    name = "Cohort Status"
  ) +
  annotate("text", x = 5.5, y = max(event_data$hi, na.rm = TRUE) * 0.9,
           label = "Title IX\nTakes Effect", hjust = 0.5, color = "red", size = 3.5) +
  labs(
    x = "Birth Cohort",
    y = "Change in Gender Gap (seconds)\n(relative to 1950-54)",
    title = paste0("Event Study: Top ", TOP_PCT * 100, "% Performers"),
    subtitle = "Negative = women improving relative to men"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_event_top)
ggsave("output/event_study_top_performers.png", p_event_top,
       width = 12, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# 5. parallel trends test
# -----------------------------------------------------------------------------

message("\n--- parallel trends test (top performers) ---")

pre_data <- event_data %>%
  filter(cohort %in% c("<=1939", "1940-44", "1945-49", "1950-54", "1955-59")) %>%
  mutate(cohort_num = as.numeric(cohort))

if (nrow(pre_data) > 2) {
  pre_trend <- lm(estimate ~ cohort_num, data = pre_data)
  pre_summary <- summary(pre_trend)

  message("pre-treatment trend:")
  message("  slope: ", round(coef(pre_trend)[2], 2))
  message("  p-value: ", round(pre_summary$coefficients[2, 4], 4))
  message("  interpretation: ",
          ifelse(pre_summary$coefficients[2, 4] > 0.05,
                 "parallel trends supported",
                 "WARNING: pre-trend detected"))
}

# -----------------------------------------------------------------------------
# 6. placebo tests
# -----------------------------------------------------------------------------

message("\n--- placebo tests (top performers) ---")

placebo_top <- tibble(
  cutoff = integer(),
  estimate = numeric(),
  std_error = numeric(),
  p_value = numeric()
)

for (cutoff in c(1948, 1953, 1958, 1963, 1968)) {
  df_tmp <- df_top_did %>%
    mutate(
      FakePost = as.integer(`Year of Birth` >= cutoff),
      FakeDiD = Female * FakePost
    )

  fit <- lm(
    FinishSeconds ~ factor(Year) + bs(Age, knots = c(21, 45, 60)) +
      Female + FakePost + FakeDiD,
    data = df_tmp
  )

  coefs <- summary(fit)$coefficients
  placebo_top <- bind_rows(placebo_top, tibble(
    cutoff = cutoff,
    estimate = coefs["FakeDiD", "Estimate"],
    std_error = coefs["FakeDiD", "Std. Error"],
    p_value = coefs["FakeDiD", "Pr(>|t|)"]
  ))
}

placebo_top <- placebo_top %>%
  mutate(
    is_real = cutoff == 1958,
    lo = estimate - 1.96 * std_error,
    hi = estimate + 1.96 * std_error
  )

print(placebo_top)
write.csv(placebo_top, "output/placebo_top_performers.csv", row.names = FALSE)

message("\nmodel 8 (top performers DiD) complete")
