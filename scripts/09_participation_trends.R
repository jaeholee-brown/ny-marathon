# descriptive figures: participation over time and age distribution
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: participants_by_year and two png files

# summarize the data by year and sex
participants_by_year <- df %>%
  group_by(Year, Gender) %>%
  summarize(
    N = n(),
    .groups = "drop"
  )

# poster-style stacked bar chart
p_participation <- ggplot(
  participants_by_year,
  aes(x = Year, y = N, fill = Gender)
) +
  # stacked bars
  geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
  # custom sex colors
  scale_fill_manual(
    values = c(
      "W" = "#FF7F00",
      "M" = "#1F78B4"
    ),
    labels = c("W" = "Women (W)", "M" = "Men (M)")
  ) +
  # axis formatting
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(10)) +
  # labels
  labs(
    title = "Americans in NYC Marathon (1970-2024)",
    x = "Year",
    y = "Number of Participants",
    fill = "Sex"
  ) +
  # poster theme adjustments
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.title.x = element_text(margin = margin(t = 12))
  )

if (interactive()) print(p_participation)

ggsave(
  filename = "output/nyc_marathon_poster.png",
  plot = p_participation,
  width = 15,
  height = 12,
  dpi = 600,
  bg = "white"
)

# age-distribution histogram
p_age <- ggplot(df, aes(x = Age)) +
  geom_histogram(
    binwidth = 1,
    boundary = 18,
    fill = "white",
    color = "grey60",
    linewidth = 0.3
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Age (years)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

if (interactive()) print(p_age)

ggsave(
  filename = "output/age_distribution.png",
  plot = p_age,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

message("descriptive figures complete")
