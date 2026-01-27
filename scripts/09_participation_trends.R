# participation trends: visualization of participant counts by year and gender
#
# requires: 00_setup.R and 01_data_cleaning.R to be sourced first
# outputs: participants_by_year, poster png

# summarize the data by year and gender
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
  # custom gender colors
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
    title = "Americans in NYC Marathon (1970-2010)",
    x = "Year",
    y = "Number of Participants",
    fill = "Gender"
  ) +
  # poster theme adjustments
  theme_minimal(base_size = 36) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 44),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.title.x = element_text(margin = margin(t = 12))
  )

print(p_participation)

ggsave(
  filename = "output/nyc_marathon_poster.png",
  plot = p_participation,
  width = 15,
  height = 12,
  dpi = 600
)

message("participation trends complete")
