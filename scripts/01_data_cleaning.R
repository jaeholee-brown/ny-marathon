# data cleaning: load and prepare the marathon dataset
#
# requires: 00_setup.R to be sourced first
# outputs: df (cleaned master dataset)

# load and clean data
df <- read.csv(
  "NYC Marathon Results.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# convert types and filter to USA participants
df <- df %>%
  mutate(
    Year = as.integer(Year),
    Age = as.integer(Age)
  ) %>%
  filter(Country == "USA")

# create derived variables
df <- df %>%
  mutate(
    `Year of Birth` = Year - Age,
    FinishMinutes = as.numeric(as_hms(`Finish Time`)) / 60,
    Gender = factor(Gender, levels = c("W", "M"))
  )

# restrict the analysis to adults
df <- df %>%
  filter(Age >= 18)

# remove rows with missing values in key variables
df <- df %>%
  filter(
    !is.na(FinishMinutes),
    !is.na(Year),
    !is.na(Age),
    !is.na(Gender)
  )

message("data loaded: ", nrow(df), " observations")
