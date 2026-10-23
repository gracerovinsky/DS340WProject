library(data.table)

# Load the saved cohort without rerunning aircraft validation.
cohort_path <- path.expand(
  "~/Downloads/Atlanta_Recovery_Cohort_Jan2019.rds"
)

stopifnot(file.exists(cohort_path))
model_cohort <- readRDS(cohort_path)

cat("Cohort records:", nrow(model_cohort), "\n")
cat("Overall recovery percentage:",
    round(100 * mean(model_cohort$recovered), 2), "%\n")

# Read weather as text to preserve coded values and quality flags.
weather_path <- path.expand("~/Downloads/72219013874.csv")
stopifnot(file.exists(weather_path))

weather <- fread(
  weather_path,
  select = c(
    "STATION", "DATE", "REPORT_TYPE",
    "TMP", "WND", "VIS", "CIG"
  ),
  colClasses = "character"
)

cat("\nWeather stations:\n")
print(unique(weather$STATION))

cat("\nWeather report types:\n")
print(weather[, .N, by = REPORT_TYPE][order(-N)])

cat("\nExample coded weather observations:\n")
print(head(weather[, .(
  DATE, REPORT_TYPE, TMP, WND, VIS, CIG
)]))




# Keep routine and special aviation observations.
# Exclude daily/monthly summaries and other report types.
weather_clean <- copy(
  weather[trimws(REPORT_TYPE) %chin% c("FM-15", "FM-16")]
)

# Convert observation timestamps to UTC.
weather_clean[, weather_time_utc := as.POSIXct(
  DATE,
  format = "%Y-%m-%dT%H:%M:%S",
  tz = "UTC"
)]

# Separate measurements from their quality flags.
weather_clean[, c("temperature_raw", "temperature_qc") :=
                tstrsplit(TMP, ",", fixed = TRUE)]

weather_clean[, c("wind_direction_raw", "wind_direction_qc",
                  "wind_type", "wind_speed_raw", "wind_speed_qc") :=
                tstrsplit(WND, ",", fixed = TRUE)]

weather_clean[, c("visibility_raw", "visibility_qc",
                  "visibility_variability",
                  "visibility_variability_qc") :=
                tstrsplit(VIS, ",", fixed = TRUE)]

weather_clean[, c("ceiling_raw", "ceiling_qc",
                  "ceiling_method", "ceiling_cavok") :=
                tstrsplit(CIG, ",", fixed = TRUE)]

# Conservative quality rule for this pilot.
accepted_qc <- c("1", "5")

# Decode units and replace missing/rejected measurements with NA.
weather_clean[, temperature_c := fifelse(
  temperature_qc %chin% accepted_qc &
    !is.na(temperature_raw) & temperature_raw != "+9999",
  as.numeric(temperature_raw) / 10,
  NA_real_
)]

weather_clean[, wind_speed_ms := fifelse(
  wind_speed_qc %chin% accepted_qc &
    !is.na(wind_speed_raw) & wind_speed_raw != "9999",
  as.numeric(wind_speed_raw) / 10,
  NA_real_
)]

weather_clean[, visibility_m := fifelse(
  visibility_qc %chin% c("1", "5", "A") &
    !is.na(visibility_raw) & visibility_raw != "999999",
  as.numeric(visibility_raw),
  NA_real_
)]

weather_clean[, ceiling_m := fifelse(
  ceiling_qc %chin% accepted_qc &
    !is.na(ceiling_raw) & ceiling_raw != "99999",
  as.numeric(ceiling_raw),
  NA_real_
)]

# Distinguish unlimited ceiling from measured ceiling height.
weather_clean[, ceiling_unlimited := ceiling_m == 22000]

weather_clean[, ceiling_height_m := fifelse(
  ceiling_unlimited,
  NA_real_,
  ceiling_m
)]

# Inspect January, which matches our flight pilot.
weather_jan <- weather_clean[
  weather_time_utc >= as.POSIXct("2019-01-01", tz = "UTC") &
  weather_time_utc < as.POSIXct("2019-02-01", tz = "UTC")
]

weather_variables <- c(
  "temperature_c", "wind_speed_ms", "visibility_m", "ceiling_m"
)

cat("\nJanuary aviation weather observations:",
    nrow(weather_jan), "\n")

cat("\nJanuary missing measurements after quality checks:\n")
print(weather_jan[, lapply(.SD, function(x) sum(is.na(x))),
                  .SDcols = weather_variables])

cat("\nJanuary decoded weather summary:\n")
print(summary(as.data.frame(weather_jan[, ..weather_variables])))

cat("\nJanuary records sharing an observation timestamp:\n")
print(weather_jan[, .N, by = weather_time_utc][N > 1])





# Use the full weather file so matches can cross month boundaries.
setorder(weather_clean, weather_time_utc)

stopifnot(
  !anyNA(weather_clean$weather_time_utc),
  !anyDuplicated(weather_clean$weather_time_utc),
  !anyNA(model_cohort$prediction_time_utc)
)

cohort_weather <- copy(model_cohort)

# Find the latest observation at or before each prediction time.
weather_index <- findInterval(
  as.numeric(cohort_weather$prediction_time_utc),
  as.numeric(weather_clean$weather_time_utc)
)

# Index 0 means no earlier observation exists.
weather_index[weather_index == 0L] <- NA_integer_

observation_age <- as.numeric(difftime(
  cohort_weather$prediction_time_utc,
  weather_clean$weather_time_utc[weather_index],
  units = "mins"
))

# Reject observations older than 90 minutes.
valid_match <- !is.na(observation_age) &
  observation_age >= 0 & observation_age <= 90

weather_index[!valid_match] <- NA_integer_

# Attach weather measurements without removing any connections.
columns_to_attach <- c(
  "weather_time_utc", "REPORT_TYPE",
  "temperature_c", "wind_speed_ms",
  "visibility_m", "visibility_qc",
  "ceiling_m", "ceiling_height_m", "ceiling_unlimited",
  "ceiling_method", "ceiling_cavok"
)

for (column in columns_to_attach) {
  set(
    cohort_weather,
    j = column,
    value = weather_clean[[column]][weather_index]
  )
}

cohort_weather[, weather_age_minutes :=
  fifelse(valid_match, observation_age, NA_real_)]

cat("\nConnections:", nrow(cohort_weather), "\n")
cat("Weather timestamp matches:", sum(valid_match), "\n")
cat("Unmatched connections:", sum(!valid_match), "\n")

cat("\nMatched weather age in minutes:\n")
print(summary(cohort_weather$weather_age_minutes))

cat("\nMissing weather measurements in the matched cohort:\n")
print(cohort_weather[
  , lapply(.SD, function(x) sum(is.na(x))),
  .SDcols = weather_variables
])

cat("\nCeiling codes for observations recorded as 22000 metres:\n")
print(weather_jan[
  ceiling_m == 22000,
  .N,
  by = .(ceiling_method, ceiling_cavok)
])




# Check which raw visibility values failed our cleaning rule.
cat("\nJanuary visibility values excluded by quality checks:\n")
print(
  weather_jan[
    is.na(visibility_m),
    .N,
    by = .(visibility_raw, visibility_qc)
  ][order(-N)]
)

# Check how many cohort connections use the ceiling code
# that still needs interpretation.
cat("\nConnections with ceiling recorded as 22000:\n")
print(cohort_weather[
  , .(connections = sum(ceiling_m == 22000, na.rm = TRUE))
])

# Check that the matching process preserved the cohort
# and never selected a future observation.
stopifnot(
  nrow(cohort_weather) == nrow(model_cohort),
  all(cohort_weather$weather_time_utc <=
        cohort_weather$prediction_time_utc),
  all(cohort_weather$weather_age_minutes <= 90)
)

# Save the matched dataset as a checkpoint.
output_path <- path.expand(
  "~/Downloads/Atlanta_Recovery_Weather_Jan2019.rds"
)

saveRDS(cohort_weather, output_path)

cat("\nWeather-matched checkpoint saved to:",
    output_path, "\n")