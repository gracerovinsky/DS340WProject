library(data.table)

cohort <- readRDS(path.expand(
  "~/Downloads/Atlanta_Recovery_Weather_Jan2019.rds"
))

schedule <- readRDS(path.expand(
  "~/Downloads/Atlanta_Schedule_Jan2019.rds"
))

# Count movements in the interval (arrival time, arrival time + 60 min].
count_next_hour <- function(prediction_times, movement_times) {
  movement_seconds <- sort(as.numeric(movement_times))
  prediction_seconds <- as.numeric(prediction_times)

  findInterval(prediction_seconds + 3600, movement_seconds) -
    findInterval(prediction_seconds, movement_seconds)
}

cohort[, scheduled_arrivals_next60 := count_next_hour(
  prediction_time_utc,
  schedule[movement == "arrival", scheduled_time_utc]
)]

cohort[, scheduled_departures_next60 := count_next_hour(
  prediction_time_utc,
  schedule[movement == "departure", scheduled_time_utc]
)]

cohort[, scheduled_movements_next60 :=
  scheduled_arrivals_next60 + scheduled_departures_next60
]

# Apply conservative month-boundary checks.
# January 1 arrivals may include flights departing in December,
# which are absent from our January flight file.
coverage_start <- as.POSIXct(
  "2019-01-02 00:00:00", tz = "America/New_York"
)

coverage_end <- as.POSIXct(
  "2019-02-01 00:00:00", tz = "America/New_York"
)

# Compare numeric timestamps: seconds since the same reference instant.
cohort[, traffic_window_covered :=
  as.numeric(prediction_time_utc) >= as.numeric(coverage_start) &
  as.numeric(prediction_time_utc) + 3600 < as.numeric(coverage_end)
]

cohort[, prediction_date_atl := as.IDate(
  format(
    prediction_time_utc,
    tz = "America/New_York",
    format = "%Y-%m-%d"
  )
)]

cat("\nDaily counts for connections with covered traffic windows:\n")

print(cohort[
  traffic_window_covered == TRUE,
  .(
    connections = .N,
    recovered = sum(recovered),
    not_recovered = sum(!recovered)
  ),
  by = prediction_date_atl
][order(prediction_date_atl)])

traffic_columns <- c(
  "scheduled_arrivals_next60",
  "scheduled_departures_next60",
  "scheduled_movements_next60"
)

# Preserve all connections, but mark incomplete windows as missing.
for (column in traffic_columns) {
  set(
    cohort,
    i = which(!cohort$traffic_window_covered),
    j = column,
    value = NA_integer_
  )
}

cat("\nTraffic-window coverage:\n")
print(cohort[, .N, by = traffic_window_covered])

cat("\nScheduled traffic in the next 60 minutes:\n")
print(summary(as.data.frame(cohort[, ..traffic_columns])))

cat("\nExample connections:\n")
print(head(cohort[, .(
  prediction_time_utc,
  scheduled_arrivals_next60,
  scheduled_departures_next60,
  scheduled_movements_next60,
  recovered
)]))

output_path <- path.expand(
  "~/Downloads/Atlanta_Recovery_Weather_Traffic_Jan2019.rds"
)

saveRDS(cohort, output_path)
cat("\nCheckpoint saved to:", output_path, "\n")