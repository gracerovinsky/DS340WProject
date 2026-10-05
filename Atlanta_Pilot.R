library(data.table)

flight_path <- path.expand("~/Downloads/Flights_2019_1.csv")
weather_path <- path.expand("~/Downloads/72219013874.csv")

stopifnot(file.exists(flight_path), file.exists(weather_path))

# Read only the columns needed for the weather-matching pilot.
flights <- fread(
  flight_path,
  select = c("FlightDate", "Origin", "CRSDepTime")
)

weather <- fread(
  weather_path,
  select = c("STATION", "NAME", "DATE", "REPORT_TYPE",
             "TMP", "WND", "VIS"),
  colClasses = "character"
)

atl <- flights[Origin == "ATL"]

cat("Total flight records:", nrow(flights), "\n")
cat("Atlanta scheduled departures:", nrow(atl), "\n")
cat("Weather observations:", nrow(weather), "\n")
cat("Weather station:", unique(weather$NAME), "\n")




# Construct scheduled departure timestamps in Atlanta local time.
hhmm <- as.integer(atl$CRSDepTime)

stopifnot(
  !anyNA(hhmm),
  all(hhmm %% 100 < 60),
  all(hhmm >= 0 & hhmm <= 2400),
  all(hhmm != 2400 | hhmm %% 100 == 0)
)

atl[, scheduled_local :=
      as.POSIXct(
        as.character(FlightDate),
        format = "%Y-%m-%d",
        tz = "America/New_York"
      ) +
      ((hhmm %/% 100) * 60 + hhmm %% 100) * 60]

# Store the same instants with UTC display.
atl[, scheduled_utc :=
      as.POSIXct(
        as.numeric(scheduled_local),
        origin = "1970-01-01",
        tz = "UTC"
      )]

atl[, prediction_cutoff := scheduled_utc - 2 * 60 * 60]

weather[, weather_time :=
          as.POSIXct(
            DATE,
            format = "%Y-%m-%dT%H:%M:%S",
            tz = "UTC"
          )]

stopifnot(
  !anyNA(atl$prediction_cutoff),
  !anyNA(weather$weather_time)
)

# Temporary tie-breaking rule; quality-based selection comes later.
setorder(weather, weather_time, REPORT_TYPE)
weather_unique <- weather[
  !duplicated(weather_time, fromLast = TRUE)
]

# Find the latest weather timestamp at or before each cutoff.
weather_seconds <- as.numeric(weather_unique$weather_time)
cutoff_seconds <- as.numeric(atl$prediction_cutoff)

index <- findInterval(cutoff_seconds, weather_seconds)

# Index 0 means there was no earlier observation.
safe_index <- pmax(index, 1L)
age_seconds <- cutoff_seconds - weather_seconds[safe_index]

has_match <- index > 0L &
             age_seconds >= 0 &
             age_seconds <= 90 * 60

atl[, weather_time := weather_unique$weather_time[safe_index]]
atl[!has_match, weather_time := as.POSIXct(NA, tz = "UTC")]

atl[, weather_age_minutes :=
      as.numeric(difftime(
        prediction_cutoff, weather_time, units = "mins"
      ))]

cat("\nFlights matched:", sum(has_match), "\n")
cat("Flights unmatched:", sum(!has_match), "\n")
cat("Match rate:", round(mean(has_match) * 100, 2), "%\n")
cat("Median weather age:",
    median(atl$weather_age_minutes, na.rm = TRUE), "minutes\n")
cat("Maximum weather age:",
    max(atl$weather_age_minutes, na.rm = TRUE), "minutes\n")

stopifnot(all(
  atl$weather_time[has_match] <= atl$prediction_cutoff[has_match]
))



# Aircraft-connection screening at Atlanta
aircraft <- fread(flight_path)
setnames(aircraft, trimws(names(aircraft)))

aircraft <- aircraft[
  !is.na(Tail_Number) & trimws(Tail_Number) != ""
]

# Scheduled clock times in minutes after midnight
aircraft[, dep_minutes :=
           (CRSDepTime %/% 100) * 60 + CRSDepTime %% 100]

aircraft[, arr_minutes :=
           (CRSArrTime %/% 100) * 60 + CRSArrTime %% 100]

incoming <- aircraft[
  Dest == "ATL" &
    Cancelled == 0 &
    Diverted == 0 &
    !is.na(ArrDelay),
  .(
    FlightDate, Operating_Airline, Tail_Number,
    Origin_in = Origin,
    CRSDepTime_in = CRSDepTime,
    CRSArrTime_in = CRSArrTime,
    arr_minutes,
    ArrDelay_in = ArrDelay
  )
]

outgoing <- aircraft[
  Origin == "ATL" &
    Cancelled == 0 &
    Diverted == 0 &
    !is.na(DepDelay),
  .(
    FlightDate, Operating_Airline, Tail_Number,
    Dest_out = Dest,
    CRSDepTime_out = CRSDepTime,
    dep_minutes,
    DepDelay_out = DepDelay
  )
]

pairs <- merge(
  incoming,
  outgoing,
  by = c("FlightDate", "Operating_Airline", "Tail_Number"),
  allow.cartesian = TRUE
)

# Both arrival and departure clock times are local to ATL.
pairs[, scheduled_turnaround :=
        dep_minutes - arr_minutes]

pairs[, actual_turnaround :=
        scheduled_turnaround + DepDelay_out - ArrDelay_in]

# Provisional limits for this same-date screening test
pairs <- pairs[
  scheduled_turnaround >= 30 &
    scheduled_turnaround <= 360 &
    actual_turnaround >= 0 &
    actual_turnaround <= 720
]

pairs[, arrival_matches := .N,
      by = .(
        FlightDate, Operating_Airline, Tail_Number,
        CRSDepTime_in, Origin_in
      )]

pairs[, departure_matches := .N,
      by = .(
        FlightDate, Operating_Airline, Tail_Number,
        CRSDepTime_out, Dest_out
      )]

candidates <- pairs[
  arrival_matches == 1 & departure_matches == 1
]

cat("\nPlausible candidate pairs:", nrow(pairs), "\n")
cat("Unique candidate pairs:", nrow(candidates), "\n")

print(head(candidates[, .(
  FlightDate, Tail_Number,
  Origin_in, Dest_out,
  CRSArrTime_in, CRSDepTime_out,
  scheduled_turnaround,
  ArrDelay_in, DepDelay_out
)]))