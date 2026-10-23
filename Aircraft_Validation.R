library(data.table)

flight_path <- path.expand("~/Downloads/Flights_2019_1.csv")
stopifnot(file.exists(flight_path))

# Inspect headers without loading the full dataset.
raw_names <- names(fread(flight_path, nrows = 0))
clean_names <- trimws(raw_names)

needed <- c(
  "FlightDate", "Operating_Airline", "Tail_Number",
  "Flight_Number_Operating_Airline",
  "Origin", "Dest", "CRSDepTime", "CRSArrTime",
  "CRSElapsedTime", "DepDelay", "ArrDelay",
  "Cancelled", "Diverted"
)

stopifnot(all(needed %in% clean_names))

# Select by column position to handle spaces in raw headers.
positions <- match(needed, clean_names)
flights <- fread(flight_path, select = positions)
setnames(flights, needed)

# Preserve an identifier for each imported record.
flights[, record_id := .I]

flights[, Tail_Number := trimws(as.character(Tail_Number))]
flights[Tail_Number == "", Tail_Number := NA_character_]

cat("Total records:", nrow(flights), "\n")
cat("Records missing aircraft ID:",
    sum(is.na(flights$Tail_Number)), "\n")
cat("Distinct aircraft IDs:",
    uniqueN(flights$Tail_Number, na.rm = TRUE), "\n")
cat("Records missing scheduled duration:",
    sum(is.na(flights$CRSElapsedTime)), "\n")

print(head(flights[, .(
  record_id, FlightDate, Tail_Number,
  Origin, Dest, CRSDepTime, CRSArrTime, CRSElapsedTime
)]))


# Check whether the same flight appears more than once.
flight_key <- c(
  "FlightDate", "Operating_Airline", "Tail_Number",
  "Flight_Number_Operating_Airline",
  "Origin", "Dest", "CRSDepTime"
)

duplicate_rows <- duplicated(flights, by = flight_key) |
                  duplicated(flights, by = flight_key, fromLast = TRUE)

cat("\nRows sharing a flight key:",
    sum(duplicate_rows), "\n")

if (any(duplicate_rows)) {
  print(head(flights[duplicate_rows]))
}

# Valid HHMM values are 0000–2359, plus 2400 for midnight.
valid_hhmm <- function(x) {
  !is.na(x) &
    x == floor(x) &
    ((x >= 0 & x <= 2359 & x %% 100 < 60) | x == 2400)
}

cat("Invalid/missing scheduled departure times:",
    sum(!valid_hhmm(flights$CRSDepTime)), "\n")

cat("Invalid/missing scheduled arrival times:",
    sum(!valid_hhmm(flights$CRSArrTime)), "\n")

cat("Missing/nonpositive scheduled durations:",
    sum(is.na(flights$CRSElapsedTime) |
        flights$CRSElapsedTime <= 0), "\n")

cat("Distinct origin airports:",
    uniqueN(flights$Origin), "\n")

# Inspect records whose scheduled duration is missing.
print(flights[is.na(CRSElapsedTime), .(
  record_id, FlightDate, Origin, Dest,
  CRSDepTime, CRSArrTime, Cancelled, Diverted
)])




# Download airport reference data once.
airport_path <- path.expand("~/Downloads/openflights_airports.dat")

if (!file.exists(airport_path)) {
  download.file(
    "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
    destfile = airport_path,
    mode = "wb"
  )
}

airport_reference <- fread(
  airport_path,
  header = FALSE,
  na.strings = "\\N",
  colClasses = "character"
)

stopifnot(ncol(airport_reference) == 14)

# OpenFlights: column 5 = IATA code; column 12 = named time zone.
airport_zones <- unique(
  airport_reference[
    !is.na(V5) & !is.na(V12),
    .(airport = V5, timezone = V12)
  ]
)

# Do not silently choose between conflicting time-zone entries.
conflicts <- airport_zones[, .N, by = airport][N > 1]

if (nrow(conflicts) > 0) {
  airport_zones <- airport_zones[
    !airport %in% conflicts$airport
  ]
}

# Only use time-zone names recognized by this R installation.
airport_zones <- airport_zones[timezone %in% OlsonNames()]

# Attach the origin's time zone without changing row order.
flights[, origin_tz :=
          airport_zones$timezone[
            match(Origin, airport_zones$airport)
          ]]

missing_zones <- flights[
  is.na(origin_tz),
  .(records = .N),
  by = Origin
][order(-records)]

cat("\nRecords missing origin time zone:",
    sum(is.na(flights$origin_tz)), "\n")

cat("Origin airports missing time zone:",
    nrow(missing_zones), "\n")

print(missing_zones)

# Sanity-check several familiar airports.
print(airport_zones[
  airport %in% c("ATL", "ORD", "DEN", "LAX", "PHX", "HNL")
][order(airport)])




# Documented correction for the missing EAR mapping.
# Source: https://cityofkearney.org/1401/Airport-Location
airport_zones <- rbind(
  airport_zones[airport != "EAR"],
  data.table(airport = "EAR", timezone = "America/Chicago")
)

flights[, origin_tz :=
          airport_zones$timezone[
            match(Origin, airport_zones$airport)
          ]]

stopifnot(!anyNA(flights$origin_tz))

# Convert local scheduled departure times to UTC.
# Group by time zone because each origin uses its own local time.
flights[, scheduled_dep_seconds := {
  departure_minutes <- (CRSDepTime %/% 100) * 60 +
                       CRSDepTime %% 100

  local_midnight <- as.POSIXct(
    as.character(FlightDate),
    format = "%Y-%m-%d",
    tz = .BY$origin_tz
  )

  as.numeric(local_midnight) + departure_minutes * 60
}, by = origin_tz]

flights[, scheduled_dep_utc := as.POSIXct(
  scheduled_dep_seconds,
  origin = "1970-01-01",
  tz = "UTC"
)]

# Elapsed duration lets arrival cross midnight automatically.
flights[, scheduled_arr_utc :=
          scheduled_dep_utc + CRSElapsedTime * 60]

stopifnot(!anyNA(flights$scheduled_dep_utc))

cat("\nRecords missing origin time zone:",
    sum(is.na(flights$origin_tz)), "\n")

cat("Records missing scheduled departure UTC:",
    sum(is.na(flights$scheduled_dep_utc)), "\n")

cat("Records missing scheduled arrival UTC:",
    sum(is.na(flights$scheduled_arr_utc)), "\n")

print(head(flights[, .(
  record_id, Origin, Dest, origin_tz,
  scheduled_dep_utc, scheduled_arr_utc
)]))




# Attach destination time zones.
flights[, dest_tz :=
          airport_zones$timezone[
            match(Dest, airport_zones$airport)
          ]]

cat("\nRecords missing destination time zone:",
    sum(is.na(flights$dest_tz)), "\n")

# Convert reconstructed UTC arrivals to destination clock minutes.
flights[, calculated_arr_minutes := NA_integer_]

flights[
  !is.na(dest_tz) & !is.na(scheduled_arr_utc),
  calculated_arr_minutes := {
    local_arrival <- as.POSIXlt(
      scheduled_arr_utc,
      tz = .BY$dest_tz
    )
    local_arrival$hour * 60L + local_arrival$min
  },
  by = dest_tz
]

# Treat 2400 and 0000 as the same clock time.
flights[, recorded_arr_minutes :=
          ((CRSArrTime %/% 100) * 60 +
             CRSArrTime %% 100) %% 1440]

# Signed clock difference, allowing for midnight.
flights[, arrival_clock_difference :=
          (calculated_arr_minutes -
             recorded_arr_minutes + 720) %% 1440 - 720]

cat("Arrivals checked:",
    sum(!is.na(flights$arrival_clock_difference)), "\n")

cat("Exact arrival-clock matches:",
    sum(flights$arrival_clock_difference == 0, na.rm = TRUE), "\n")

cat("Arrival-clock mismatches:",
    sum(flights$arrival_clock_difference != 0, na.rm = TRUE), "\n")

# Show which routes have mismatches.
print(
  flights[
    !is.na(arrival_clock_difference) &
      arrival_clock_difference != 0,
    .(records = .N),
    by = .(Origin, Dest, arrival_clock_difference)
  ][order(-records)][1:10]
)




# Flag inconsistencies while preserving every record.
flights[, arrival_clock_ok :=
          !is.na(arrival_clock_difference) &
          arrival_clock_difference == 0]

arrival_issues <- flights[
  !is.na(arrival_clock_difference) &
    arrival_clock_difference != 0,
  .(
    record_id, FlightDate, Tail_Number,
    Origin, Dest, origin_tz, dest_tz,
    CRSDepTime, CRSArrTime, CRSElapsedTime,
    arrival_clock_difference,
    Cancelled, Diverted
  )
]

cat("\nAll arrival-clock mismatches:\n")
print(arrival_issues, nrows = 50)

cat("\nTime-zone mappings for affected airports:\n")
affected_airports <- unique(c(
  arrival_issues$Origin,
  arrival_issues$Dest
))

print(
  airport_zones[airport %in% affected_airports][order(airport)],
  nrows = 100
)




# Preserve all records with an aircraft ID, including problematic
# operations, so removing one cannot create a false connection.
sequence <- copy(flights[!is.na(Tail_Number)])

# Flag tied departure timestamps: their ordering is ambiguous.
sequence[, departure_tie := .N > 1L,
         by = .(Tail_Number, scheduled_dep_utc)]

setorder(sequence, Tail_Number, scheduled_dep_utc, record_id)

# Attach information from the next observed flight for this aircraft.
next_fields <- c(
  "record_id", "Origin", "Dest",
  "scheduled_dep_utc", "DepDelay",
  "Cancelled", "Diverted",
  "arrival_clock_ok", "departure_tie"
)

sequence[
  , paste0("next_", next_fields) :=
      shift(.SD, type = "lead"),
  by = Tail_Number,
  .SDcols = next_fields
]

# A connection needs matching destination and next origin.
sequence[, airport_continuity :=
           !is.na(next_record_id) & Dest == next_Origin]

sequence[, scheduled_turnaround :=
           as.numeric(difftime(
             next_scheduled_dep_utc,
             scheduled_arr_utc,
             units = "mins"
           ))]

# Retrospective plausibility check, not a prediction feature.
sequence[, actual_turnaround :=
           scheduled_turnaround + next_DepDelay - ArrDelay]

# Initial conservative Atlanta connections.
# Keep the old turnaround limits for comparability.
atl_connections <- sequence[
  Dest == "ATL" &
    airport_continuity == TRUE &
    departure_tie == FALSE &
    next_departure_tie == FALSE &
    Cancelled == 0 & next_Cancelled == 0 &
    Diverted == 0 & next_Diverted == 0 &
    arrival_clock_ok == TRUE &
    next_arrival_clock_ok == TRUE &
    !is.na(ArrDelay) & !is.na(next_DepDelay) &
    scheduled_turnaround >= 30 &
    scheduled_turnaround <= 360 &
    actual_turnaround >= 0 
]

cat("\nRecords involved in tied aircraft departure times:",
    sum(sequence$departure_tie), "\n")

cat("Consecutive scheduled-flight candidates at Atlanta:",
    nrow(atl_connections), "\n")

print(head(atl_connections[, .(
  record_id, next_record_id, Tail_Number,
  Origin, connection_airport = Dest,
  next_destination = next_Dest,
  scheduled_arr_utc, next_scheduled_dep_utc,
  scheduled_turnaround, actual_turnaround
)]))





# Reconstruct actual departure instants from schedule + delay.
# Include diverted flights here so they cannot be silently skipped.
actual_order <- copy(sequence[
  Cancelled == 0 & !is.na(DepDelay)
])

actual_order[, actual_dep_utc :=
               scheduled_dep_utc + DepDelay * 60]

actual_order[, actual_departure_tie := .N > 1L,
             by = .(Tail_Number, actual_dep_utc)]

setorder(
  actual_order,
  Tail_Number, actual_dep_utc, record_id
)

actual_order[, actual_next_id := shift(record_id, type = "lead"),
             by = Tail_Number]

actual_order[, actual_next_tie :=
               shift(actual_departure_tie, type = "lead"),
             by = Tail_Number]

# Add these checks to our existing candidates.
check_index <- match(
  atl_connections$record_id,
  actual_order$record_id
)

atl_connections[, actual_next_id :=
                  actual_order$actual_next_id[check_index]]

atl_connections[, actual_order_unambiguous :=
                  !actual_order$actual_departure_tie[check_index] &
                  !actual_order$actual_next_tie[check_index]]

atl_connections[, same_next_flight :=
                  !is.na(actual_next_id) &
                  actual_next_id == next_record_id]

cat("\nSchedule-based Atlanta candidates:",
    nrow(atl_connections), "\n")

cat("Candidates missing incoming departure delay:",
    sum(is.na(atl_connections$DepDelay)), "\n")

cat("Same next flight in actual departure order:",
    sum(atl_connections$same_next_flight), "\n")

cat("Same next flight with no actual departure ties:",
    sum(
      atl_connections$same_next_flight &
      atl_connections$actual_order_unambiguous,
      na.rm = TRUE
    ), "\n")

print(head(atl_connections[
  same_next_flight == FALSE,
  .(
    record_id, Tail_Number,
    scheduled_next_id = next_record_id,
    actual_next_id, DepDelay,
    scheduled_turnaround, actual_turnaround
  )
]))






# Check for aircraft whose observed departure history is incomplete.
incomplete_aircraft <- unique(sequence[
  Cancelled == 0 & is.na(DepDelay),
  Tail_Number
])

atl_connections[, incomplete_departure_history :=
                  Tail_Number %in% incomplete_aircraft]

cat("\nOperated records missing departure delay:",
    sequence[Cancelled == 0 & is.na(DepDelay), .N], "\n")

cat("Agreeing Atlanta pairs on affected aircraft:",
    atl_connections[
      same_next_flight == TRUE &
        incomplete_departure_history == TRUE,
      .N
    ], "\n")

# Preserve the connections passing both ordering checks.
checked_connections <- copy(atl_connections[
  same_next_flight == TRUE &
    actual_order_unambiguous == TRUE
])

cat("Working connections after order checks:",
    nrow(checked_connections), "\n")

# Inspect nearby operations for each of the four disagreements.
disagreements <- atl_connections[same_next_flight == FALSE]

for (j in seq_len(nrow(disagreements))) {
  aircraft_id <- disagreements$Tail_Number[j]
  reference_time <- disagreements$scheduled_dep_utc[j]

  cat("\nNearby operations for aircraft:", aircraft_id, "\n")

  print(sequence[
    Tail_Number == aircraft_id &
      abs(as.numeric(difftime(
        scheduled_dep_utc, reference_time, units = "hours"
      ))) <= 12,
    .(
      record_id, Origin, Dest,
      scheduled_dep_utc, DepDelay, ArrDelay,
      Cancelled, Diverted, arrival_clock_ok
    )
  ])
}






# Preliminary recovery analysis among late incoming flights.
late_connections <- copy(
  checked_connections[ArrDelay >= 15]
)

late_connections[, recovered := next_DepDelay < 15]

cat("\nChecked connections:", nrow(checked_connections), "\n")
cat("Connections with incoming arrival delay >= 15 minutes:",
    nrow(late_connections), "\n")

print(late_connections[, .(
  connections = .N
), by = recovered][order(recovered)])

if (nrow(late_connections) > 0) {
  cat("Preliminary recovery percentage:",
      round(100 * mean(late_connections$recovered), 2),
      "%\n")
}

# Save outside the Git repository.
checkpoint_path <- path.expand(
  "~/Downloads/Atlanta_Connections_Jan2019_Checkpoint.rds"
)

saveRDS(
  list(
    checked_connections = checked_connections,
    late_connections = late_connections,
    disagreements = disagreements
  ),
  checkpoint_path
)

cat("Checkpoint saved to:", checkpoint_path, "\n")







# Descriptive comparison using predefined turnaround groups.
late_connections[, turnaround_group := cut(
  scheduled_turnaround,
  breaks = c(30, 60, 90, 120, 180, 360),
  right = FALSE,
  include.lowest = TRUE,
  labels = c(
    "30–<60 min",
    "60–<90 min",
    "90–<120 min",
    "120–<180 min",
    "180–360 min"
  )
)]

turnaround_summary <- late_connections[, .(
  connections = .N,
  recovered = sum(recovered),
  recovery_percent = round(100 * mean(recovered), 2)
), by = turnaround_group][order(turnaround_group)]

cat("\nRecovery by scheduled turnaround:\n")
print(turnaround_summary)

stopifnot(
  !anyNA(late_connections$turnaround_group),
  sum(turnaround_summary$connections) == nrow(late_connections)
)






late_connections[, incoming_delay_group := cut(
  ArrDelay,
  breaks = c(15, 30, 60, 120, Inf),
  right = FALSE,
  labels = c(
    "15–<30 min", "30–<60 min",
    "60–<120 min", "120+ min"
  )
)]

recovery_by_delay <- late_connections[, .(
  connections = .N,
  recovered = sum(recovered),
  recovery_percent = round(100 * mean(recovered), 2)
), by = .(
  incoming_delay_group, turnaround_group
)][order(incoming_delay_group, turnaround_group)]

cat("\nRecovery by incoming delay and turnaround:\n")
print(recovery_by_delay, nrows = 30)






# Proposed prediction time: incoming aircraft's actual arrival.
late_connections[, prediction_time_utc :=
                   scheduled_arr_utc + ArrDelay * 60]

late_connections[, minutes_to_scheduled_departure :=
                   scheduled_turnaround - ArrDelay]

cat("\nTime remaining at incoming arrival:\n")
print(late_connections[, .(
  connections = .N,
  recovered = sum(recovered),
  recovery_percent = round(100 * mean(recovered), 2)
), by = .(
  scheduled_departure_passed =
    minutes_to_scheduled_departure < 0
)])

cat("\nRange of minutes until scheduled departure:\n")
print(range(late_connections$minutes_to_scheduled_departure))







# Main prediction cohort:
# incoming flight is late, but next scheduled departure is still ahead.
model_cohort <- copy(late_connections[
  minutes_to_scheduled_departure > 0 &
    actual_turnaround > 0
])

cat("\nMain prediction cohort:", nrow(model_cohort), "\n")

print(model_cohort[, .(
  connections = .N,
  recovery_percent = round(100 * mean(recovered), 2)
), by = recovered])

cat("Arrivals exactly at next scheduled departure:",
    late_connections[
      minutes_to_scheduled_departure == 0, .N
    ], "\n")

cat("Connections with zero actual turnaround:",
    late_connections[actual_turnaround == 0, .N], "\n")

# Preserve the earlier checkpoint and save this cohort separately.
saveRDS(
  model_cohort,
  path.expand("~/Downloads/Atlanta_Recovery_Cohort_Jan2019.rds")
)




# Export Atlanta's full scheduled activity.
# Include cancelled/diverted flights because we are measuring
# scheduled activity, not the flights that ultimately operated.

atl_schedule <- rbindlist(list(
  flights[
    Origin == "ATL" & !is.na(scheduled_dep_utc),
    .(
      record_id,
      movement = "departure",
      scheduled_time_utc = scheduled_dep_utc
    )
  ],
  flights[
    Dest == "ATL" & !is.na(scheduled_arr_utc),
    .(
      record_id,
      movement = "arrival",
      scheduled_time_utc = scheduled_arr_utc
    )
  ]
))

setorder(atl_schedule, scheduled_time_utc)

cat("\nAtlanta scheduled movements:\n")
print(atl_schedule[, .N, by = movement])

cat("\nSchedule timestamp range:\n")
print(range(atl_schedule$scheduled_time_utc))

schedule_path <- path.expand(
  "~/Downloads/Atlanta_Schedule_Jan2019.rds"
)

saveRDS(atl_schedule, schedule_path)

cat("\nAtlanta schedule saved to:", schedule_path, "\n")





# AUDIT: begin with schedule-linked candidates and incoming information.
# Do not filter on the outgoing flight's operational outcome yet.
audit <- copy(sequence[
  Dest == "ATL" &
    airport_continuity == TRUE &
    departure_tie == FALSE &
    next_departure_tie == FALSE &
    Cancelled == 0 &
    Diverted == 0 &
    arrival_clock_ok == TRUE &
    !is.na(ArrDelay) &
    ArrDelay >= 15 &
    scheduled_turnaround >= 30 &
    scheduled_turnaround <= 360 &
    scheduled_turnaround - ArrDelay > 0
])

# Record each retrospective condition separately.
audit[, outgoing_operated_normally :=
  !is.na(next_Cancelled) & next_Cancelled == 0 &
  !is.na(next_Diverted) & next_Diverted == 0
]

audit[, outgoing_delay_known := !is.na(next_DepDelay)]

audit[, outgoing_schedule_consistent :=
  !is.na(next_arrival_clock_ok) & next_arrival_clock_ok
]

audit[, actual_turnaround_pass :=
  !is.na(actual_turnaround) &
  actual_turnaround > 0
]

# Attach actual-order checks to this broader candidate set.
audit_index <- match(audit$record_id, actual_order$record_id)

audit[, actual_order_pass :=
  !is.na(actual_order$actual_next_id[audit_index]) &
  actual_order$actual_next_id[audit_index] == next_record_id &
  !is.na(actual_order$actual_departure_tie[audit_index]) &
  !actual_order$actual_departure_tie[audit_index] &
  !is.na(actual_order$actual_next_tie[audit_index]) &
  !actual_order$actual_next_tie[audit_index]
]

# Show cumulative exclusions in a documented order.
audit_conditions <- c(
  "outgoing_operated_normally",
  "outgoing_delay_known",
  "outgoing_schedule_consistent",
  "actual_turnaround_pass",
  "actual_order_pass"
)

remaining <- rep(TRUE, nrow(audit))

audit_counts <- data.table(
  step = "Schedule-linked late arrivals with departure still ahead",
  remaining = nrow(audit),
  excluded_at_step = 0L
)

for (condition in audit_conditions) {
  previous_count <- sum(remaining)
  remaining <- remaining & (audit[[condition]] %in% TRUE)

  audit_counts <- rbind(
    audit_counts,
    data.table(
      step = condition,
      remaining = sum(remaining),
      excluded_at_step = previous_count - sum(remaining)
    )
  )
}

cat("\nCOHORT FILTER AUDIT:\n")
print(audit_counts)

# Confirm that this audit reproduces the existing model cohort.
stopifnot(
  setequal(
    audit$record_id[remaining],
    model_cohort$record_id
  )
)

cat("\nAudit reproduces the existing model cohort.\n")

saveRDS(
  list(candidates = audit, filter_counts = audit_counts),
  path.expand("~/Downloads/Atlanta_Cohort_FilterAudit_Jan2019.rds")
)





cat("\nCases excluded by actual-turnaround limits:\n")

print(audit[
  outgoing_operated_normally == TRUE &
    outgoing_delay_known == TRUE &
    outgoing_schedule_consistent == TRUE &
    actual_turnaround_pass == FALSE,
  .(
    record_id,
    next_record_id,
    Tail_Number,
    ArrDelay,
    next_DepDelay,
    scheduled_turnaround,
    actual_turnaround,
    actual_order_pass
  )
])