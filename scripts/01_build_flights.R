# 01_build_flights.R
# Load all flight files, clean, and build UTC timestamps for every scheduled flight.
# Output: <OUT_DIR>/flights_clean.rds
source("config.R"); source("R/utils.R")

files <- find_flight_files(FLIGHT_DIR, YEARS)
message("Reading ", length(files), " flight file(s)")

need <- c("FlightDate", "Operating_Airline", "Tail_Number", "Flight_Number_Operating_Airline",
          "Origin", "Dest", "CRSDepTime", "CRSArrTime", "CRSElapsedTime",
          "DepDelay", "ArrDelay", "Cancelled", "Diverted")

read_one <- function(f) {
  hdr <- trimws(names(fread(f, nrows = 0)))
  miss <- setdiff(need, hdr)
  if (length(miss)) stop("File ", basename(f), " is missing columns: ", paste(miss, collapse = ", "))
  d <- fread(f, select = need)
  setnames(d, trimws(names(d)))
  d
}
f <- rbindlist(lapply(files, read_one), use.names = TRUE)
message("Rows read: ", format(nrow(f), big.mark = ","))

f[, FlightDate := as.IDate(FlightDate)]
f[, `:=`(Cancelled = to_flag(Cancelled), Diverted = to_flag(Diverted))]
f[, Tail_Number := trimws(as.character(Tail_Number))]
f[Tail_Number %in% c("", "UNKNOWN", "UNKNOW", "NA"), Tail_Number := NA_character_]
f[, Operating_Airline := trimws(as.character(Operating_Airline))]

# drop exact duplicate records
key <- c("FlightDate", "Operating_Airline", "Tail_Number", "Flight_Number_Operating_Airline",
         "Origin", "Dest", "CRSDepTime")
n0 <- nrow(f); f <- unique(f, by = key)
message("Duplicate rows removed: ", n0 - nrow(f))

# rows without a usable tail number can't be placed in an aircraft sequence
n0 <- nrow(f); f <- f[!is.na(Tail_Number)]
message("Rows without tail number removed: ", format(n0 - nrow(f), big.mark = ","))

# ---- time zones --------------------------------------------------
tzmap <- get_airport_tz(DATA_DIR)
f[, origin_tz := tzmap$tz[match(Origin, tzmap$iata)]]
f[, dest_tz   := tzmap$tz[match(Dest,   tzmap$iata)]]
unk <- f[is.na(origin_tz) | is.na(dest_tz), .N, by = .(Origin, Dest)][order(-N)]
message("Flights with an unmapped airport time zone: ", format(sum(unk$N), big.mark = ","))
fwrite(unk, file.path(RESULTS_DIR, "unmapped_timezone_routes.csv"))

# ---- UTC timestamps ---------------------------------------------
# departure: local origin clock -> UTC. arrival: departure + SCHEDULED elapsed time.
f[, dep_utc := local_to_utc(FlightDate, CRSDepTime, origin_tz)]
f[, arr_utc := dep_utc + CRSElapsedTime * 60]
f[, time_unknown := is.na(dep_utc) | is.na(arr_utc)]

# fallback ordering time so rows with unknown times stay in the aircraft sequence
naive <- as.numeric(as.POSIXct(format(f$FlightDate), tz = "UTC")) +
         ifelse(is.na(f$CRSDepTime), 0, hhmm_to_minutes(f$CRSDepTime)) * 60
f[, order_time := fifelse(is.na(dep_utc), naive, dep_utc)]

# ---- validation: recomputed local arrival clock must match CRSArrTime ----
arr_min_calc <- utc_to_local_minutes(f$arr_utc, f$dest_tz)
arr_min_rec  <- hhmm_to_minutes(f$CRSArrTime)
d <- abs(arr_min_calc - arr_min_rec); d <- pmin(d, 1440 - d)       # circular difference
f[, arr_inconsistent := !is.na(d) & d > 2]
message("Arrival-time validation: ", format(sum(f$arr_inconsistent), big.mark = ","),
        " inconsistent rows (", round(100 * mean(f$arr_inconsistent), 3), "%)")
bad <- f[arr_inconsistent == TRUE, .N, by = .(Origin, Dest)][order(-N)][1:min(.N, 50)]
fwrite(bad, file.path(RESULTS_DIR, "arrival_inconsistent_routes_top50.csv"))

f[, c("origin_tz", "dest_tz") := NULL]
saveRDS(f, file.path(OUT_DIR, "flights_clean.rds"))
message("Saved flights_clean.rds with ", format(nrow(f), big.mark = ","), " rows")
