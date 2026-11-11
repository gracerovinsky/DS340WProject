# 02_clean_weather.R
# Decode NOAA Global Hourly (ISD) fields for every study airport's station.
# Input : <WEATHER_DIR>/<station>*.csv  (one file per station-year)
# Output: <OUT_DIR>/weather_clean.rds   (column `airport` identifies the station)
source("config.R"); source("R/utils.R")

GOOD <- c("0", "1", "4", "5", "A", "C", "I", "M", "P", "R", "U")   # ISD quality codes treated as valid
num  <- function(x) suppressWarnings(as.numeric(x))
want <- c("STATION", "NAME", "DATE", "REPORT_TYPE", "TMP", "WND", "VIS", "CIG")
read_wx <- function(f) { h <- names(fread(f, nrows = 0)); fread(f, select = intersect(want, h), colClasses = "character") }

clean_wx <- function(w) {
  w[, report_type := trimws(REPORT_TYPE)]
  w <- w[report_type %in% c("FM-15", "FM-16")]                 # routine + special aviation reports only
  w[, wx_sec := as.numeric(as.POSIXct(DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))]
  w <- w[!is.na(wx_sec)]
  tp <- tstrsplit(w$TMP, ",", fixed = TRUE, keep = 1:2)        # +0056,1 (tenths deg C)
  w[, tmp_c  := fifelse(tp[[2]] %in% GOOD & num(tp[[1]]) != 9999, num(tp[[1]]) / 10, NA_real_)]
  wd <- tstrsplit(w$WND, ",", fixed = TRUE, keep = 1:5)        # dir,q,type,speed,q (tenths m/s)
  w[, wnd_ms := fifelse(wd[[5]] %in% GOOD & num(wd[[4]]) != 9999, num(wd[[4]]) / 10, NA_real_)]
  vs <- tstrsplit(w$VIS, ",", fixed = TRUE, keep = 1:2)        # dist (m), q
  w[, vis_m  := fifelse(vs[[2]] %in% GOOD & num(vs[[1]]) != 999999, num(vs[[1]]), NA_real_)]
  cg <- tstrsplit(w$CIG, ",", fixed = TRUE, keep = 1:2)        # ceiling (m), q
  w[, ceil_m := fifelse(cg[[2]] %in% GOOD & num(cg[[1]]) != 99999, num(cg[[1]]), NA_real_)]
  # FAA-style flight category (1/3/5 SM = 1609/4828/8047 m; 500/1000/3000 ft = 152/305/914 m)
  v <- w$vis_m; cl <- w$ceil_m; lt <- function(x, t) !is.na(x) & x < t
  w[, flight_cat := fifelse(is.na(v), NA_character_,
                     fifelse(lt(v, 1609) | lt(cl, 152), "LIFR",
                     fifelse(lt(v, 4828) | lt(cl, 305), "IFR",
                     fifelse(lt(v, 8047) | lt(cl, 914), "MVFR", "VFR"))))]
  w[, vis_km := vis_m / 1000]
  w[, n_na := is.na(tmp_c) + is.na(wnd_ms) + is.na(vis_m)]     # one obs per timestamp: most complete wins
  setorder(w, wx_sec, n_na, report_type)
  w <- w[!duplicated(wx_sec)]
  w[, .(wx_sec, report_type, tmp_c, wnd_ms, vis_km, ceil_m, flight_cat)]
}

out <- list(); qc <- list()
for (ap in names(AIRPORTS)) {
  st <- AIRPORTS[[ap]]
  files <- list.files(WEATHER_DIR, pattern = paste0("^", st, ".*\\.csv$"), full.names = TRUE)
  if (!length(files)) { warning("No weather files for ", ap, " (station ", st, ") - airport will be skipped"); next }
  w <- rbindlist(lapply(files, read_wx), use.names = TRUE, fill = TRUE)
  # SANITY CHECK: make sure the station really is the airport you intended
  nm <- if ("NAME" %in% names(w)) paste(unique(w$NAME), collapse = " | ") else "(no NAME column)"
  message(sprintf("%s  station %s -> %s   [%d files, %s raw rows]", ap, st, nm, length(files), format(nrow(w), big.mark = ",")))
  cw <- clean_wx(w); cw[, airport := ap]
  out[[ap]] <- cw
  qc[[ap]] <- data.table(airport = ap, obs = nrow(cw),
                         pct_missing_temp = round(100 * mean(is.na(cw$tmp_c)), 1),
                         pct_missing_wind = round(100 * mean(is.na(cw$wnd_ms)), 1),
                         pct_missing_vis  = round(100 * mean(is.na(cw$vis_km)), 1),
                         pct_LIFR_IFR     = round(100 * mean(cw$flight_cat %in% c("LIFR", "IFR"), na.rm = TRUE), 1))
}
if (!length(out)) stop("No weather data loaded for any airport")
wx <- rbindlist(out); setcolorder(wx, "airport")
qc <- rbindlist(qc); print(qc); fwrite(qc, file.path(RESULTS_DIR, "weather_quality_by_airport.csv"))
saveRDS(wx, file.path(OUT_DIR, "weather_clean.rds"))
message("Saved weather_clean.rds (", nrow(wx), " observations)")
