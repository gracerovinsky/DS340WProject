# 03_build_connections.R
# Build aircraft connections at every study airport, recovery labels, weather and traffic features.
# Output: <OUT_DIR>/connections.rds  +  results/connection_funnel.csv (+ summaries)
source("config.R"); source("R/utils.R")

f  <- readRDS(file.path(OUT_DIR, "flights_clean.rds"))
wx <- readRDS(file.path(OUT_DIR, "weather_clean.rds"))
tzmap <- get_airport_tz(DATA_DIR)

# ---- 1. aircraft sequences, built ONCE from ALL flights (incl. cancelled/diverted) ----
setorder(f, Tail_Number, order_time, CRSDepTime)
f[, `:=`(nxt_airline  = shift(Operating_Airline, 1L, type = "lead"),
         nxt_origin   = shift(Origin,            1L, type = "lead"),
         nxt_dep_utc  = shift(dep_utc,           1L, type = "lead"),
         nxt_depdelay = shift(DepDelay,          1L, type = "lead"),
         nxt_cancel   = shift(Cancelled,         1L, type = "lead"),
         nxt_unknown  = shift(time_unknown,      1L, type = "lead")),
  by = Tail_Number]

build_airport <- function(ap) {
  ap_tz <- tzmap$tz[tzmap$iata == ap]; stopifnot(length(ap_tz) == 1)
  wxa <- wx[airport == ap]
  if (!nrow(wxa)) { warning("no weather for ", ap, ": skipped"); return(NULL) }
  setorder(wxa, wx_sec)
  funnel <- list()
  step <- function(label, d) { funnel[[length(funnel) + 1]] <<- data.table(airport = ap, step = label, n = nrow(d)); d }

  # scheduled traffic at this airport (all scheduled movements, incl. later-cancelled)
  ev <- sort(c(f[Dest == ap & !is.na(arr_utc), arr_utc], f[Origin == ap & !is.na(dep_utc), dep_utc]))
  cov_end <- local_to_utc(max(f$FlightDate) + 1L, 0L, ap_tz)       # end of loaded data (local midnight)

  c0 <- step("flights arriving at airport", f[Dest == ap])
  c0 <- step("operated, arrival delay known", c0[!Cancelled & !Diverted & !is.na(ArrDelay)])
  c0 <- step("valid timestamps", c0[!time_unknown & !arr_inconsistent])
  c0 <- step("aircraft has a next flight", c0[!is.na(nxt_origin) & !nxt_unknown])
  c0 <- step("next flight leaves same airport, same airline", c0[nxt_origin == Dest & nxt_airline == Operating_Airline])
  c0[, sched_turnaround := (nxt_dep_utc - arr_utc) / 60]
  c0 <- step(sprintf("scheduled turnaround %d-%d min", MIN_TURN, MAX_TURN),
             c0[sched_turnaround >= MIN_TURN & sched_turnaround <= MAX_TURN])

  # prediction time = actual arrival; label
  c0[, pred_sec := arr_utc + ArrDelay * 60]
  c0[, time_remaining := (nxt_dep_utc - pred_sec) / 60]
  c0 <- step("arrival delay >= threshold (eligible)", c0[ArrDelay >= DELAY_THRESH])
  n_next_cancel <- c0[nxt_cancel == TRUE, .N]
  c0[, recovered := fifelse(nxt_cancel | is.na(nxt_depdelay), NA_integer_, as.integer(nxt_depdelay < DELAY_THRESH))]
  c0 <- step("next flight operated (label defined)", c0[!is.na(recovered)])

  # traffic feature + coverage
  w_sec <- TRAFFIC_WIN_MIN * 60
  c0[, traffic_next := findInterval(pred_sec + w_sec, ev) - findInterval(pred_sec, ev)]
  c0 <- step("traffic window inside loaded data", c0[(pred_sec + w_sec) <= cov_end])

  # weather: latest observation at or before prediction time, max age
  idx <- findInterval(c0$pred_sec, wxa$wx_sec); idx[idx == 0L] <- NA_integer_
  age <- (c0$pred_sec - wxa$wx_sec[idx]) / 60
  ok  <- !is.na(idx) & age <= WX_MAX_AGE_MIN
  c0[, wx_age_min := fifelse(ok, age, NA_real_)]
  for (v in c("tmp_c", "wnd_ms", "vis_km", "ceil_m", "flight_cat")) { val <- wxa[[v]][idx]; val[!ok] <- NA; set(c0, j = v, value = val) }
  c0 <- step("weather observation within max age", c0[!is.na(wx_age_min)])
  c0 <- step("complete weather (temp, wind, visibility)", c0[!is.na(tmp_c) & !is.na(wnd_ms) & !is.na(vis_km)])

  # modelling columns
  loc <- as.POSIXlt(.POSIXct(c0$pred_sec, tz = ap_tz))
  c0[, `:=`(airport = ap, prev_origin = Origin,
            pred_date = as.Date(format(.POSIXct(pred_sec, tz = ap_tz), "%Y-%m-%d")),
            pred_year = loc$year + 1900L, pred_hour = loc$hour,
            arr_delay_h = ArrDelay / 60, turn_h = sched_turnaround / 60)]
  keep <- c("airport", "FlightDate", "Operating_Airline", "Tail_Number", "prev_origin", "ArrDelay", "sched_turnaround",
            "time_remaining", "recovered", "pred_date", "pred_year", "pred_hour", "arr_delay_h", "turn_h",
            "tmp_c", "wnd_ms", "vis_km", "ceil_m", "flight_cat", "traffic_next", "wx_age_min")
  list(conn = c0[, ..keep], funnel = rbindlist(funnel), next_cancelled = data.table(airport = ap, n = n_next_cancel))
}

res <- lapply(names(AIRPORTS), function(ap) { message("Building ", ap); build_airport(ap) })
res <- Filter(Negate(is.null), res)
conn   <- rbindlist(lapply(res, `[[`, "conn"))
funnel <- rbindlist(lapply(res, `[[`, "funnel"))
saveRDS(conn, file.path(OUT_DIR, "connections.rds"))

funnel[, step := factor(step, levels = unique(step))]
fw <- dcast(funnel, step ~ airport, value.var = "n"); fwrite(fw, file.path(RESULTS_DIR, "connection_funnel.csv"))
cat("\n--- Connection funnel (rows remaining after each step) ---\n"); print(fw)
nc <- rbindlist(lapply(res, `[[`, "next_cancelled")); fwrite(nc, file.path(RESULTS_DIR, "eligible_next_flight_cancelled.csv"))
cat("\nEligible connections whose NEXT flight was cancelled (excluded from main analysis):\n"); print(nc)

cat("\nFinal modelling rows:", format(nrow(conn), big.mark = ","), "\n")
cat("\nRecovery rate by airport and year:\n")
print(dcast(conn[, .(rate = round(mean(recovered), 3)), by = .(airport, pred_year)], airport ~ pred_year, value.var = "rate"))
cat("\nConnections by airport and flight category (low-visibility coverage):\n")
print(dcast(conn[, .N, by = .(airport, flight_cat)], airport ~ flight_cat, value.var = "N", fill = 0))
fwrite(conn[, .(n = .N, recovery_rate = mean(recovered)), by = .(airport, pred_year, flight_cat)][order(airport, pred_year, flight_cat)],
       file.path(RESULTS_DIR, "recovery_by_airport_year_weather.csv"))
