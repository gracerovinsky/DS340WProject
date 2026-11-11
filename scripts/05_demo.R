# 05_demo.R  --  live-demo script
#  Mode A (what-if):  Rscript scripts/05_demo.R airport=ORD arr_delay=45 turnaround=75 temp=-3 wind=8 vis=1.5 movements=60
#  Mode B (replay):   Rscript scripts/05_demo.R replay=8     # random held-out connections: prediction vs. what happened
source("config.R"); source("R/utils.R")
M <- readRDS(file.path(OUT_DIR, "models.rds"))
args <- commandArgs(trailingOnly = TRUE)
kv <- setNames(sub(".*=", "", args), sub("=.*", "", args))
g  <- function(k, default) if (!is.na(kv[k])) as.numeric(kv[k]) else default
show <- function(newd) {
  for (nm in names(M$models)) newd[[paste0("p_", nm)]] <- round(predict_model(M$models[[nm]], nm, newd, M$feats, M$airports), 3)
  newd
}
if (!is.na(kv["replay"])) {
  s <- readRDS(file.path(OUT_DIR, "test_sample_for_demo.rds"))
  s <- s[sample.int(nrow(s), as.integer(kv["replay"]))]
  out <- show(s)
  print(out[, c("pred_date", "airport", "ArrDelay", "sched_turnaround", "flight_cat", "traffic_next",
                grep("^p_", names(out), value = TRUE), "recovered"), with = FALSE])
  cat("(recovered: 1 = next departure < 15 min late)\n")
} else {
  ap <- if (!is.na(kv["airport"])) toupper(kv["airport"]) else "ATL"
  stopifnot(ap %in% M$airports)
  mv  <- g("movements", M$med_traffic[airport == ap]$med_traffic)
  newd <- data.table(airport = ap, arr_delay_h = g("arr_delay", 45) / 60, turn_h = g("turnaround", 75) / 60,
                     tmp_c = g("temp", 10), wnd_ms = g("wind", 4), vis_km = pmin(g("vis", 16), 16),
                     traffic_rel = mv / M$med_traffic[airport == ap]$med_traffic)
  cat(sprintf("%s: aircraft arrives %g min late, %g min scheduled turnaround, vis %g km, %g scheduled movements next hour.\n",
              ap, g("arr_delay", 45), g("turnaround", 75), g("vis", 16), mv))
  print(show(newd)); cat("(p_* = predicted probability the next departure is < 15 min late)\n")
}
