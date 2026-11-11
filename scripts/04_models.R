# 04_models.R
# Baseline vs expanded models across airports; train on TRAIN_YEARS, test on each of TEST_YEARS.
# Outputs: results/*.csv, figures/*.png, <OUT_DIR>/models.rds
source("config.R"); source("R/utils.R")
set.seed(SEED)

d <- readRDS(file.path(OUT_DIR, "connections.rds"))
d[, vis_km := pmin(vis_km, 16)]                              # ASOS reports cap at ~16 km
airports <- sort(unique(d$airport)); d[, airport := factor(airport, levels = airports)]
tr <- d[pred_year %in% TRAIN_YEARS]

# traffic relative to the airport's typical (training-year median) scheduled traffic
med_tr <- tr[, .(med_traffic = max(median(traffic_next), 1)), by = airport]
d[med_tr, on = "airport", traffic_rel := traffic_next / i.med_traffic]
tr <- d[pred_year %in% TRAIN_YEARS]; te <- d[pred_year %in% TEST_YEARS]
cat("Airports:", paste(airports, collapse = ", "), "\nTrain rows:", nrow(tr), " Test rows:", nrow(te), "\n")
stopifnot(nrow(tr) > 100, nrow(te) > 100, length(unique(tr$recovered)) == 2)

base_x <- c("arr_delay_h", "turn_h")
full_x <- c(base_x, "tmp_c", "wnd_ms", "vis_km", "traffic_rel")
fm <- function(x, ap = TRUE) as.formula(paste("recovered ~", paste(c(if (ap) "airport", x), collapse = " + ")))

models <- list(baseline_logit = glm(fm(base_x), data = tr, family = binomial()),
               expanded_logit = glm(fm(full_x), data = tr, family = binomial()))
sink(file.path(RESULTS_DIR, "logit_coefficients.txt"))
for (nm in names(models)) { cat("\n====", nm, "====\n"); print(summary(models[[nm]])) }
sink()

feats <- list(baseline_logit = base_x, expanded_logit = full_x, baseline_xgb = base_x, expanded_xgb = full_x)
if (requireNamespace("xgboost", quietly = TRUE)) {                 # optional gradient boosting
  fit_xgb <- function(x) xgboost::xgb.train(
    params = list(objective = "binary:logistic", max_depth = 4, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8, nthread = 4),
    data = xgboost::xgb.DMatrix(design_matrix(tr, x, airports), label = tr$recovered), nrounds = 300, verbose = 0)
  models$baseline_xgb <- fit_xgb(base_x); models$expanded_xgb <- fit_xgb(full_x)
}
for (nm in names(models)) te[, (nm) := predict_model(models[[nm]], nm, te, feats, airports)]
te[, constant := mean(tr$recovered)]
model_names <- c("constant", names(models))

# ---- 1. overall metrics: by year, and by airport ----------------------
mt <- function(s, extra) rbindlist(lapply(model_names, function(m) c(extra, list(model = m), metrics_row(s[[m]], s$recovered))))
res_year <- rbindlist(lapply(TEST_YEARS, function(yr) mt(te[pred_year == yr], list(year = yr))))
res_ap   <- rbindlist(lapply(airports,   function(a)  mt(te[airport == a],    list(airport = a))))
fwrite(res_year, file.path(RESULTS_DIR, "metrics_by_year.csv")); fwrite(res_ap, file.path(RESULTS_DIR, "metrics_by_airport.csv"))
r4 <- function(x) x[, lapply(.SD, function(z) if (is.numeric(z)) round(z, 4) else z)]
cat("\n--- Metrics by test year ---\n"); print(r4(res_year)); cat("\n--- Metrics by airport (all test years) ---\n"); print(r4(res_ap))

# ---- 2. day-level bootstrap of Brier(expanded) - Brier(baseline) ------
boot_delta <- function(s, m_base = "baseline_logit", m_exp = "expanded_logit", B = N_BOOT) {
  s <- copy(s); s[, `:=`(se_b = (get(m_base) - recovered)^2, se_e = (get(m_exp) - recovered)^2)]
  a <- s[, .(n = .N, sb = sum(se_b), se = sum(se_e)), by = pred_date]       # resample whole DAYS (all airports together)
  dl <- replicate(B, { i <- sample.int(nrow(a), replace = TRUE); (sum(a$se[i]) - sum(a$sb[i])) / sum(a$n[i]) })
  c(delta = (sum(a$se) - sum(a$sb)) / sum(a$n), lo = unname(quantile(dl, .025)), hi = unname(quantile(dl, .975)))
}
bt <- rbindlist(c(lapply(TEST_YEARS, function(yr) data.table(group = as.character(yr), t(boot_delta(te[pred_year == yr])))),
                  list(data.table(group = "ALL", t(boot_delta(te))))))
fwrite(bt, file.path(RESULTS_DIR, "bootstrap_brier_delta_by_year.csv"))
cat("\n--- Brier(expanded) - Brier(baseline); negative = weather/traffic helps ---\n"); print(bt)

# ---- 3. performance across operating conditions (test set pooled) -----
qq <- quantile(tr$traffic_rel, c(.25, .5, .75))
te[, traffic_q := cut(traffic_rel, c(-Inf, qq, Inf), labels = c("Q1 low", "Q2", "Q3", "Q4 high"))]
te[, hour_bin  := cut(pred_hour, c(-1, 5, 11, 17, 23), labels = c("00-05", "06-11", "12-17", "18-23"))]
te[, delay_bin := cut(ArrDelay, c(14, 30, 60, 120, Inf), labels = c("15-30", "31-60", "61-120", ">120"))]
sub_tab <- function(var) rbindlist(lapply(levels(droplevels(factor(te[[var]]))), function(l) {
  s <- te[as.character(get(var)) == l]; if (nrow(s) < 50) return(NULL)
  b <- boot_delta(s, B = 300)
  data.table(variable = var, level = l, n = nrow(s), recovery_rate = round(mean(s$recovered), 3),
             brier_baseline = round(brier(s$baseline_logit, s$recovered), 4), brier_expanded = round(brier(s$expanded_logit, s$recovered), 4),
             auc_baseline = round(auc_roc(s$baseline_logit, s$recovered), 3), auc_expanded = round(auc_roc(s$expanded_logit, s$recovered), 3),
             delta_brier = round(b["delta"], 4), ci_lo = round(b["lo"], 4), ci_hi = round(b["hi"], 4))
}))
sub <- rbindlist(lapply(c("airport", "flight_cat", "traffic_q", "hour_bin", "delay_bin"), sub_tab))
fwrite(sub, file.path(RESULTS_DIR, "performance_by_condition.csv"))
cat("\n--- Performance by operating condition ---\n"); print(sub)

# weather effect inside each airport x flight category is in recovery_by_airport_year_weather.csv (script 03)

# ---- 4. leave-one-airport-out: does the model transfer to an unseen airport? ----
loao <- rbindlist(lapply(airports, function(a) {
  trn <- tr[airport != a]; tst <- te[airport == a]
  if (nrow(tst) < 50 || nrow(trn) < 100) return(NULL)
  mb <- glm(fm(base_x, FALSE), trn, family = binomial()); me <- glm(fm(full_x, FALSE), trn, family = binomial())
  tst[, `:=`(baseline_logit = predict(mb, tst, type = "response"), expanded_logit = predict(me, tst, type = "response"))]
  b <- boot_delta(tst, B = 300)
  data.table(held_out_airport = a, n = nrow(tst), brier_baseline = round(brier(tst$baseline_logit, tst$recovered), 4),
             brier_expanded = round(brier(tst$expanded_logit, tst$recovered), 4), delta_brier = round(b["delta"], 4),
             ci_lo = round(b["lo"], 4), ci_hi = round(b["hi"], 4))
}))
fwrite(loao, file.path(RESULTS_DIR, "leave_one_airport_out.csv")); cat("\n--- Leave-one-airport-out (no airport term) ---\n"); print(loao)

# ---- 5. figures -------------------------------------------------------
png(file.path(FIG_DIR, "reliability.png"), width = 1600, height = 600, res = 150)
par(mfrow = c(1, length(TEST_YEARS)), mar = c(4, 4, 3, 1))
for (yr in TEST_YEARS) {
  s <- te[pred_year == yr]
  plot(0:1, 0:1, type = "l", lty = 2, col = "grey50", xlab = "Predicted P(recovery)", ylab = "Observed recovery rate", main = yr)
  for (k in 1:2) {
    m <- c("baseline_logit", "expanded_logit")[k]
    b <- cut(s[[m]], unique(quantile(s[[m]], 0:10 / 10)), include.lowest = TRUE)
    pts <- s[, .(x = mean(get(m)), y = mean(recovered)), by = .(bin = b)][order(x)]
    points(pts$x, pts$y, pch = c(16, 17)[k], col = c("black", "red")[k]); lines(pts$x, pts$y, col = c("black", "red")[k])
  }
  if (yr == TEST_YEARS[1]) legend("topleft", c("baseline", "expanded"), pch = c(16, 17), col = c("black", "red"), bty = "n")
}
dev.off()

png(file.path(FIG_DIR, "delta_brier_by_condition.png"), width = 1500, height = 1100, res = 150)
sub[, lab := paste0(variable, ": ", level)]; yy <- rev(seq_len(nrow(sub)))
par(mar = c(4, 12, 2, 1))
plot(sub$delta_brier, yy, xlim = range(c(sub$ci_lo, sub$ci_hi, 0)), yaxt = "n", pch = 16, ylab = "",
     xlab = "Brier(expanded) - Brier(baseline)   (<0 = weather/traffic helps)")
segments(sub$ci_lo, yy, sub$ci_hi, yy); abline(v = 0, lty = 2); axis(2, yy, sub$lab, las = 1, cex.axis = .7)
dev.off()

png(file.path(FIG_DIR, "brier_by_year.png"), width = 1000, height = 700, res = 150)
mat <- sapply(c("constant", "baseline_logit", "expanded_logit"), function(m) res_year[model == m]$brier)
barplot(t(mat), beside = TRUE, names.arg = TEST_YEARS, legend.text = c("constant", "baseline", "expanded"),
        args.legend = list(x = "topleft", bty = "n"), ylab = "Brier score (lower = better)", main = "Held-out performance by year")
dev.off()

png(file.path(FIG_DIR, "recovery_by_airport_and_visibility.png"), width = 1200, height = 700, res = 150)
rv <- d[, .(rate = mean(recovered), n = .N), by = .(airport, flight_cat)][n >= 30]
cats <- c("VFR", "MVFR", "IFR", "LIFR"); mat <- sapply(cats, function(cc) sapply(airports, function(a) { z <- rv[airport == a & flight_cat == cc]$rate; if (length(z)) z else NA_real_ }))
barplot(t(mat), beside = TRUE, names.arg = airports, legend.text = cats, args.legend = list(x = "topright", bty = "n"),
        ylab = "Recovery rate", main = "Delay recovery by airport and visibility category (cells with n>=30)")
dev.off()

saveRDS(list(models = models, feats = feats, airports = airports, med_traffic = med_tr), file.path(OUT_DIR, "models.rds"))
saveRDS(te[, .(pred_date, pred_year, airport, ArrDelay, sched_turnaround, arr_delay_h, turn_h, tmp_c, wnd_ms, vis_km,
               traffic_next, traffic_rel, flight_cat, recovered)], file.path(OUT_DIR, "test_sample_for_demo.rds"))
cat("\nDone. See results/ and figures/\n")
