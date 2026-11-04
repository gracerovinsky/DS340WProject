library(data.table)

saved <- readRDS(path.expand(
  "~/Downloads/Atlanta_Recovery_EvaluationReady_Jan2019.rds"
))

test <- copy(saved$test_data)

probability_logit <- function(p) {
  qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
}

# Original model predictions.
test[, baseline_raw := predict(
  saved$baseline_model, newdata = test, type = "response"
)]

test[, expanded_raw := predict(
  saved$expanded_model, newdata = test, type = "response"
)]

# Apply calibrators fitted on January 20–25.
test[, baseline_calibrated := predict(
  saved$baseline_calibrator,
  newdata = data.frame(
    baseline_logit = probability_logit(baseline_raw)
  ),
  type = "response"
)]

test[, expanded_calibrated := predict(
  saved$expanded_calibrator,
  newdata = data.frame(
    expanded_logit = probability_logit(expanded_raw)
  ),
  type = "response"
)]

test[, constant_reference := saved$training_recovery_rate]

prediction_columns <- c(
  "constant_reference",
  "baseline_raw",
  "baseline_calibrated",
  "expanded_raw",
  "expanded_calibrated"
)

stopifnot(
  !anyNA(test[, ..prediction_columns]),
  !anyNA(test$recovered)
)

# Long format for consistent comparisons.
results <- melt(
  test,
  id.vars = c("record_id", "recovered", "traffic_group"),
  measure.vars = prediction_columns,
  variable.name = "model",
  value.name = "probability"
)

overall <- results[, .(
  connections = .N,
  observed_recovery = mean(recovered),
  mean_prediction = mean(probability),
  brier = mean((probability - as.numeric(recovered))^2)
), by = model]

by_traffic <- results[, .(
  connections = .N,
  observed_recovery = mean(recovered),
  mean_prediction = mean(probability),
  brier = mean((probability - as.numeric(recovered))^2)
), by = .(traffic_group, model)]

cat("\nPILOT TEST RESULTS — January 26–31:\n")
print(overall)

cat("\nPILOT TEST RESULTS BY SCHEDULED TRAFFIC:\n")
print(by_traffic)

saveRDS(
  list(
    predictions = test,
    overall = overall,
    by_traffic = by_traffic
  ),
  path.expand("~/Downloads/Atlanta_Recovery_TestResults_Jan2019.rds")
)

cat("\nPilot test results saved.\n")





# Compare predicted and observed recovery within probability ranges.
results[, probability_bin := cut(
  probability,
  breaks = seq(0, 1, by = 0.2),
  include.lowest = TRUE
)]

reliability <- results[
  model != "constant_reference",
  .(
    connections = .N,
    mean_prediction = mean(probability),
    observed_recovery = mean(recovered)
  ),
  by = .(model, probability_bin)
][order(model, probability_bin)]

cat("\nTEST RELIABILITY TABLE:\n")
print(reliability, nrows = 30)






plot_path <- path.expand("~/Downloads/Atlanta_Reliability_Jan2019.png")

png(plot_path, width = 1500, height = 750, res = 150)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

for (prefix in c("baseline", "expanded")) {
  plot(
    NA_real_, NA_real_,
    xlim = c(0, 1), ylim = c(0, 1), asp = 1,
    xlab = "Mean predicted recovery probability",
    ylab = "Observed recovery rate",
    main = if (prefix == "baseline") "Baseline" else "Weather + traffic"
  )
  abline(0, 1, lty = 2, col = "gray60")

  styles <- c("raw", "calibrated")
  colors <- c("#1565A8", "#B34826")
  symbols <- c(16, 17)

  for (i in seq_along(styles)) {
    model_name <- paste(prefix, styles[i], sep = "_")
    bins <- reliability[model == model_name]

    points(
      bins$mean_prediction, bins$observed_recovery,
      pch = symbols[i], col = colors[i],
      cex = 0.7 + sqrt(bins$connections) / 10
    )
  }

  legend(
    "topleft", c("Original", "Recalibrated"),
    col = colors, pch = symbols, bty = "n"
  )
}

invisible(dev.off())
cat("\nReliability plot saved to:", plot_path, "\n")