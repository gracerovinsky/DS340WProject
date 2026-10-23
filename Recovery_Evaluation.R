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