library(data.table)

cohort <- readRDS(path.expand(
  "~/Downloads/Atlanta_Recovery_Weather_Traffic_Jan2019.rds"
))

# Use the same eligible connections for all model comparisons.
model_data <- copy(cohort[traffic_window_covered == TRUE])

# Recreate local dates so this script works independently.
model_data[, prediction_date_atl := as.IDate(format(
  prediction_time_utc,
  tz = "America/New_York",
  format = "%Y-%m-%d"
))]

model_data[, dataset := fcase(
  prediction_date_atl <= as.IDate("2019-01-19"), "training",
  prediction_date_atl <= as.IDate("2019-01-25"), "calibration",
  default = "test"
)]

model_data[, dataset := factor(
  dataset,
  levels = c("training", "calibration", "test")
)]

cat("\nChronological split:\n")
print(model_data[, .(
  connections = .N,
  first_date = min(prediction_date_atl),
  last_date = max(prediction_date_atl),
  recovered = sum(recovered),
  not_recovered = sum(!recovered)
), by = dataset][order(dataset)])

# Verify that both outcomes occur in each set.
stopifnot(
  model_data[, all(
    c("training", "calibration", "test") %in% dataset
  )],
  model_data[, .(outcomes = uniqueN(recovered)),
             by = dataset][, all(outcomes == 2)]
)

# Save the split before fitting models.
split_path <- path.expand(
  "~/Downloads/Atlanta_Recovery_ModelSplit_Jan2019.rds"
)

saveRDS(model_data, split_path)
cat("\nModel split saved to:", split_path, "\n")




# Separate the three chronological sets.
train_data <- copy(model_data[dataset == "training"])
calibration_data <- copy(model_data[dataset == "calibration"])
test_data <- copy(model_data[dataset == "test"])

# Express these predictors in hours for readable coefficients.
for (data_part in list(
  train_data, calibration_data, test_data
)) {
  data_part[, incoming_delay_hours := ArrDelay / 60]
  data_part[, scheduled_turnaround_hours := scheduled_turnaround / 60]
}

baseline_predictors <- c(
  "incoming_delay_hours",
  "scheduled_turnaround_hours"
)

# Check training inputs before fitting.
stopifnot(
  !anyNA(train_data[, ..baseline_predictors]),
  !anyNA(train_data$recovered)
)

# Fit using training data only.
baseline_model <- glm(
  recovered ~ incoming_delay_hours + scheduled_turnaround_hours,
  data = train_data,
  family = binomial(),
  na.action = na.fail
)

cat("\nBaseline model converged:",
    baseline_model$converged, "\n")

cat("\nTraining records used:",
    nobs(baseline_model), "\n")

cat("\nBaseline coefficients:\n")
print(coef(summary(baseline_model)))

# Generate probabilities for the calibration set.
calibration_data[, baseline_probability := predict(
  baseline_model,
  newdata = calibration_data,
  type = "response"
)]

cat("\nCalibration-set predicted probabilities:\n")
print(summary(calibration_data$baseline_probability))

cat("\nCalibration-set Brier score:\n")
print(calibration_data[, mean(
  (baseline_probability - as.numeric(recovered))^2
)])

# Reference: predict the training recovery rate for everyone.
training_recovery_rate <- mean(train_data$recovered)

cat("\nConstant-probability reference Brier score:\n")
print(calibration_data[, mean(
  (training_recovery_rate - as.numeric(recovered))^2
)])

# Save the fitted model and separate datasets.
saveRDS(
  list(
    baseline_model = baseline_model,
    training_recovery_rate = training_recovery_rate,
    train_data = train_data,
    calibration_data = calibration_data,
    test_data = test_data
  ),
  path.expand("~/Downloads/Atlanta_Recovery_Baseline_Jan2019.rds")
)

cat("\nBaseline checkpoint saved.\n")




# Scale additional predictors into convenient units.
for (data_part in list(
  train_data, calibration_data, test_data
)) {
  data_part[, visibility_km := visibility_m / 1000]
  data_part[, scheduled_movements_tens :=
              scheduled_movements_next60 / 10]
}

expanded_predictors <- c(
  baseline_predictors,
  "temperature_c",
  "wind_speed_ms",
  "visibility_km",
  "scheduled_movements_tens"
)

# Require complete inputs for this comparison.
stopifnot(
  !anyNA(train_data[, ..expanded_predictors]),
  !anyNA(calibration_data[, ..expanded_predictors])
)

expanded_model <- glm(
  recovered ~ incoming_delay_hours +
    scheduled_turnaround_hours +
    temperature_c +
    wind_speed_ms +
    visibility_km +
    scheduled_movements_tens,
  data = train_data,
  family = binomial(),
  na.action = na.fail
)

cat("\nExpanded model converged:",
    expanded_model$converged, "\n")

cat("\nExpanded model training records:",
    nobs(expanded_model), "\n")

cat("\nExpanded model coefficients:\n")
print(coef(summary(expanded_model)))

calibration_data[, expanded_probability := predict(
  expanded_model,
  newdata = calibration_data,
  type = "response"
)]

cat("\nCalibration-set Brier comparison:\n")
print(calibration_data[, .(
  constant_reference = mean(
    (training_recovery_rate - as.numeric(recovered))^2
  ),
  baseline = mean(
    (baseline_probability - as.numeric(recovered))^2
  ),
  weather_and_traffic = mean(
    (expanded_probability - as.numeric(recovered))^2
  )
)])

saveRDS(
  list(
    baseline_model = baseline_model,
    expanded_model = expanded_model,
    training_recovery_rate = training_recovery_rate,
    train_data = train_data,
    calibration_data = calibration_data,
    test_data = test_data
  ),
  path.expand("~/Downloads/Atlanta_Recovery_ModelComparison_Jan2019.rds")
)

cat("\nModel comparison checkpoint saved.\n")




# Convert probabilities to log-odds, avoiding exact 0 or 1.
probability_logit <- function(p) {
  qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
}

calibration_data[, baseline_logit :=
  probability_logit(baseline_probability)]

calibration_data[, expanded_logit :=
  probability_logit(expanded_probability)]

# Fit recalibration using ONLY the calibration period.
baseline_calibrator <- glm(
  recovered ~ baseline_logit,
  data = calibration_data,
  family = binomial(),
  na.action = na.fail
)

expanded_calibrator <- glm(
  recovered ~ expanded_logit,
  data = calibration_data,
  family = binomial(),
  na.action = na.fail
)

cat("\nCalibrator convergence:\n")
print(c(
  baseline = baseline_calibrator$converged,
  expanded = expanded_calibrator$converged
))

cat("\nBaseline recalibration coefficients:\n")
print(coef(summary(baseline_calibrator)))

cat("\nExpanded recalibration coefficients:\n")
print(coef(summary(expanded_calibrator)))

saveRDS(
  list(
    baseline_model = baseline_model,
    expanded_model = expanded_model,
    baseline_calibrator = baseline_calibrator,
    expanded_calibrator = expanded_calibrator,
    training_recovery_rate = training_recovery_rate,
    train_data = train_data,
    calibration_data = calibration_data,
    test_data = test_data
  ),
  path.expand("~/Downloads/Atlanta_Recovery_CalibratedModels_Jan2019.rds")
)

cat("\nCalibrated models saved. Test set remains unevaluated.\n")




# Fix subgroup definitions before test evaluation.
traffic_threshold <- median(
  train_data$scheduled_movements_next60
)

visibility_threshold_m <- 5000

for (data_part in list(
  train_data, calibration_data, test_data
)) {
  data_part[, visibility_group := fifelse(
    visibility_m < visibility_threshold_m,
    "Below 5 km",
    "At least 5 km"
  )]

  data_part[, traffic_group := fifelse(
    scheduled_movements_next60 > traffic_threshold,
    "Above training median",
    "At or below training median"
  )]
}

cat("\nTraining-derived traffic threshold:",
    traffic_threshold, "scheduled movements\n")

# Inspect subgroup sizes without calculating test performance.
cat("\nTest visibility-group sizes:\n")
print(test_data[, .N, by = visibility_group])

cat("\nTest traffic-group sizes:\n")
print(test_data[, .N, by = traffic_group])

cat("\nTest combined subgroup sizes:\n")
print(test_data[
  , .N, by = .(visibility_group, traffic_group)
])

saveRDS(
  list(
    baseline_model = baseline_model,
    expanded_model = expanded_model,
    baseline_calibrator = baseline_calibrator,
    expanded_calibrator = expanded_calibrator,
    training_recovery_rate = training_recovery_rate,
    traffic_threshold = traffic_threshold,
    visibility_threshold_m = visibility_threshold_m,
    train_data = train_data,
    calibration_data = calibration_data,
    test_data = test_data
  ),
  path.expand("~/Downloads/Atlanta_Recovery_EvaluationReady_Jan2019.rds")
)

cat("\nEvaluation checkpoint saved.\n")