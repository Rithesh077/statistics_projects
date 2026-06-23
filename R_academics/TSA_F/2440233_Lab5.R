#' ---
#' title: "time series analysis - lab 5"
#' author: "2440233"
#' date: "`r Sys.Date()`"
#' output: pdf_document
#' ---

# 1.introduction
# in this lab, we analyze a 10-year monthly electricity consumption dataset.
# we will construct a time series object, visualize it, examine trend,
# seasonality, and random fluctuations, apply 3-month and 5-month moving averages,
# implement simple exponential smoothing (ses) to forecast the next six months,
# and evaluate performance using mae, mse, and rmse.

# 2. data overview
# the dataset contains monthly electricity consumption in million kwh
# from january 2015 to december 2024 (120 observations).
# the data exhibits a linear trend of +8 units per year (+0.667 units per month)
# and a strong, repeating monthly seasonal pattern.

# 3. methodology and concepts covered
# time series object creation: converting raw vectors into a ts object in r.
# exploratory visualization: line plotting to identify patterns.
# moving average (ma) smoothing: linear filtering to smooth out random fluctuations.
# simple exponential smoothing (ses): a smoothing method that assigns exponentially
# decreasing weights to past observations.
# forecasting performance metrics: mean absolute error (mae), mean squared error (mse),
# and root mean squared error (rmse) for comparison.

# define dataset
consumption <- c(
  420, 435, 450, 470, 495, 520, 540, 535, 510, 485, 460, 445,
  428, 443, 458, 478, 503, 528, 548, 543, 518, 493, 468, 453,
  436, 451, 466, 486, 511, 536, 556, 551, 526, 501, 476, 461,
  444, 459, 474, 494, 519, 544, 564, 559, 534, 509, 484, 469,
  452, 467, 482, 502, 527, 552, 572, 567, 542, 517, 492, 477,
  460, 475, 490, 510, 535, 560, 580, 575, 550, 525, 500, 485,
  468, 483, 498, 518, 543, 568, 588, 583, 558, 533, 508, 493,
  476, 491, 506, 526, 551, 576, 596, 591, 566, 541, 516, 501,
  484, 499, 514, 534, 559, 584, 604, 599, 574, 549, 524, 509,
  492, 507, 522, 542, 567, 592, 612, 607, 582, 557, 532, 517
)

# create time series object
electricity_ts <- ts(consumption, start = c(2015, 1), frequency = 12)

# plot raw data
#+ fig.width=10, fig.height=6
plot(electricity_ts,
     main = "monthly electricity consumption",
     ylab = "consumption (million kwh)",
     xlab = "year",
     col = "blue", lwd = 2)
grid()

# 4. inference for each output
cat("4. inference for each output\n\n")
cat("inference for raw time series plot: the series shows a clear upward trend and monthly seasonality with peaks in july and troughs in january. the trend appears linear and the seasonal amplitude remains constant over time. there is no significant random fluctuation as the progression is highly systematic.\n\n")

# moving average smoothing
ma3_smoothed <- filter(electricity_ts, rep(1/3, 3), sides = 1)
ma5_smoothed <- filter(electricity_ts, rep(1/5, 5), sides = 1)

# plot moving averages
#+ fig.width=10, fig.height=6
plot(electricity_ts, col = "gray80", lwd = 1.5,
     main = "moving average smoothing comparison",
     ylab = "consumption (million kwh)", xlab = "year")
lines(ma3_smoothed, col = "blue", lwd = 2)
lines(ma5_smoothed, col = "red", lwd = 2)
legend("topleft", legend = c("original", "3-month ma", "5-month ma"),
       col = c("gray80", "blue", "red"), lwd = 2, bty = "n")
grid()

cat("inference for moving average comparison plot: the 3-month moving average smooths short-term fluctuations but lags behind the trend. the 5-month moving average is smoother but exhibits a larger lag and dampens seasonal peaks, demonstrating that a larger window increases smoothing at the expense of responsiveness.\n\n")

# simple exponential smoothing (ses)
fit_ses <- HoltWinters(electricity_ts, beta = FALSE, gamma = FALSE)

# print parameters
alpha_val <- fit_ses$alpha
cat("inference for ses parameter: the estimated alpha parameter is:\n")
cat(tolower(paste("alpha:", alpha_val)), "\n")
cat("this shows that because the series has a strong trend and seasonal pattern, the model assigns almost all weight to the most recent observation, effectively acting as a naive one-step-ahead predictor.\n\n")

# generate forecasts for the next six months
forecasts <- predict(fit_ses, n.ahead = 6)
cat("forecasts for the next six months:\n")
forecasts_output <- capture.output(print(forecasts))
cat(tolower(paste(forecasts_output, collapse = "\n")), "\n\n")

cat("inference for forecasts: the forecast for the next six months is flat at 517 million kwh. this is because simple exponential smoothing assumes a constant level and lacks trend or seasonal components, leading to a flat forecast function that does not capture the expected growth or seasonal peaks.\n\n")

# performance metrics comparison
# lag moving averages by 1 to get one-step-ahead forecasts
ma3_forecast <- c(NA, ma3_smoothed[1:(length(electricity_ts) - 1)])
ma5_forecast <- c(NA, ma5_smoothed[1:(length(electricity_ts) - 1)])
ses_forecast <- c(NA, fit_ses$fitted[, "xhat"])

# common evaluation period: index 6 to 120
actual <- electricity_ts[6:120]
ma3_pred <- ma3_forecast[6:120]
ma5_pred <- ma5_forecast[6:120]
ses_pred <- ses_forecast[6:120]

calc_metrics <- function(act, pred) {
  mae <- mean(abs(act - pred))
  mse <- mean((act - pred)^2)
  rmse <- sqrt(mse)
  c(mae = mae, mse = mse, rmse = rmse)
}

metrics_ma3 <- calc_metrics(actual, ma3_pred)
metrics_ma5 <- calc_metrics(actual, ma5_pred)
metrics_ses <- calc_metrics(actual, ses_pred)

metrics_table <- rbind(
  "3-month ma" = metrics_ma3,
  "5-month ma" = metrics_ma5,
  "ses" = metrics_ses
)

cat("performance metrics table:\n")
metrics_output <- capture.output(print(metrics_table))
cat(tolower(paste(metrics_output, collapse = "\n")), "\n\n")

cat("inference for performance metrics: simple exponential smoothing has the lowest mae (19.37), mse (412.01), and rmse (20.30) because it adapts quickly to the most recent data point (alpha approx 1.0). the 3-month moving average performs second best, while the 5-month moving average has the highest errors due to its larger lag.\n\n")

# plot historical data and forecasts
#+ fig.width=10, fig.height=6
plot(electricity_ts, xlim = c(2015, 2025.5), ylim = c(400, 650),
     main = "simple exponential smoothing historical and forecast values",
     ylab = "consumption (million kwh)", xlab = "year", col = "blue", lwd = 2)
lines(fit_ses$fitted[, "xhat"], col = "red", lwd = 1.5)
lines(forecasts, col = "darkgreen", lwd = 2.5, type = "o", pch = 16)
legend("topleft", legend = c("historical", "ses fitted", "6-month forecast"),
       col = c("blue", "red", "darkgreen"), lwd = 2, bty = "n")
grid()

# 5. conclusion
cat("5. conclusion\n\n")
cat("conclusion: the evaluation of forecasting methods on the electricity consumption dataset shows that simple exponential smoothing performs best in terms of mae, mse, and rmse due to its fast adaptation to recent level shifts. however, ses is not suitable for long-term forecasting here because it generates a flat forecast function that fails to capture the prominent upward trend and seasonal patterns. for this data, a multiplicative or additive holt-winters model or a sarima model would be more appropriate. moving average methods are also limited as they lag behind the trend and dampen seasonality, with larger windows increasing these errors.\n")
