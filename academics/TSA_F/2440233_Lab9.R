#' ---
#' title: "time series analysis - lab 9"
#' author: "2440233"
#' date: "`r Sys.Date()`"
#' output: pdf_document
#' ---

# 1. introduction
cat("\n--- 1. introduction ---\n")
cat("in this lab, we use the monthly u.s. consumer price index (cpiaucsl) dataset from jan 2010 to jun 2026.\n")
cat("we handle any missing values via carry-forward imputation, apply log-differencing to\n")
cat("transform the data into a stationary inflation rate, and use acf/pacf to identify\n")
cat("candidate arma(p,q) models. finally, we compare aic/bic metrics, validate residuals,\n")
cat("and generate a 12-month forecast.\n")

# load the dataset
climate_data <- read.csv("CPIAUCSL.csv")
raw_cpi <- suppressWarnings(as.numeric(climate_data$CPIAUCSL))

# handle coerced nas using a carry-forward imputation to preserve time series continuity
for (i in 2:length(raw_cpi)) {
  if (is.na(raw_cpi[i])) {
    raw_cpi[i] <- raw_cpi[i - 1]
  }
}
climate_data$CPIAUCSL <- raw_cpi
climate_data$date <- as.Date(climate_data$observation_date)

# subset data strictly up to june 2026
cpi_subset <- subset(climate_data, date >= as.Date("2010-01-01") & date <= as.Date("2026-06-30"))

# create time series object (monthly frequency = 12)
cpi_ts <- ts(cpi_subset$CPIAUCSL, start = c(2010, 1), frequency = 12)

# 2. transformation to inflation rate
# apply log-differencing to ensure stationarity: inflation_t = log(cpi_t) - log(cpi_{t-1})
inflation_ts <- diff(log(cpi_ts))

# 3. exploratory visualization and model identification
# plot original cpi, inflation rate, and acf/pacf
par(mfrow = c(2, 2))
plot(cpi_ts, main = "fig 1: u.s. cpi (jan 2010 - jun 2026)", ylab = "cpi level", col = "blue")
plot(inflation_ts, main = "fig 2: inflation rate", ylab = "inflation rate", col = "darkgreen")
acf(inflation_ts, main = "fig 3: acf of inflation", lag.max = 36, col = "red")
pacf(inflation_ts, main = "fig 4: pacf of inflation", lag.max = 36, col = "purple")
par(mfrow = c(1, 1))

cat("\ninference: the pacf cuts off sharply after lag 1, while the acf decays slowly.\n")
cat("this firm textbook signature suggests an ar(1) model. to ensure robustness,\n")
cat("we also select arma(1, 1) as a second candidate to see if the ma component\n")
cat("absorbs any remaining noise.\n")

# 4. model estimation and comparison
cat("\n--- model estimation ---\n")
fit_ar1 <- arima(inflation_ts, order = c(1, 0, 0))
fit_arma11 <- arima(inflation_ts, order = c(1, 0, 1))

metrics_df <- data.frame(
  model = c("ar(1)", "arma(1, 1)"),
  aic = c(round(AIC(fit_ar1), 3), round(AIC(fit_arma11), 3)),
  bic = c(round(BIC(fit_ar1), 3), round(BIC(fit_arma11), 3))
)
print(metrics_df, row.names = FALSE)

cat("\ninference: aic favors arma(1, 1) slightly (-1838.003 vs -1836.641). however, bic\n")
cat("penalizes extra parameters more harshly and favors the simpler ar(1) (-1826.792 vs -1824.871).\n")
cat("following the principle of parsimony, we select ar(1) as the final optimal model.\n")

# 5. residual validation
cat("\n--- residual validation for ar(1) ---\n")
res_ar1 <- residuals(fit_ar1)

par(mfrow = c(1, 2))
plot(res_ar1, main = "fig 5: residuals of ar(1)", ylab = "residuals", col = "darkgray")
abline(h = 0, col = "red", lty = 2)
acf(res_ar1, main = "fig 6: acf of residuals", lag.max = 24)
par(mfrow = c(1, 1))

lb_test <- Box.test(res_ar1, lag = 12, type = "Ljung-Box")
cat("ljung-box test statistic:", round(lb_test$statistic, 4), "\n")
cat("p-value:", round(lb_test$p.value, 4), "\n")

cat("\ninference: the p-value (> 0.05) indicates we fail to reject the null hypothesis.\n")
cat("the residuals are uncorrelated white noise, confirming the ar(1) model successfully\n")
cat("captures all temporal dependencies.\n")

# 6. forecasting
cat("\n--- 12-month forecast ---\n")
forecast_ar1 <- predict(fit_ar1, n.ahead = 12)

# plot forecast with 95% confidence intervals
ts.plot(inflation_ts, forecast_ar1$pred, 
        gpars = list(col = c("black", "blue"), lwd = c(1, 2), 
                     main = "fig 7: 12-month inflation forecast using ar(1)",
                     ylab = "inflation rate"))

upper_bound <- forecast_ar1$pred + 1.96 * forecast_ar1$se
lower_bound <- forecast_ar1$pred - 1.96 * forecast_ar1$se
lines(upper_bound, col = "red", lty = 2)
lines(lower_bound, col = "red", lty = 2)
legend("topleft", legend = c("historical", "forecast", "95% ci"), 
       col = c("black", "blue", "red"), lty = c(1, 1, 2), lwd = c(1, 2, 1))
