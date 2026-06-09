#' ---
#' title: "time series analysis - lab 3"
#' subtitle: "method of differencing, seasonal differencing, and moving average smoothing"
#' author: "2440233"
#' date: "`r Sys.Date()`"
#' output:
#'   pdf_document:
#'     toc: true

#' # introduction
#'
#' the objective of this lab is to analyze the "nottem" dataset which contains
#' the average monthly air temperatures at nottingham castle from 1920 to 1939.
#' we will examine the series for trend, seasonality, and stationarity, and
#' apply techniques like differencing, seasonal differencing, moving average
#' smoothing, log transformation, and acf analysis to understand the series
#' characteristics.

#' # objectives
#'
#' 1. load and visualize the nottem dataset to understand its behavior over time
#' 2. identify trend, seasonal, cyclical, and irregular components
#' 3. perform additive and multiplicative decomposition and compare results
#' 4. assess stationarity by checking if mean and variance are constant
#' 5. apply log transformation and evaluate its effect on variance stabilization
#' 6. generate and interpret the acf plot
#' 7. apply differencing, seasonal differencing, and moving average smoothing

#' # methodology
#'
#' dataset: nottem (built in R dataset)
#' contains monthly average air temperatures (degrees fahrenheit) at
#' nottingham castle, england, from january 1920 to december 1939.
#' that is 240 observations with monthly frequency (12 per year).
#' source: anderson, o. d. (1976)

#' # analysis

#' ## q1: load and visualize the nottem dataset

cat("q1: load and visualize the nottem dataset\n\n")

data("nottem")
nt <- nottem

cat("class:", class(nt), "\n")
cat("start:", start(nt)[1], "/", start(nt)[2], "\n")
cat("end:", end(nt)[1], "/", end(nt)[2], "\n")
cat("frequency:", frequency(nt), "\n")
cat("observations:", length(nt), "\n\n")

cat("first 12 values:\n")
print(head(nt, 12))
cat("\nlast 12 values:\n")
print(tail(nt, 12))

cat("\nsummary:\n")
print(summary(nt))
cat("sd:", round(sd(nt), 2), "\n\n")

#+ fig.width=10, fig.height=6
plot(nt,
     main = "fig 1: monthly average temperature at nottingham (1920-1939)",
     ylab = "temperature (fahrenheit)",
     xlab = "year",
     col = "blue", lwd = 2)
grid()

cat("interpretation of fig 1:\n")
cat("- the series shows a strong seasonal pattern repeating every 12 months\n")
cat("- temperatures peak in summer (jul-aug) and dip in winter (jan-feb)\n")
cat("- there is no obvious long term upward or downward trend\n")
cat("- the seasonal amplitude appears roughly constant across all years\n")
cat("- this suggests an additive model may be appropriate\n\n")

#' ## q2: monthly distribution

cat("q2: monthly distribution of temperatures\n\n")

#+ fig.width=10, fig.height=6
boxplot(nt ~ cycle(nt),
        main = "fig 2: monthly distribution of temperatures",
        xlab = "month", ylab = "temperature (fahrenheit)",
        col = rainbow(12), names = month.abb)

cat("interpretation of fig 2:\n")
cat("- july and august have the highest median temperatures around 60-62 F\n")
cat("- january and february have the lowest around 39-40 F\n")
cat("- the spread (box size) is fairly consistent across months\n")
cat("- consistent spread confirms that an additive model is suitable\n\n")

#' ## q3: year-wise seasonal overlay

cat("q3: year-wise seasonal overlay\n\n")

#+ fig.width=10, fig.height=6
nt_matrix <- matrix(nt, nrow = 12, ncol = 20)
matplot(1:12, nt_matrix, type = "l", lty = 1, lwd = 1.5,
        col = rainbow(20),
        main = "fig 3: year-wise seasonal overlay",
        xlab = "month", ylab = "temperature (fahrenheit)", xaxt = "n")
axis(1, at = 1:12, labels = month.abb)
legend("topleft", legend = 1920:1939, col = rainbow(20),
       lty = 1, cex = 0.5, ncol = 4, bty = "n")

cat("interpretation of fig 3:\n")
cat("- all years follow the same seasonal curve shape\n")
cat("- the curves overlap closely indicating no significant trend shift\n")
cat("- the gap between years is roughly the same at peaks and troughs\n")
cat("- this confirms constant seasonal amplitude (additive behavior)\n\n")

#' ## q4: identify components using decomposition

cat("q4: decomposition - identify trend, seasonal, cyclical, and irregular components\n\n")

# additive decomposition
decomp_add <- decompose(nt, type = "additive")

#+ fig.width=10, fig.height=8
plot(decomp_add, col = "blue")
title(main = "fig 4: additive decomposition of nottem", outer = FALSE)

cat("interpretation of fig 4 (additive decomposition):\n")
cat("- observed: the original series with all components combined\n")
cat("- trend: fluctuates mildly around 49-50 F with no strong direction\n")
cat("- seasonal: repeating pattern with fixed amplitude of about +/- 11 F\n")
cat("- random: residuals fluctuate around 0 with fairly constant variance\n\n")

cat("table 1: additive seasonal indices\n")
si_add <- decomp_add$figure
si_add_df <- data.frame(month = month.abb,
                        index = round(si_add, 2),
                        effect = ifelse(si_add > 0,
                          paste0(round(si_add, 2), " F above annual mean"),
                          paste0(round(abs(si_add), 2), " F below annual mean")))
print(si_add_df, row.names = FALSE)
cat("\n")

# multiplicative decomposition
decomp_mult <- decompose(nt, type = "multiplicative")

#+ fig.width=10, fig.height=8
plot(decomp_mult, col = "red")
title(main = "fig 5: multiplicative decomposition of nottem", outer = FALSE)

cat("interpretation of fig 5 (multiplicative decomposition):\n")
cat("- trend component is similar to the additive case\n")
cat("- seasonal indices are ratios around 1.0\n")
cat("- random component fluctuates around 1.0\n")
cat("- both decompositions produce similar results since seasonal amplitude is constant\n\n")

#' ## q5: compare additive vs multiplicative and select the better model

cat("q5: additive vs multiplicative model comparison\n\n")

cat("additive model: y(t) = t(t) + s(t) + c(t) + i(t)\n")
cat("- assumes seasonal fluctuations are constant in absolute terms\n\n")
cat("multiplicative model: y(t) = t(t) x s(t) x c(t) x i(t)\n")
cat("- assumes seasonal fluctuations scale with the trend level\n\n")

resid_add <- na.omit(decomp_add$random)
resid_mult <- na.omit(decomp_mult$random)

cat("additive residuals:\n")
cat("  mean:", round(mean(resid_add), 4), "(should be close to 0)\n")
cat("  sd:", round(sd(resid_add), 4), "\n\n")

cat("multiplicative residuals:\n")
cat("  mean:", round(mean(resid_mult), 4), "(should be close to 1)\n")
cat("  sd:", round(sd(resid_mult), 4), "\n\n")

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(na.omit(decomp_add$random),
     main = "fig 6a: additive residuals",
     ylab = "residual", col = "blue", lwd = 1.5)
abline(h = 0, lty = 2)
plot(na.omit(decomp_mult$random),
     main = "fig 6b: multiplicative residuals",
     ylab = "residual", col = "red", lwd = 1.5)
abline(h = 1, lty = 2)
par(mfrow = c(1, 1))

cat("interpretation of fig 6:\n")
cat("- additive residuals are evenly scattered around 0 with constant variance\n")
cat("- multiplicative residuals are also fairly even around 1\n")
cat("- since the seasonal amplitude does not grow with the level,\n")
cat("  additive model is the more suitable choice for nottem\n\n")

cat("selected model: additive\n")
cat("y(t) = t(t) + s(t) + i(t)\n\n")
cat("justification:\n")
cat("1. seasonal swings are constant (~22 F range) regardless of the overall level (fig 1, 3)\n")
cat("2. box plot spreads are similar across months (fig 2)\n")
cat("3. additive residuals have constant variance around 0 (fig 6a)\n")
cat("4. temperature data is on an interval scale, additive effects are natural\n")
cat("5. no multiplicative scaling of seasonal amplitude is observed\n\n")

#' ## q6: assess stationarity - check mean and variance over time

cat("q6: assess stationarity of the series\n\n")

# split into 4 equal segments of 5 years each
n_seg <- 4
seg_len <- length(nt) / n_seg

cat("table 2: segment-wise mean and variance\n")
cat(sprintf("%-12s | %-8s | %-10s\n", "segment", "mean", "variance"))
cat(paste(rep("-", 35), collapse = ""), "\n")
for (i in 1:n_seg) {
  seg <- nt[((i - 1) * seg_len + 1):(i * seg_len)]
  yr_start <- 1920 + (i - 1) * 5
  yr_end <- yr_start + 4
  label <- paste0(yr_start, "-", yr_end)
  cat(sprintf("%-12s | %-8.2f | %-10.2f\n", label, mean(seg), var(seg)))
}
cat("\n")

cat("overall mean:", round(mean(nt), 2), "\n")
cat("overall variance:", round(var(nt), 2), "\n\n")

# annual means
annual_means <- aggregate(nt, nfrequency = 1, FUN = mean)

#+ fig.width=10, fig.height=6
plot(annual_means,
     main = "fig 7: annual mean temperature (1920-1939)",
     ylab = "mean temperature (F)", xlab = "year",
     col = "darkgreen", lwd = 2, type = "o", pch = 16)
abline(h = mean(nt), col = "red", lty = 2, lwd = 1.5)
grid()

cat("interpretation of fig 7:\n")
cat("- annual means fluctuate around 49-50 F with no clear trend\n")
cat("- the overall mean (red line) is stable\n")
cat("- however the series has strong seasonality which makes it non-stationary\n")
cat("- the mean of the series is NOT constant on a monthly basis due to seasonality\n")
cat("- variance is roughly constant across segments\n")
cat("- conclusion: the series is NOT stationary because of the seasonal component\n")
cat("  even though there is no trend, the periodic pattern violates stationarity\n\n")

#' ## q7: log transformation

cat("q7: log transformation and its effect on variance\n\n")

nt_log <- log(nt)

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(nt,
     main = "fig 8a: original nottem series",
     ylab = "temperature (F)", xlab = "year",
     col = "blue", lwd = 2)
grid()
plot(nt_log,
     main = "fig 8b: log-transformed nottem series",
     ylab = "log(temperature)", xlab = "year",
     col = "darkgreen", lwd = 2)
grid()
par(mfrow = c(1, 1))

cat("comparison of original vs log-transformed:\n")
cat("original - mean:", round(mean(nt), 2), " sd:", round(sd(nt), 2), "\n")
cat("log      - mean:", round(mean(nt_log), 4), " sd:", round(sd(nt_log), 4), "\n\n")

# check variance in segments for both
cat("table 3: segment-wise variance comparison\n")
cat(sprintf("%-12s | %-14s | %-14s\n", "segment", "original var", "log var"))
cat(paste(rep("-", 45), collapse = ""), "\n")
for (i in 1:n_seg) {
  seg_orig <- nt[((i - 1) * seg_len + 1):(i * seg_len)]
  seg_log <- nt_log[((i - 1) * seg_len + 1):(i * seg_len)]
  yr_start <- 1920 + (i - 1) * 5
  yr_end <- yr_start + 4
  label <- paste0(yr_start, "-", yr_end)
  cat(sprintf("%-12s | %-14.2f | %-14.4f\n", label, var(seg_orig), var(seg_log)))
}
cat("\n")

cat("interpretation of fig 8 and table 3:\n")
cat("- the log transformation compresses the scale but the shape is nearly identical\n")
cat("- since the original variance was already fairly constant across segments,\n")
cat("  the log transformation does not provide significant improvement\n")
cat("- log transformation is more useful when variance increases with level\n")
cat("  (as in airpassengers). for nottem, it is not necessary\n")
cat("- however, log transform slightly reduces the relative seasonal amplitude\n\n")

#' ## q8: acf plot - autocorrelation function

cat("q8: autocorrelation function (acf) analysis\n\n")

#+ fig.width=10, fig.height=6
acf(nt, lag.max = 48,
    main = "fig 9: acf of nottem series",
    col = "blue", lwd = 2)

cat("interpretation of fig 9 (acf plot):\n")
cat("- the acf shows a strong sinusoidal pattern that repeats every 12 lags\n")
cat("- significant positive correlations at lags 12, 24, 36 (yearly multiples)\n")
cat("- significant negative correlations at lags 6, 18, 30 (half-yearly)\n")
cat("- this confirms the strong 12-month seasonal pattern\n")
cat("- the acf does not decay quickly to zero, indicating non-stationarity\n")
cat("- the periodic pattern in acf is a hallmark of seasonal non-stationarity\n\n")

#' ## q9: method of differencing (first differencing)

cat("q9: method of differencing - first order differencing\n\n")

cat("differencing removes trend by computing y(t) - y(t-1).\n")
cat("if the series has a trend, first differencing should remove it\n")
cat("and make the mean more stable over time.\n\n")

nt_diff1 <- diff(nt, differences = 1)

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(nt,
     main = "fig 10a: original nottem series",
     ylab = "temperature (F)", xlab = "year",
     col = "blue", lwd = 2)
grid()
plot(nt_diff1,
     main = "fig 10b: first differenced series",
     ylab = "differenced values", xlab = "year",
     col = "purple", lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
grid()
par(mfrow = c(1, 1))

cat("first differenced series stats:\n")
cat("  mean:", round(mean(nt_diff1), 4), "(should be close to 0 if trend removed)\n")
cat("  sd:", round(sd(nt_diff1), 2), "\n")
cat("  length:", length(nt_diff1), "(one less than original)\n\n")

#+ fig.width=10, fig.height=6
acf(nt_diff1, lag.max = 48,
    main = "fig 11: acf of first differenced series",
    col = "purple", lwd = 2)

cat("interpretation of fig 10-11:\n")
cat("- first differencing removes any trend component\n")
cat("- the differenced series oscillates around 0\n")
cat("- however the acf still shows strong seasonal pattern at lags 12, 24, 36\n")
cat("- this means first differencing alone is NOT sufficient\n")
cat("- we still need seasonal differencing to remove the seasonal component\n\n")

#' ## q10: method of seasonal differencing

cat("q10: seasonal differencing (lag = 12)\n\n")

cat("seasonal differencing computes y(t) - y(t-12).\n")
cat("this removes the seasonal pattern by subtracting the value\n")
cat("from the same month in the previous year.\n\n")

nt_sdiff <- diff(nt, lag = 12)

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(nt,
     main = "fig 12a: original nottem series",
     ylab = "temperature (F)", xlab = "year",
     col = "blue", lwd = 2)
grid()
plot(nt_sdiff,
     main = "fig 12b: seasonally differenced series (lag 12)",
     ylab = "seasonal difference", xlab = "year",
     col = "darkred", lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
grid()
par(mfrow = c(1, 1))

cat("seasonally differenced series stats:\n")
cat("  mean:", round(mean(nt_sdiff), 4), "\n")
cat("  sd:", round(sd(nt_sdiff), 2), "\n")
cat("  length:", length(nt_sdiff), "(12 less than original)\n\n")

#+ fig.width=10, fig.height=6
acf(nt_sdiff, lag.max = 48,
    main = "fig 13: acf of seasonally differenced series",
    col = "darkred", lwd = 2)

cat("interpretation of fig 12-13:\n")
cat("- seasonal differencing successfully removes the 12-month seasonal pattern\n")
cat("- the resulting series fluctuates around 0 with no obvious pattern\n")
cat("- the acf shows rapid decay with most lags within the confidence bands\n")
cat("- significant spike at lag 1 and lag 12 may remain but are much reduced\n")
cat("- the seasonally differenced series appears much closer to stationary\n\n")

# combined: first + seasonal differencing
nt_both_diff <- diff(diff(nt, lag = 12), differences = 1)

#+ fig.width=10, fig.height=6
plot(nt_both_diff,
     main = "fig 14: first + seasonal differencing combined",
     ylab = "double differenced values", xlab = "year",
     col = "darkblue", lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
grid()

#+ fig.width=10, fig.height=6
acf(nt_both_diff, lag.max = 48,
    main = "fig 15: acf of first + seasonally differenced series",
    col = "darkblue", lwd = 2)

cat("interpretation of fig 14-15:\n")
cat("- combining first and seasonal differencing removes both trend and seasonality\n")
cat("- the series looks like white noise fluctuating around 0\n")
cat("- the acf drops within confidence bands quickly\n")
cat("- this confirms the series can be made stationary with proper differencing\n\n")

#' ## q11: moving average smoothing

cat("q11: moving average smoothing\n\n")

cat("moving average smoothing replaces each value with the average of\n")
cat("its neighbors within a specified window. this removes short term\n")
cat("fluctuations and highlights the underlying trend.\n\n")

# simple moving average using filter()
ma_3 <- filter(nt, rep(1/3, 3), sides = 2)
ma_7 <- filter(nt, rep(1/7, 7), sides = 2)
ma_12 <- filter(nt, rep(1/12, 12), sides = 2)
# centered 2x12 MA (same as decompose uses internally)
ma_2x12 <- filter(filter(nt, rep(1/12, 12), sides = 2),
                  rep(1/2, 2), sides = 2)

#+ fig.width=10, fig.height=10
par(mfrow = c(2, 2))
plot(nt, col = "grey70", lwd = 1,
     main = "fig 16a: 3-point moving average")
lines(ma_3, col = "blue", lwd = 2)
legend("bottomright", legend = c("original", "MA(3)"),
       col = c("grey70", "blue"), lwd = c(1, 2), cex = 0.8, bty = "n")

plot(nt, col = "grey70", lwd = 1,
     main = "fig 16b: 7-point moving average")
lines(ma_7, col = "red", lwd = 2)
legend("bottomright", legend = c("original", "MA(7)"),
       col = c("grey70", "red"), lwd = c(1, 2), cex = 0.8, bty = "n")

plot(nt, col = "grey70", lwd = 1,
     main = "fig 16c: 12-point moving average")
lines(ma_12, col = "darkgreen", lwd = 2)
legend("bottomright", legend = c("original", "MA(12)"),
       col = c("grey70", "darkgreen"), lwd = c(1, 2), cex = 0.8, bty = "n")

plot(nt, col = "grey70", lwd = 1,
     main = "fig 16d: 2x12 centered moving average")
lines(ma_2x12, col = "purple", lwd = 2)
legend("bottomright", legend = c("original", "2x12 MA"),
       col = c("grey70", "purple"), lwd = c(1, 2), cex = 0.8, bty = "n")
par(mfrow = c(1, 1))

cat("interpretation of fig 16:\n")
cat("- MA(3): smooths out month-to-month noise but seasonal pattern still visible\n")
cat("- MA(7): further smoothing, seasonal peaks and troughs are dampened\n")
cat("- MA(12): almost completely removes the seasonal pattern, shows the trend\n")
cat("- 2x12 centered MA: the cleanest trend estimate, same method decompose() uses\n")
cat("- as the window size increases, more smoothing occurs and seasonality is filtered out\n")
cat("- the 12-point and 2x12 MA effectively isolate the trend component\n\n")

# overlay all MAs together
#+ fig.width=10, fig.height=6
plot(nt, col = "grey70", lwd = 1,
     main = "fig 17: comparison of moving averages",
     ylab = "temperature (F)", xlab = "year")
lines(ma_3, col = "blue", lwd = 1.5)
lines(ma_7, col = "red", lwd = 1.5)
lines(ma_12, col = "darkgreen", lwd = 2)
lines(ma_2x12, col = "purple", lwd = 2.5)
legend("bottomright",
       legend = c("original", "MA(3)", "MA(7)", "MA(12)", "2x12 MA"),
       col = c("grey70", "blue", "red", "darkgreen", "purple"),
       lwd = c(1, 1.5, 1.5, 2, 2.5), cex = 0.7, bty = "n")
grid()

cat("interpretation of fig 17:\n")
cat("- larger window sizes produce smoother curves\n")
cat("- MA(12) and 2x12 MA converge to the underlying trend\n")
cat("- the trend fluctuates mildly between 48 and 51 F with no strong direction\n")
cat("- moving average is a simple but effective method for trend extraction\n\n")

#' ## q12: summary comparison table

cat("q12: summary of all transformations\n\n")

cat(sprintf("%-30s | %-10s | %-10s | %-15s\n",
            "transformation", "mean", "sd", "stationary?"))
cat(paste(rep("-", 72), collapse = ""), "\n")
cat(sprintf("%-30s | %-10.2f | %-10.2f | %-15s\n",
            "original", mean(nt), sd(nt), "no (seasonal)"))
cat(sprintf("%-30s | %-10.4f | %-10.2f | %-15s\n",
            "first differenced", mean(nt_diff1), sd(nt_diff1), "no (seasonal)"))
cat(sprintf("%-30s | %-10.4f | %-10.2f | %-15s\n",
            "seasonal differenced (lag 12)", mean(nt_sdiff), sd(nt_sdiff),
            "nearly"))
cat(sprintf("%-30s | %-10.4f | %-10.2f | %-15s\n",
            "first + seasonal differenced",
            mean(nt_both_diff), sd(nt_both_diff), "yes"))
cat(sprintf("%-30s | %-10.4f | %-10.4f | %-15s\n",
            "log transformed", mean(nt_log), sd(nt_log), "no (seasonal)"))
cat("\n")

#' # conclusion

cat("conclusion:\n\n")
cat("1. the nottem dataset shows strong monthly seasonality with no significant trend.\n")
cat("2. the additive model is more suitable because seasonal amplitude is constant\n")
cat("   and does not scale with the level of the series.\n")
cat("3. the series is NOT stationary due to the seasonal component, even though\n")
cat("   the overall mean and variance appear roughly constant.\n")
cat("4. log transformation has minimal effect since variance is already stable.\n")
cat("5. the acf confirms strong seasonal autocorrelation at lags 12, 24, 36.\n")
cat("6. first differencing removes trend but not seasonality.\n")
cat("7. seasonal differencing (lag 12) effectively removes the seasonal pattern.\n")
cat("8. combining first and seasonal differencing produces a near-stationary series.\n")
cat("9. moving average smoothing with window 12 or 2x12 isolates the trend.\n")
cat("10. the nottem data is well-suited for seasonal ARIMA (SARIMA) modeling\n")
cat("    with seasonal differencing of order 1 and period 12.\n")
