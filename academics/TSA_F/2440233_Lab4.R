#' ---
#' title: "time series analysis - lab 4"
#' subtitle: "white noise process and stationarity testing with differencing"
#' author: "2440233"
#' date: "`r Sys.Date()`"
#' output:
#'   pdf_document:
#'     toc: true

#' # introduction
#'
#' this lab has two objectives:
#' 1. determine whether the Nile dataset behaves like a white noise process
#'    using time series visualization and the ACF plot.
#' 2. examine whether the JohnsonJohnson dataset is stationary using time
#'    series and ACF plots, and transform it into a stationary series
#'    using differencing.

#' # methodology
#'
#' datasets used:
#' - Nile: annual flow of the river nile at aswan from 1871 to 1970.
#'   100 observations, annual frequency. source: durbin and koopman (2001).
#' - JohnsonJohnson: quarterly earnings per share of johnson & johnson
#'   from 1960 Q1 to 1980 Q4. 84 observations, quarterly frequency.

#' # analysis

#' # part 1: white noise testing using the Nile dataset

#' ## q1: load and explore the Nile dataset

cat("=" , rep("=", 60), "\n", sep = "")
cat("PART 1: WHITE NOISE TESTING - NILE DATASET\n")
cat("=" , rep("=", 60), "\n\n", sep = "")

cat("q1: load and explore the nile dataset\n\n")

data("Nile")
nile <- Nile

cat("class:", class(nile), "\n")
cat("start:", start(nile)[1], "\n")
cat("end:", end(nile)[1], "\n")
cat("frequency:", frequency(nile), "\n")
cat("observations:", length(nile), "\n\n")

cat("first 10 values:\n")
print(head(nile, 10))
cat("\nlast 10 values:\n")
print(tail(nile, 10))

cat("\nsummary:\n")
print(summary(nile))
cat("mean:", round(mean(nile), 2), "\n")
cat("sd:", round(sd(nile), 2), "\n")
cat("variance:", round(var(nile), 2), "\n\n")

#' ## q2: what is a white noise process?

cat("q2: what is a white noise process?\n\n")

cat("a white noise process is a sequence of random variables that satisfies:\n")
cat("1. constant mean: E[Y_t] = 0 (or some constant mu) for all t\n")
cat("2. constant variance: Var(Y_t) = sigma^2 for all t\n")
cat("3. zero autocorrelation: Cov(Y_t, Y_t-k) = 0 for all k != 0\n\n")

cat("in simple terms, white noise means:\n")
cat("- each value is independent of every other value\n")
cat("- there is no pattern, trend, or seasonality\n")
cat("- knowing past values tells you nothing about future values\n")
cat("- the series is purely random fluctuations around a constant mean\n\n")

cat("why is it important?\n")
cat("- if a series IS white noise, there is nothing to model or forecast\n")
cat("- if residuals from a model are white noise, the model captured all patterns\n")
cat("- it is the simplest form of a stationary process\n\n")

#' ## q3: visualize the Nile series

cat("q3: visualize the nile time series\n\n")

#+ fig.width=10, fig.height=6
plot(nile,
     main = "fig 1: annual flow of the river nile (1871-1970)",
     ylab = "flow (10^8 cubic meters)",
     xlab = "year",
     col = "blue", lwd = 2)
abline(h = mean(nile), col = "red", lty = 2, lwd = 1.5)
grid()

cat("interpretation of fig 1:\n")
cat("- the series fluctuates around a mean of about 919 cubic meters\n")
cat("- there appears to be a shift around 1898 where the level drops\n")
cat("- before 1898: the mean appears higher (around 1100)\n")
cat("- after 1898: the mean appears lower (around 850)\n")
cat("- this level shift suggests the series may NOT be white noise\n")
cat("- white noise should have a constant mean throughout\n\n")

#' ## q4: check mean and variance stability

cat("q4: check mean and variance across segments\n\n")

# split into segments before and after the apparent break
nile_before <- window(nile, end = 1898)
nile_after <- window(nile, start = 1899)

cat("segment analysis:\n")
cat(sprintf("%-15s | %-10s | %-10s | %-10s\n",
            "segment", "mean", "sd", "n"))
cat(paste(rep("-", 50), collapse = ""), "\n")
cat(sprintf("%-15s | %-10.2f | %-10.2f | %-10d\n",
            "1871-1898", mean(nile_before), sd(nile_before), length(nile_before)))
cat(sprintf("%-15s | %-10.2f | %-10.2f | %-10d\n",
            "1899-1970", mean(nile_after), sd(nile_after), length(nile_after)))
cat(sprintf("%-15s | %-10.2f | %-10.2f | %-10d\n",
            "overall", mean(nile), sd(nile), length(nile)))
cat("\n")

cat("interpretation:\n")
cat("- the mean drops from ~1097 before 1898 to ~850 after 1898\n")
cat("- this is a significant shift of about 250 units\n")
cat("- this change in mean violates the white noise requirement of constant mean\n")
cat("- the variance also changes between the two periods\n\n")

# also check 4 equal segments
cat("table 1: four equal segments analysis\n")
n_seg <- 4
seg_len <- length(nile) / n_seg
cat(sprintf("%-15s | %-10s | %-12s\n", "segment", "mean", "variance"))
cat(paste(rep("-", 42), collapse = ""), "\n")
for (i in 1:n_seg) {
  seg <- nile[((i - 1) * seg_len + 1):(i * seg_len)]
  yr_start <- 1871 + (i - 1) * 25
  yr_end <- yr_start + 24
  label <- paste0(yr_start, "-", yr_end)
  cat(sprintf("%-15s | %-10.2f | %-12.2f\n", label, mean(seg), var(seg)))
}
cat("\n")

#' ## q5: histogram of the Nile data

cat("q5: distribution of nile flow values\n\n")

#+ fig.width=10, fig.height=6
hist(nile, breaks = 20,
     main = "fig 2: histogram of nile river flow",
     xlab = "flow (10^8 cubic meters)",
     col = "lightblue", border = "blue",
     freq = FALSE)
curve(dnorm(x, mean = mean(nile), sd = sd(nile)),
      add = TRUE, col = "red", lwd = 2)
legend("topright", legend = "normal curve",
       col = "red", lwd = 2, bty = "n")

cat("interpretation of fig 2:\n")
cat("- the distribution is roughly bell-shaped but slightly right-skewed\n")
cat("- white noise from a gaussian process should be symmetric and normal\n")
cat("- the spread looks reasonable but the shape shows some irregularity\n\n")

#' ## q6: ACF plot of the Nile series

cat("q6: acf analysis of the nile series\n\n")

#+ fig.width=10, fig.height=6
acf(nile, lag.max = 30,
    main = "fig 3: acf of nile river flow",
    col = "blue", lwd = 2)

cat("interpretation of fig 3 (acf of nile):\n")
cat("- if the series were white noise, ALL bars should be within the blue bands\n")
cat("- lag 1 shows significant positive autocorrelation (~0.5)\n")
cat("- lags 2-5 also show significant positive correlations\n")
cat("- the acf decays slowly rather than cutting off immediately\n")
cat("- this slow decay is characteristic of a non-stationary series or\n")
cat("  a series with strong positive autocorrelation (AR process)\n")
cat("- CONCLUSION: the nile series is NOT white noise because:\n")
cat("  1. there are significant autocorrelations at multiple lags\n")
cat("  2. the acf does not drop within confidence bands immediately\n")
cat("  3. there is a structural break around 1898\n\n")

#' ## q7: generate true white noise for comparison

cat("q7: what actual white noise looks like (for comparison)\n\n")

set.seed(42)
wn <- ts(rnorm(100, mean = 0, sd = 1), start = 1871, frequency = 1)

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 2))
plot(nile,
     main = "fig 4a: nile series (actual data)",
     ylab = "flow", col = "blue", lwd = 2)
abline(h = mean(nile), col = "red", lty = 2)
plot(wn,
     main = "fig 4b: simulated white noise",
     ylab = "value", col = "darkgreen", lwd = 1.5)
abline(h = 0, col = "red", lty = 2)
acf(nile, lag.max = 20,
    main = "fig 4c: acf of nile",
    col = "blue", lwd = 2)
acf(wn, lag.max = 20,
    main = "fig 4d: acf of white noise",
    col = "darkgreen", lwd = 2)
par(mfrow = c(1, 1))

cat("interpretation of fig 4:\n")
cat("- fig 4b shows true white noise: random scatter around 0 with no pattern\n")
cat("- fig 4d shows white noise acf: all bars within the blue dashed bands\n")
cat("- comparing to nile: the nile series has persistent correlations (fig 4c)\n")
cat("- the nile series shows clusters of high and low values, not random scatter\n")
cat("- this confirms the nile river flow is NOT a white noise process\n\n")

#' ## q8: summary of white noise testing

cat("q8: summary - is the nile series white noise?\n\n")

cat("criteria for white noise and nile results:\n\n")
cat(sprintf("%-40s | %-10s | %-15s\n",
            "criterion", "required", "nile result"))
cat(paste(rep("-", 70), collapse = ""), "\n")
cat(sprintf("%-40s | %-10s | %-15s\n",
            "constant mean", "yes", "NO (level shift)"))
cat(sprintf("%-40s | %-10s | %-15s\n",
            "constant variance", "yes", "approximately"))
cat(sprintf("%-40s | %-10s | %-15s\n",
            "zero autocorrelation at all lags", "yes", "NO (sig. at lags 1-5)"))
cat(sprintf("%-40s | %-10s | %-15s\n",
            "no patterns in time plot", "yes", "NO (level shift)"))
cat(sprintf("%-40s | %-10s | %-15s\n",
            "acf within confidence bands", "yes", "NO"))
cat("\n")
cat("CONCLUSION: the nile series is NOT a white noise process.\n")
cat("it shows significant autocorrelation, a structural break around 1898,\n")
cat("and slow decay in the acf. this suggests the series has memory\n")
cat("(past values influence future values) and possibly a trend change.\n\n")

#' # part 2: stationarity testing and differencing - JohnsonJohnson dataset

cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("PART 2: STATIONARITY AND DIFFERENCING - JOHNSONJOHNSON\n")
cat("=" , rep("=", 60), "\n\n", sep = "")

#' ## q9: load and explore the JohnsonJohnson dataset

cat("q9: load and explore the johnsonjohnson dataset\n\n")

data("JohnsonJohnson")
jj <- JohnsonJohnson

cat("class:", class(jj), "\n")
cat("start:", start(jj)[1], "Q", start(jj)[2], "\n")
cat("end:", end(jj)[1], "Q", end(jj)[2], "\n")
cat("frequency:", frequency(jj), "\n")
cat("observations:", length(jj), "\n\n")

cat("first 8 values (first 2 years):\n")
print(head(jj, 8))
cat("\nlast 8 values (last 2 years):\n")
print(tail(jj, 8))

cat("\nsummary:\n")
print(summary(jj))
cat("mean:", round(mean(jj), 2), "\n")
cat("sd:", round(sd(jj), 2), "\n\n")

#' ## q10: visualize JohnsonJohnson series

cat("q10: visualize the johnsonjohnson time series\n\n")

#+ fig.width=10, fig.height=6
plot(jj,
     main = "fig 5: quarterly earnings of johnson & johnson (1960-1980)",
     ylab = "earnings per share ($)",
     xlab = "year",
     col = "blue", lwd = 2)
grid()

cat("interpretation of fig 5:\n")
cat("- there is a strong upward TREND: earnings grow from ~$0.7 to ~$16\n")
cat("- there is SEASONALITY: quarterly repeating pattern visible\n")
cat("- the seasonal amplitude GROWS with the level (wider swings later)\n")
cat("- the growth appears EXPONENTIAL, not linear\n")
cat("- all these features indicate the series is NOT stationary\n\n")

#' ## q11: boxplot by quarter

cat("q11: quarterly distribution\n\n")

#+ fig.width=10, fig.height=6
boxplot(jj ~ cycle(jj),
        main = "fig 6: quarterly distribution of jj earnings",
        xlab = "quarter", ylab = "earnings per share ($)",
        col = c("lightblue", "lightgreen", "lightyellow", "lightsalmon"),
        names = c("Q1", "Q2", "Q3", "Q4"))

cat("interpretation of fig 6:\n")
cat("- Q3 and Q4 tend to have higher earnings\n")
cat("- the spread of each box is very large due to the trend\n")
cat("- the box sizes are not equal, larger spread for higher-value quarters\n")
cat("- this confirms multiplicative seasonality\n\n")

#' ## q12: check stationarity - mean and variance over time

cat("q12: assess stationarity of johnsonjohnson\n\n")

# split into 4 segments of ~5 years each
n_seg <- 4
seg_len <- length(jj) / n_seg

cat("table 2: segment-wise mean and variance\n")
cat(sprintf("%-15s | %-10s | %-12s\n", "segment", "mean", "variance"))
cat(paste(rep("-", 42), collapse = ""), "\n")
for (i in 1:n_seg) {
  seg <- jj[((i - 1) * seg_len + 1):(i * seg_len)]
  yr_start <- 1960 + (i - 1) * 5
  yr_end <- yr_start + 4
  label <- paste0(yr_start, "-", yr_end)
  cat(sprintf("%-15s | %-10.2f | %-12.2f\n", label, mean(seg), var(seg)))
}
cat("\n")

cat("interpretation of table 2:\n")
cat("- the mean increases dramatically from segment to segment\n")
cat("- the variance also increases massively (from ~0.1 to ~24)\n")
cat("- both mean and variance change over time\n")
cat("- CONCLUSION: the series is clearly NOT stationary\n")
cat("  it violates both constant mean and constant variance conditions\n\n")

#' ## q13: ACF of original JohnsonJohnson

cat("q13: acf analysis of the original jj series\n\n")

#+ fig.width=10, fig.height=6
acf(jj, lag.max = 30,
    main = "fig 7: acf of johnsonjohnson (original)",
    col = "blue", lwd = 2)

cat("interpretation of fig 7:\n")
cat("- the acf decays very slowly - almost all lags are significant\n")
cat("- this slow decay is the hallmark of a NON-STATIONARY series\n")
cat("- there is no sinusoidal pattern because the trend dominates\n")
cat("- for a stationary series, the acf should decay to zero quickly\n")
cat("- conclusion: the acf confirms non-stationarity\n\n")

#' ## q14: log transformation to stabilize variance

cat("q14: log transformation of jj series\n\n")

jj_log <- log(jj)

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(jj,
     main = "fig 8a: original jj series",
     ylab = "earnings ($)", xlab = "year",
     col = "blue", lwd = 2)
grid()
plot(jj_log,
     main = "fig 8b: log-transformed jj series",
     ylab = "log(earnings)", xlab = "year",
     col = "darkgreen", lwd = 2)
grid()
par(mfrow = c(1, 1))

cat("interpretation of fig 8:\n")
cat("- log transform converts exponential growth into linear growth\n")
cat("- the seasonal amplitude becomes much more constant after log\n")
cat("- this confirms the original series has multiplicative seasonality\n")
cat("- the log-transformed series still has a trend so it is still non-stationary\n")
cat("- but the variance is now more stable, which is a necessary first step\n\n")

#' ## q15: first differencing of log-transformed series

cat("q15: first differencing to remove trend\n\n")

jj_log_diff <- diff(jj_log, differences = 1)

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(jj_log,
     main = "fig 9a: log-transformed jj series (still has trend)",
     ylab = "log(earnings)", xlab = "year",
     col = "darkgreen", lwd = 2)
grid()
plot(jj_log_diff,
     main = "fig 9b: first differenced log jj series",
     ylab = "differenced log(earnings)", xlab = "year",
     col = "purple", lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
grid()
par(mfrow = c(1, 1))

cat("first differenced log(jj) stats:\n")
cat("  mean:", round(mean(jj_log_diff), 4), "\n")
cat("  sd:", round(sd(jj_log_diff), 4), "\n")
cat("  length:", length(jj_log_diff), "\n\n")

#+ fig.width=10, fig.height=6
acf(jj_log_diff, lag.max = 30,
    main = "fig 10: acf of first differenced log(jj)",
    col = "purple", lwd = 2)

cat("interpretation of fig 9-10:\n")
cat("- first differencing removes the linear trend from the log series\n")
cat("- the differenced series fluctuates around 0\n")
cat("- but the acf shows significant spikes at lags 4, 8, 12...\n")
cat("- these are multiples of 4 (quarterly seasonality)\n")
cat("- conclusion: first differencing removed trend but NOT seasonality\n")
cat("- we need seasonal differencing next\n\n")

#' ## q16: seasonal differencing (lag = 4)

cat("q16: seasonal differencing (lag = 4 for quarterly data)\n\n")

jj_log_sdiff <- diff(jj_log, lag = 4)

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(jj_log,
     main = "fig 11a: log-transformed jj (has trend + seasonality)",
     ylab = "log(earnings)", xlab = "year",
     col = "darkgreen", lwd = 2)
grid()
plot(jj_log_sdiff,
     main = "fig 11b: seasonally differenced log(jj) (lag 4)",
     ylab = "seasonal difference", xlab = "year",
     col = "darkred", lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
grid()
par(mfrow = c(1, 1))

cat("seasonally differenced log(jj) stats:\n")
cat("  mean:", round(mean(jj_log_sdiff), 4), "\n")
cat("  sd:", round(sd(jj_log_sdiff), 4), "\n\n")

#+ fig.width=10, fig.height=6
acf(jj_log_sdiff, lag.max = 20,
    main = "fig 12: acf of seasonally differenced log(jj)",
    col = "darkred", lwd = 2)

cat("interpretation of fig 11-12:\n")
cat("- seasonal differencing removes the quarterly pattern\n")
cat("- the resulting series still shows some trend (mild upward drift)\n")
cat("- the acf shows slow decay suggesting some trend remains\n")
cat("- we need to apply first differencing on top of seasonal differencing\n\n")

#' ## q17: combined differencing (first + seasonal)

cat("q17: combined first + seasonal differencing\n\n")

jj_log_both <- diff(diff(jj_log, lag = 4), differences = 1)

#+ fig.width=10, fig.height=6
plot(jj_log_both,
     main = "fig 13: first + seasonal differencing of log(jj)",
     ylab = "double differenced values", xlab = "year",
     col = "darkblue", lwd = 1.5)
abline(h = 0, lty = 2, col = "red")
grid()

#+ fig.width=10, fig.height=6
acf(jj_log_both, lag.max = 20,
    main = "fig 14: acf of first + seasonally differenced log(jj)",
    col = "darkblue", lwd = 2)

cat("stats of fully differenced series:\n")
cat("  mean:", round(mean(jj_log_both), 4), "(close to 0 = good)\n")
cat("  sd:", round(sd(jj_log_both), 4), "\n\n")

cat("interpretation of fig 13-14:\n")
cat("- the fully differenced series fluctuates randomly around 0\n")
cat("- the acf shows most lags within the confidence bands\n")
cat("- there may be a significant spike at lag 1 and lag 4\n")
cat("- but the overall pattern shows rapid decay to zero\n")
cat("- CONCLUSION: the series is now approximately STATIONARY\n")
cat("- the transformation pipeline that worked:\n")
cat("  1. log transform (stabilize variance)\n")
cat("  2. seasonal differencing lag 4 (remove seasonality)\n")
cat("  3. first differencing (remove remaining trend)\n\n")

#' ## q18: full comparison of all transformations

cat("q18: summary of all transformations applied to jj\n\n")

cat(sprintf("%-40s | %-10s | %-10s | %-12s\n",
            "transformation", "mean", "sd", "stationary?"))
cat(paste(rep("-", 78), collapse = ""), "\n")
cat(sprintf("%-40s | %-10.2f | %-10.2f | %-12s\n",
            "original jj", mean(jj), sd(jj), "NO"))
cat(sprintf("%-40s | %-10.4f | %-10.4f | %-12s\n",
            "log(jj)", mean(jj_log), sd(jj_log), "NO (trend)"))
cat(sprintf("%-40s | %-10.4f | %-10.4f | %-12s\n",
            "diff(log(jj))", mean(jj_log_diff), sd(jj_log_diff),
            "NO (seasonal)"))
cat(sprintf("%-40s | %-10.4f | %-10.4f | %-12s\n",
            "seasonal diff(log(jj), lag=4)",
            mean(jj_log_sdiff), sd(jj_log_sdiff), "NO (trend)"))
cat(sprintf("%-40s | %-10.4f | %-10.4f | %-12s\n",
            "diff + seasonal diff of log(jj)",
            mean(jj_log_both), sd(jj_log_both), "YES"))
cat("\n")

#' ## q19: visual summary

cat("q19: visual comparison of transformation pipeline\n\n")

#+ fig.width=10, fig.height=12
par(mfrow = c(4, 1), mar = c(3, 4, 2, 1))
plot(jj, main = "step 0: original jj (non-stationary)",
     ylab = "earnings ($)", col = "blue", lwd = 2)
plot(jj_log, main = "step 1: log(jj) - variance stabilized",
     ylab = "log(earnings)", col = "darkgreen", lwd = 2)
plot(jj_log_sdiff, main = "step 2: seasonal diff of log(jj) - seasonality removed",
     ylab = "seasonal diff", col = "darkred", lwd = 1.5)
abline(h = 0, lty = 2)
plot(jj_log_both, main = "step 3: first + seasonal diff of log(jj) - STATIONARY",
     ylab = "diff values", col = "darkblue", lwd = 1.5)
abline(h = 0, lty = 2)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

cat("interpretation of the transformation pipeline:\n")
cat("- step 0 → step 1: log transform handles the exponential growth\n")
cat("  and stabilizes the variance (seasonal swings become constant)\n")
cat("- step 1 → step 2: seasonal differencing (lag=4) removes quarterly pattern\n")
cat("- step 2 → step 3: first differencing removes remaining linear trend\n")
cat("- the final series (step 3) is approximately stationary and ready for\n")
cat("  ARIMA/SARIMA modeling\n\n")

#' # conclusion

cat("=" , rep("=", 60), "\n", sep = "")
cat("OVERALL CONCLUSIONS\n")
cat("=" , rep("=", 60), "\n\n", sep = "")

cat("part 1 - nile dataset (white noise testing):\n")
cat("1. the nile series is NOT a white noise process\n")
cat("2. it shows significant autocorrelation at lags 1 through 5\n")
cat("3. there is a structural break around 1898 causing a level shift\n")
cat("4. the acf decays slowly instead of immediately dropping to zero\n")
cat("5. true white noise would have no significant acf beyond lag 0\n\n")

cat("part 2 - johnsonjohnson dataset (stationarity and differencing):\n")
cat("1. the original jj series is non-stationary with trend, seasonality,\n")
cat("   and increasing variance\n")
cat("2. log transformation stabilizes the variance and linearizes the trend\n")
cat("3. first differencing alone removes trend but not seasonality\n")
cat("4. seasonal differencing (lag=4) removes the quarterly pattern\n")
cat("5. combining log + seasonal differencing + first differencing\n")
cat("   produces a stationary series suitable for ARIMA modeling\n")
cat("6. the full transformation is: diff(diff(log(jj), lag=4), differences=1)\n")
