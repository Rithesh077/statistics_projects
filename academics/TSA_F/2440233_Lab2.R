#' ---
#' title: "time series analysis - lab 2"
#' subtitle: "exploratory time series data analysis"
#' author: "2440233"
#' date: "`r Sys.Date()`"
#' output:
#'   pdf_document:
#'     toc: true

#' # introduction
#'
#' the objective of this lab is to perform exploratory time series data
#' analysis on a real world dataset. we will look at the structure of the
#' data, plot it, identify the components, and figure out whether an
#' additive or multiplicative model fits better.

#' # methodology

# dataset used: AirPassengers (built in dataset)
# it has monthly totals of international airline passengers from
# jan 1949 to dec 1960. thats 144 observations, monthly frequency.
# source: box, jenkins and reinsel (1976).

#' # analysis

#' ## q1: describe etsda in 10 lines

cat("q1: exploratory time series data analysis\n\n")
cat("1. etsda is used to understand the structure and patterns in time ordered data.\n")
cat("2. we use time plots to visually check for trends, seasonality and irregularities.\n")
cat("3. summary stats like mean, variance and range help quantify the data.\n")
cat("4. it helps figure out if the data has constant or changing behavior over time.\n")
cat("5. the series can be broken down into trend, seasonality, cyclical and random parts.\n")
cat("6. tools like line plots, box plots and lag plots help explore patterns.\n")
cat("7. it helps detect outliers and structural breaks in the data.\n")
cat("8. we can check if variance changes over time which tells us if we need transformations.\n")
cat("9. comparing variances of components helps decide between additive and multiplicative models.\n")
cat("10. all of this helps us pick the right model to represent the time series.\n\n")

#' ## q2: components of time series

cat("q2: components of time series data\n\n")
cat("1. trend - the long term direction of the data, going up down or staying flat.\n")
cat("2. seasonality - repeating patterns at fixed intervals like monthly or yearly.\n")
cat("3. cyclical - long term waves with no fixed period, usually linked to economic cycles.\n")
cat("4. irregular - the leftover random noise after removing the other components.\n\n")

#' ## q3: plot and examine the data

cat("q3: plot the time series and examine its behavior\n\n")

data("AirPassengers")
ap <- AirPassengers

cat("class:", class(ap), "\n")
cat("start:", start(ap)[1], "/", start(ap)[2], "\n")
cat("end:", end(ap)[1], "/", end(ap)[2], "\n")
cat("frequency:", frequency(ap), "\n")
cat("observations:", length(ap), "\n\n")

cat("first 12 values:\n")
print(head(ap, 12))
cat("\nlast 12 values:\n")
print(tail(ap, 12))

cat("\nsummary:\n")
print(summary(ap))
cat("sd:", round(sd(ap), 2), "\n\n")

#+ fig.width=10, fig.height=6
plot(ap,
     main = "fig 1: monthly airline passengers (1949-1960)",
     ylab = "passengers (thousands)",
     xlab = "year",
     col = "blue", lwd = 2)
grid()

cat("interpretation of fig 1:\n")
cat("- there is a clear upward trend from 1949 to 1960\n")
cat("- seasonal peaks happen every summer and dips every winter\n")
cat("- the seasonal swings get bigger as the overall level increases\n")
cat("- this suggests a multiplicative model would be better than additive\n\n")

#+ fig.width=10, fig.height=6
boxplot(ap ~ cycle(ap),
        main = "fig 2: monthly distribution of passengers",
        xlab = "month", ylab = "passengers (thousands)",
        col = rainbow(12), names = month.abb)

cat("interpretation of fig 2:\n")
cat("- jul and aug have the highest passenger counts\n")
cat("- nov and feb have the lowest\n")
cat("- the box sizes get bigger for higher months confirming multiplicative seasonality\n\n")

#+ fig.width=10, fig.height=6
ap_matrix <- matrix(ap, nrow = 12, ncol = 12)
matplot(1:12, ap_matrix, type = "l", lty = 1, lwd = 1.5,
        col = rainbow(12),
        main = "fig 3: year-wise seasonal overlay",
        xlab = "month", ylab = "passengers (thousands)", xaxt = "n")
axis(1, at = 1:12, labels = month.abb)
legend("topleft", legend = 1949:1960, col = rainbow(12),
       lty = 1, cex = 0.6, ncol = 3, bty = "n")

cat("interpretation of fig 3:\n")
cat("- every year follows the same seasonal shape\n")
cat("- later years are higher up showing the trend\n")
cat("- the gap between years is bigger during peak months than trough months\n\n")

#' ## q4: identify components

cat("q4: identify the components in the dataset\n\n")

decomp_mult <- decompose(ap, type = "multiplicative")

#+ fig.width=10, fig.height=8
plot(decomp_mult, col = "blue")

cat("interpretation of fig 4:\n")
cat("- observed: the original series with everything combined\n")
cat("- trend: smooth increasing curve from about 130 to 430\n")
cat("- seasonal: repeating pattern around 1.0, above 1 means above average month\n")
cat("- random: fluctuates around 1.0 with no pattern, model fits well\n\n")

cat("table 1: seasonal indices\n")
si <- decomp_mult$figure
si_df <- data.frame(month = month.abb,
                    index = round(si, 4),
                    effect = ifelse(si > 1,
                      paste0(round((si - 1) * 100, 2), "% above trend"),
                      paste0(round((1 - si) * 100, 2), "% below trend")))
print(si_df, row.names = FALSE)

cat("\n- july is highest at 1.2255 meaning 22.55% above trend\n")
cat("- november is lowest at 0.8068 meaning 19.32% below trend\n\n")

cat("table 2: trend at mid year\n")
trend_vals <- decomp_mult$trend
cat("year  | trend\n")
for (yr in 1949:1960) {
  val <- window(trend_vals, start = c(yr, 7), end = c(yr, 7))
  if (length(val) > 0 && !is.na(val[1]))
    cat(sprintf("%d  |  %.1f\n", yr, val[1]))
}
cat("\n")

decomp_add <- decompose(ap, type = "additive")

#+ fig.width=10, fig.height=8
plot(decomp_add, col = "red")

cat("interpretation of fig 5:\n")
cat("- trend looks similar to multiplicative\n")
cat("- seasonal component has constant amplitude\n")
cat("- but the random component has increasing variance over time\n")
cat("- that means additive model doesnt fit properly\n\n")

#' ## q5: model selection

cat("q5: additive vs multiplicative model\n\n")

cat("additive model: y(t) = t(t) + s(t) + c(t) + i(t)\n")
cat("- use when seasonal swings stay the same regardless of trend level\n\n")
cat("multiplicative model: y(t) = t(t) x s(t) x c(t) x i(t)\n")
cat("- use when seasonal swings grow with the trend level\n\n")

resid_mult <- na.omit(decomp_mult$random)
resid_add <- na.omit(decomp_add$random)

cat("multiplicative residuals:\n")
cat("  mean:", round(mean(resid_mult), 4), "(should be close to 1)\n")
cat("  sd:", round(sd(resid_mult), 4), "\n\n")

cat("additive residuals:\n")
cat("  mean:", round(mean(resid_add), 2), "(should be close to 0)\n")
cat("  sd:", round(sd(resid_add), 2), "\n\n")

#+ fig.width=10, fig.height=8
par(mfrow = c(2, 1))
plot(na.omit(decomp_add$random),
     main = "fig 6a: additive residuals",
     ylab = "residual", col = "red", lwd = 1.5)
abline(h = 0, lty = 2)
plot(na.omit(decomp_mult$random),
     main = "fig 6b: multiplicative residuals",
     ylab = "residual", col = "blue", lwd = 1.5)
abline(h = 1, lty = 2)
par(mfrow = c(1, 1))

cat("interpretation of fig 6:\n")
cat("- additive residuals fan out over time which is bad\n")
cat("- multiplicative residuals stay constant around 1 which is good\n\n")

#+ fig.width=10, fig.height=6
plot(log(ap),
     main = "fig 7: log transformed passengers",
     ylab = "log(passengers)", xlab = "year",
     col = "darkgreen", lwd = 2)
grid()

cat("interpretation of fig 7:\n")
cat("- after log transform the seasonal swings become constant\n")
cat("- this confirms the original data is multiplicative\n")
cat("- because log turns multiplication into addition\n\n")



cat("selected model: multiplicative\n")
cat("y(t) = t(t) x s(t) x i(t)\n\n")
cat("justification:\n")
cat("1. the seasonal swings get bigger as trend goes up (fig 1, fig 3)\n")
cat("2. multiplicative residuals have constant variance, additive dont (fig 6)\n")
cat("3. log transform makes seasonal amplitude constant confirming multiplicative (fig 7)\n")
cat("4. seasonal indices as ratios around 1 make more sense than fixed constants\n")
cat("5. multiplicative decomposition gives cleaner residuals than additive (fig 4 vs 5)\n\n")

# conclusion
# the airpassengers data has a clear upward trend and monthly seasonality.
# the seasonal effect grows with the trend so a multiplicative model fits best.
# this was confirmed by residual analysis, log transformation, and decomposition.

#' # conclusion

cat("conclusion:\n\n")
cat("the airpassengers data has a clear upward trend and monthly seasonality.\n")
cat("the seasonal effect grows with the trend so multiplicative model fits best.\n")
cat("this was confirmed by residual analysis, log transformation and decomposition.\n")
