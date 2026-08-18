# question  1

daily_exercise <- c(10, 15, 20, 30, 25, 40, 35, 50, 45, 60)
stress_score <- c(78, 72, 70, 65, 68, 60, 62, 55, 58, 52)
summary(daily_exercise)
summary(stress_score)
df_1 <- data.frame(daily_exercise, stress_score)
View(df_1)

# scatterplot
plot(
     x = daily_exercise,
     y = stress_score,
     main = "Scatterplot of Daily Exercise vs Stress Score",
     xlab = "Daily Exercise (minutes)",
     ylab = "Stress Score (0-100)",
     pch = 19,
     col = "green"
)
# correlation
prim_corr <- cor(daily_exercise, stress_score)
print(paste("Original Correlation:", round(prim_corr, 4)))
# inference:since -1<=corr<=1 and the value here is -0.99 the strength is very high but the direction of the quantitative variables is opposite
# interpretation: this indicates that the as the duration of exercise increases the  stress score decreases, this is also seen in the scatterplot as it shows the downward trend
ex_new <- c(daily_exercise, 5)
stress_new <- c(stress_score, 95)


# question 2
print(data.frame(ex_new, stress_new))

# scatterplot
plot(
     x = ex_new,
     y = stress_new,
     main = "Scatterplot with Outlier Included",
     xlab = "Daily Exercise (minutes)",
     ylab = "Stress Score (0-100)",
     pch = 19,
     col = "blue"
)
points(5, 95, col = "red", pch = 19, cex = 1.5)
legend("topright", legend = c("Original Data", "Outlier"), col = c("blue", "red"), pch = 19)
# correlation
corr_new <- cor(ex_new, stress_new)
print(paste("New Correlation:", round(corr_new, 4)))

diff_corr <- corr_new - prim_corr
print(paste("Difference between the new corr coeff and old corr coeff:", round(diff_corr, 4)))

# inference: adding the outliers weakened the strength of correlation coefficient by a magnitude of 0.0649
# interpretation: the addition of this outliers introduced noise/variance to the data, this deviates the linearity of the points than it was before
