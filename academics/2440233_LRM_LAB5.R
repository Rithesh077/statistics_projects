# INTRODUCTION
# Multiple Linear Regrssion (MLR) is a statistical technique that uses several
# explanatory variables to predict the outcome of a response variable.
# The gaol is to model the linear relationship between the explanatory (independent)
# variables and response (dependent) variable.
# Formula: Y b0 + b1*x1 + b2*x2 + ... + bn*xn + e

# DATA DESCRIPTION
# We are using the 'mtcars' dataset and the 'marketing' dataset from the 'datarium' package.
# The marketing dataset contains the impact of three advertising medias (youtube, facebook,
# and newspaper) on sales. It has three predictos (independent varibles) and one
# response variable (sales).
# n 200 observations.

# Load necessary libraries and data
if (!require(datarium)) install.packages("datarium")
library(datarium)
data("marketing")
data("mtcars")

# TASK 1: Identify and report the estimated regression coefficients
# Fitting the model
model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)

# Inference
cat("\n TASK 1 INFERENCE \n")
print("The estimated regression coefficents (beta values) are:")
print(coef(model))
print("Interpretation: The intercept (beta0) is the expected sales when all budgets are zero.")
print("The other coefficients represent the change in sales for a oneunit increase in the respective budget.")

# TASK 2: Compute and interpret the coefficient of determination (R2)
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared

# Inference
cat("\n TASK 2 INFERENCE \n")
print(paste("The Coefficient of Determnation (Rsquared) is:", round(r_squared, 4)))
print(paste("This means that approximately", round(r_squared * 100, 2), "% of the variation in sales"))
print("can be explained by the advertising budgets on Youtube, Facebook, and Newspaper.")

# TASK 3: Report t-statistic and p-value for each coefficient
model_summary <- summary(model)
coefficients_table <- model_summary$coefficients

# Inference
cat("\n TASK 3 INFERENCE \n")
print("The t-statistics and p-values for the regression coefficients are:")
print(coefficients_table[, c("t value", "Pr(>|t|)")])
print("Statistcal Significance Comment:")
print("Yputube and Facebook have p-values < 0.05, indicating they are significantly related to sales.")
print("Newspaper has a high p-value, suggesting it is NOT a significant predictor in this multivariable modle.")

# TASK 4: Compute and plot residuals
residuals_values <- residuals(model)

# Plotting
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(model)
par(mfrow = c(1, 1))

# Inference
cat("\n TASK 4 INFERENCE \n")
print("Residuals have been computed and plotted.")
print("Observing the Residuals vs Fitted plot, we look for randomness.")
print("Any distinct patterms would indicate that the linear model might need improvement.")
print("Ideally, residuals should be randomly distributed around the horizontal line.")

# CONCLUSION
# The analysis shows that Youtube and Facebook advertising budgets have a strong
# positive impact on sales. Newspaper advertising, however, does not appear to
# have a significant effect in the presence of the other two predictors.
# The high Rsquared value confirms that our model fits the data very well.
# Final Conclusion: The model is effective for predicting sales based on digital ad spend.
