# INTRODUCTION
# The objective of this laboratory assignment is to analyze the relationship 
# between two quantitative variables using Simple Linear Regression (SLR). 
# The goal is to fit a linear model of the form: Y = beta0 + beta1*X + error.
#
# Using the method of Ordinary Least Squares (OLS), we will estimate the 
# intercept (beta0) and the slope (beta1) to quantify how changes in the 
# predictor variable influence the response variable. Furthermore, we will 
# assess the validity and strength of this model by:
# 1. Testing the statistical significance of the regression coefficients (t-test).
# 2. Evaluating the Goodness of Fit using the Coefficient of Determination (R^2).
# 3. Validating the overall model significance using ANOVA.

# DATA DESCRIPTION
# For this analysis, we utilize the built-in R dataset named 'women'. 
# This dataset provides average height and weight data for American women 
# aged 30–39.
#
# - Dataset Name: women
# - Sample Size (n): 15 observations.
# - Variables:
#     1. Predictor Variable (X): 'height' (in inches). Independent variable.
#     2. Response Variable (Y): 'weight' (in lbs). Dependent variable.
# ==============================================================================

# Load the dataset
data(women)

# ==============================================================================
# PART A: Exploring the Dataset
# Theory: Before fitting a model, we must understand the data structure.
# We look at dimensions to know sample size (n) and summary statistics 
# (mean, min, max) to detect outliers and understand the central tendency.
# ==============================================================================

# Q1: Display structure and summary
print("Data Snapshot:")
head(women)
print(paste("Dimensions:", dim(women)[1], "rows and", dim(women)[2], "columns"))
summary(women)

# --- Q2 Interpretation ---
cat("\nPART A: INFERENCE:\n")
print("Q2a: Avg Height = 65.0 in; Avg Weight = 136.7 lbs.")
print("Q2b: Height range: [58, 72]; Weight range: [115, 164].")
print("Q2c: The data is reasonably spread out, covering a diverse range of body types.")


# PART B: Fitting the Simple Linear Regression Model
# Theory: We fit the model Y = beta0 + beta1*X + error using Ordinary Least Squares (OLS).
# - beta0 (Intercept): Expected Y when X is 0.
# - beta1 (Slope): Expected change in Y for a 1-unit change in X.
# The 'lm()' function minimizes the sum of squared residuals to estimate these.

# Q3: Fit the model and show summary
model <- lm(weight ~ height, data = women)
print("Model Summary:")
print(summary(model))

# Extract coefficients for clean printing
b0 <- round(coef(model)[1], 2) # Intercept
b1 <- round(coef(model)[2], 2) # Slope

#Q4 & Q5 Interpretation
cat("\nPART B: INFERENCE\n")
print(paste0("Q4: Fitted Equation: weight = ", b0, " + ", b1, " * height"))
print(paste0("Q5: For every 1-inch increase in height, weight increases by approx ", b1, " lbs."))


# PART C: Significance of Regression Coefficients
# Theory: We test if the predictor (height) actually helps predict the response.
# We perform a t-test on the slope coefficient.
# - Null Hypothesis (H0): beta1 = 0 (No linear relationship).
# - Alt Hypothesis (H1): beta1 != 0 (Significant linear relationship).
# If p-value < 0.05, we reject H0.

# Q6: Extract testing metrics
t_stat <- summary(model)$coefficients["height", "t value"]
p_val  <- summary(model)$coefficients["height", "Pr(>|t|)"]

# Q6 & Q7 Interpretation
cat("\nPART C: INFERENCE:\n")
print("Q6a: H0: Slope is 0 (no effect). H1: Slope is not 0 (significant effect).")
print(paste0("Q6b: t-statistic = ", round(t_stat, 2), ", p-value = ", format(p_val, scientific=TRUE)))
print("Q6c: p-value < 0.05. Height significantly predicts weight.")
print("Q7: Intercept (-87.52) is weight at height 0. Physically meaningless in this context.")


# PART D: Goodness of Fit (R-squared)
# Theory: R-squared (Coefficient of Determination) measures the proportion of 
# variance in the response variable (Weight) that can be predicted from the 
# explanatory variable (Height). Ranges from 0 to 1.

# Q8: Extract R-squared
r2 <- summary(model)$r.squared

#Q8 Interpretation
cat("\nPART D: INFERENCE:\n")
print(paste0("Q8a: R-squared value = ", round(r2, 4)))
print("Q8b: 99.1% of the variation in weight is explained by height. Excellent fit.")


# PART E: ANOVA for the Regression Model
# Theory: ANOVA (Analysis of Variance) tests the overall model significance using the F-test.
# It splits total variation (SST) into Regression (SSR) and Residual (SSE) variance.
# - H0: The model explains no variation (beta1 = 0).
# - H1: The model explains significant variation.
# In Simple Linear Regression, the F-test result is mathematically equivalent to the t-test.

# Q10: ANOVA Table
print("ANOVA Table:")
anova_output <- anova(model)
print(anova_output)

# Q10 Interpretation
cat("\nPART E: INFERENCE:\n")
print("Q10a: H0: Model is insignificant. H1: Model is significant.")
print("Q10b: The F-value is 1433 with a p-value near zero.")
print("Q10c: Conclusion: Reject H0. The regression model is statistically significant.")