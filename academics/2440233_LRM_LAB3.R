# ==========================================
# INTRODUCTION & THEORY
# ==========================================
# We are analyzing the 'mtcars' dataset.
# The goal is to fit a Simple Linear Regression model:
#   mpg = beta0 + beta1 * wt + error
#
# Theory:
# 1. We use Ordinary Least Squares (OLS) to find the line.
# 2. OLS finds the line that minimizes the sum of squared errors.
# 3. Ideally, residuals (errors) should sum to zero.

# Load data
data(mtcars)

# ==========================================
# QUESTION 1: FITTING THE MODEL
# ==========================================

# Fit the model (mpg is response y, wt is predictor x)
model <- lm(mpg ~ wt, data = mtcars)

# Extract coefficients for the equation
intercept <- coef(model)[1]
slope <- coef(model)[2]

# --- Q1 Interpretation & Inference ---
print("--- QUESTION 1 OUTPUT ---")

# a) Write the fitted regression line
print(paste("The fitted regression line is: mpg =", 
            round(intercept, 4), "+ (", round(slope, 4), ") * wt"))

# b) Interpret Beta0 (Intercept) and Beta1 (Slope)
print("Interpretation of Intercept (Beta0):")
print(paste("Theoretically, a car weighing 0 lbs would satisfy", 
            round(intercept, 2), "mpg. (This is just a baseline, physically impossible)."))

print("Interpretation of Slope (Beta1):")
print(paste("For every 1 unit increase in weight (1000 lbs), the mpg decreases by", 
            round(abs(slope), 2), "."))


# ==========================================
# QUESTION 2: RESIDUAL PROPERTIES
# ==========================================

# Theory on Residuals:
# 1. Residuals (e) = Observed(y) - Fitted(y_hat).
# 2. The OLS method mathematically guarantees three properties:
#    - The sum of residuals = 0
#    - The sum of (residuals * x) = 0
#    - The sum of (residuals * fitted_values) = 0
# 3. This ensures the line passes through the mean and extracts all linear patterns.

# Get the necessary values
residuals_vec <- resid(model)      # e
fitted_vals   <- fitted(model)     # y_hat
x_vals        <- mtcars$wt         # x

# a) Compute the sums
sum_e       <- sum(residuals_vec)
sum_e_x     <- sum(residuals_vec * x_vals)
sum_e_yhat  <- sum(residuals_vec * fitted_vals)

# --- Q2 Interpretation & Inference ---
print("--- QUESTION 2 OUTPUT ---")

# Report the numerical results
print("Sum of Residuals:")
print(sum_e)

print("Sum of Residuals * x (Weight):")
print(sum_e_x)

print("Sum of Residuals * Fitted Values:")
print(sum_e_yhat)

# b) Compare with theory and explain
print("Comparison to Theory:")
print("Theoretically, all these sums should be exactly zero.")

print("Why they appear as tiny numbers (e.g., e-15) in R:")
print("Computers use 'floating-point arithmetic.' They cannot store infinite precision.")
print("These numbers are scientific notation for 0.0000000...1.")
print("We consider the properties to hold because these values are effectively zero.")