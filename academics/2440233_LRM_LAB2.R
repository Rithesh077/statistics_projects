# DATA LOADING
# Years of schooling (X)
X <- c(15, 16, 8, 6, 15, 12, 12, 18, 12, 20, 
       17, 12, 12, 9, 15, 12, 16, 12, 12, 14)

# Hourly earnings in 1994 (Y)
Y <- c(17.24, 15.00, 14.91, 4.50, 18.00, 6.29, 19.23, 18.69, 7.21, 42.06, 
       15.38, 12.70, 26.00, 7.50, 5.00, 21.63, 12.10, 5.55, 7.50, 8.00)

# Create the Linear Regression Model
model <- lm(Y ~ X)
coeffs <- coef(model)
beta_0 <- coeffs[1] # Intercept
beta_1 <- coeffs[2] # Slope

# QUESTION 1: Estimate coefficients beta_0 and beta_1
cat("\n--- Question 1 ---\n")
cat(sprintf("Estimated Intercept (beta_0): %.4f\n", beta_0))
cat(sprintf("Estimated Slope (beta_1): %.4f\n", beta_1))
cat(sprintf("Regression Equation: Y = %.2f + %.2fX\n", beta_0, beta_1))

# QUESTION 2: What does beta_1 tell us?
cat("\n--- Question 2 ---\n")
cat("Interpretation of beta_1:\n")
cat(sprintf("It tells us that for every additional year of schooling, hourly earnings increase by approximately $%.2f on average.\n", beta_1))

# QUESTION 3: If beta_1 were negative?
cat("\n--- Question 3 ---\n")
cat("Scenario if beta_1 were negative:\n")
cat("It would imply that higher education leads to LOWER earnings. This is strange because it contradicts human capital theory, where skills gained from education usually command higher wages.\n")

# QUESTION 4: Hypothetical Equation Y = -2.5 + 2.1X
cat("\n--- Question 4 ---\n")
# Part A: Interpret Intercept
cat("a) Interpretation of intercept (-2.5):\n")
cat("   It represents the predicted hourly earnings for someone with 0 years of schooling.\n")

# Part B: Is it realistic?
cat("b) Is this realistic?\n")
cat("   No. First, you cannot earn negative money (-$2.50). Second, 0 years of schooling is an extrapolation far outside the data range (lowest X is 6).\n")

# QUESTION 5: Difference between 8 years and 16 years of schooling
cat("\n--- Question 5 ---\n")
# We use the actual estimated slope from Question 1 (beta_1)
years_diff <- 16 - 8
earnings_diff <- years_diff * beta_1

cat(sprintf("Using the estimated slope (%.4f):\n", beta_1))
cat(sprintf("Difference in years: %d\n", years_diff))
cat(sprintf("Expected earnings difference: %d * %.4f = $%.2f\n", years_diff, beta_1, earnings_diff))
cat(sprintf("Answer: The person with 16 years is expected to earn $%.2f more per hour.\n", earnings_diff))

# QUESTION 6: Change in beta_1 from 1.20 to 0.80
cat("\n--- Question 6 ---\n")
cat("Change from 1.20 to 0.80:\n")
cat("1. Strength of relationship: The relationship is weaker in terms of monetary return. Education still increases earnings, but the 'payoff' per year is lower.\n")
cat("2. Less productive? In economic terms, yes. It implies the market values the productivity gained from an extra year of schooling less than before (80 cents vs $1.20).\n")