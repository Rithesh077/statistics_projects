# INTRODUCTION
# The objective of this analysis is to perform Multiple Linear Regression (MLR) 
# on the 'swiss' dataset. We aim to model the relationship between a province's 
# Fertility measure and various socio-economic indicators.
#
# The model assumes the form: 
# Y = beta0 + beta1*X1 + beta2*X2 + beta3*X3 + error
#
# We will fit the model using Ordinary Least Squares (OLS), check the significance 
# of individual predictors using t-tests, and assess the overall model fit using 
# the F-test and ANOVA.

# DATA DESCRIPTION
# Dataset: 'swiss' (Standardised fertility measure and socio-economic indicators 
# for each of 47 French-speaking provinces of Switzerland at about 1888).
#
# Variables used in this analysis:
# 1. Response Variable (Y): 
#    - Fertility: Common standardised fertility measure.

# 2. Predictor Variables (X):
#    - Agriculture (X1): % of males involved in agriculture as an ocupation.
#    - Education (X2): % of draftees receiving highest mark on army examination.
#    - Catholic (X3): % of the population that is Catholic

# Load necessary library for plotting
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Q1: Load Data
data(swiss)

print("Q1: First 6 Observations")
print(head(swiss))

# Q2: Variable Selection Justification
print("Q2: Justification of Variables")
print("Response: Fertility.")
print("Predictors: Agriculture (Economic), Education (Social), Catholic (Cultural).")

# COMMENT: INTERPRETATION
# We selected these specific variables because they represent three distinct dimensions:
# 1. Agriculture: Represents the rural/agrarian nature.
# 2. Education: Represents modernization and awareness.
# 3. Catholic: Represents religious influence on family planning.

# Q3: Fit the Multiple Linear Regression Model
model <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)

# extract coeficients
coeffs <- coef(model)
b0 <- round(coeffs["(Intercept)"], 2)
b1 <- round(coeffs["Agriculture"], 2)
b2 <- round(coeffs["Education"], 2)
b3 <- round(coeffs["Catholic"], 2)

print("Q3: Fitted Regression Equation")
print(paste0("Fertility = ", b0, " + (", b1, ")*Agriculture + (", b2, ")*Education + (", b3, ")*Catholic"))

# Q4: Model Summary and Coeficients
print("Q4: Model Summary")
model_sum <- summary(model)
coef_matrix <- model_sum$coefficients

# Estimates and Std. Errors
print(coef_matrix[, c("Estimate", "Std. Error")])

# COMMENT: INTERPRETATION
# The 'Estimate' column gives us our Beta values.
# The 'Std. Error' indicates precision; smaller errors imply more precise estimates.

# Q5: Null and Alternative Hypotheses (Individual Parameters)
print("Q5: Hypotheses for Coefficients")
print("H0: beta_i = 0 (Variable has NO effect)")
print("H1: beta_i != 0 (Variable HAS an effect)")

# COMMENT: THEORY
# We test each beta individually to see if the predictor adds value to the model 
# given that the other predictors are already included.

# Q6: t-tests for Significance
print("Q6: t-statistics and p-values")
# Extracting specific colums
print(coef_matrix[, c("t value", "Pr(>|t|)")])

# Q7: Identify Significant Predictors
print("Q7: Significant Variables (alpha = 0.05)")

# filter variables where p-value < 0.05
p_values <- coef_matrix[, "Pr(>|t|)"]
sig_vars <- names(p_values[p_values < 0.05])
# Remove Intercept from the list for cleaner reporting
sig_vars <- sig_vars[sig_vars != "(Intercept)"]

print(paste("The statistically significant predictors are:", paste(sig_vars, collapse = ", ")))

# COMMENT: INTERPRETATION
# We programmatically checked which p-values were less than 0.05.
# Based on the output, Agriculture, Education, and Catholic are all significant.
# We reject H0 for these specific variables.

# Q8: Hypotheses for Overall Model Significance
print("Q8: Overall Model Hypotheses")
print("H0: beta_1 = beta_2 = beta_3 = 0 (Model explains NO variation)")
print("H1: At least one beta_i != 0 (Model explains SIGNIFICANT variation)")

# Q9: F-test for Overall Significance
# Dynamically calculate F-stats
f_stat_val <- model_sum$fstatistic[1]
df1 <- model_sum$fstatistic[2]
df2 <- model_sum$fstatistic[3]
# Calculate P-value dynamically
p_val_f <- pf(f_stat_val, df1, df2, lower.tail = FALSE)

print("Q9: F-test Results")
print(paste("F-statistic:", round(f_stat_val, 2)))
print(paste("p-value:", format(p_val_f, scientific = TRUE)))

# Check significance dynamically
significance_check <- ifelse(p_val_f < 0.05, "Significant", "Not Significant")
print(paste("Conclusion: The overall model is", significance_check))

# COMMENT: INTERPRETATION
# Since the p-value is extremely small (< 0.05), we REJECT H0.
# The model contains useful information for predicting Fertility.

# Q10: ANOVA Table
print("Q10: ANOVA Table")
anova_res <- anova(model)
print(anova_res)

# COMMENT: INTERPRETATION
# The ANOVA table confirms the sequential contribution of each variable.
# Variables with high 'Sum Sq' and low 'Pr(>F)' contribute most to the model fit.

# Q11: Practical Interpretation of Coefficients
print("Q11: Practical Interpretation")
print(paste("Effect of Education:", b2))
print(paste("Effect of Catholic:", b3))

# COMMENT: INTERPRETATION
# 1. Education: Holding other variables constant, a 1-unit increase in Education
#    is associated with a decrease in Fertility of approx 'b2' units.
# 2. Catholic: Holding other variables constant, a 1-unit increase in Catholic
#    percentage is associated with an increase in Fertility of approx 'b3' units.

# Q12: Conclusion
print("Q12: Final Conclusion")
print("Regression analysis confirms that Education, Agriculture, and Religion significantly impact Fertility.")
print(paste("Education has a strong negative impact (", b2, "), while Catholicism has a positive impact (", b3, ")."))

# VISUALISATION
# Visualising Fertility vs Education, grouped by Majority Catholic

swiss$MajorityCatholic <- ifelse(swiss$Catholic > 50, "Catholic > 50%", "Protestant/Mixed")

plot <- ggplot(swiss, aes(x = Education, y = Fertility, color = MajorityCatholic)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fertility vs Education (Grouped by Religion)",
       subtitle = "Visualizing the fitted trends",
       x = "Education",
       y = "Fertility Index") +
  theme_minimal()

print(plot)

# COMMENT: PLOT INTERPRETATION
# The plot shows the negative trend of Education (downward slope).
# It also visually separates the data by religious dominance, aligning with
# our finding that 'Catholic' is a significant predictor.

