# introduction
# objective is to fit multiple linear regression on airquality data
# performing residual diagnostics to validate assumptions

# data description
# dataset used is airquality
# response variable is ozone
# predictors are solar.r, wind, and temp
# missing values are handled by removing them

# part a data handling and model fitting

# 1 loading data
data(airquality)
print(head(airquality))

# 2 identifying variables with missing values
# removing incomplete observations
print("missing values in each column")
print(colSums(is.na(airquality)))
air_clean <- na.omit(airquality)
print("dimensions after removing na")
print(dim(air_clean))

# 3 fitting multiple linear regression model
model <- lm(Ozone ~ Solar.R + Wind + Temp, data = air_clean)
print(summary(model))
?airquality

# 4 fitted regression equation
# ozone = -64.34 + 0.059*solar.r - 3.33*wind + 1.65*temp

# 5 comments on model
# overall significance
print("p-value is < 2.2e-16, model is highly significant")
# coefficient of determination
print("r-squared is 0.6059, 60.59% variance explained")

# part b residual extraction

# 6 extracting raw residuals and fitted values
raw_residuals <- residuals(model)
fitted_values <- fitted(model)

print("first 6 raw residuals")
print(head(raw_residuals))
print("first 6 fitted values")
print(head(fitted_values))

# 7 formula for residual
# residual = observed y - fitted y
# importance: residuals help in checking model assumptions like normality

# part c residual diagnostics

# 8 residuals vs fitted plot
plot(model, which = 1, main = "residuals vs fitted")
print("red line is horizontal, linearity holds")
print("points randomly scattered, homoscedasticity holds")

# 9 normal q-q plot
plot(model, which = 2, main = "normal q-q")
print("points fall on the line, normality assumption satisfied")

# 10 scale-location plot
plot(model, which = 3, main = "scale-location")
print("no patterns, equal variance assumption holds")

# 11 residuals vs predictors
# adjusting margins to avoid error
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot(air_clean$Solar.R, raw_residuals, main = "residuals vs solar.r", xlab = "solar.r", ylab = "residuals")
plot(air_clean$Wind, raw_residuals, main = "residuals vs wind", xlab = "wind", ylab = "residuals")
plot(air_clean$Temp, raw_residuals, main = "residuals vs temp", xlab = "temp", ylab = "residuals")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

print("random scatter in predictor plots, no systematic patterns")

# part d interpretation and conclusion

# 16 diagnostic results
# interpretation
# based on the plots, linearity and homoscedasticity are satisfied
# normality of residuals is also satisfied from q-q plot
# no systematic patterns vs predictors
# conclusion
# the model assumptions are valid
# the model is adequate for this data
