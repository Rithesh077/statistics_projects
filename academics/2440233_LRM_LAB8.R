# aim
# to understand and implment forward selecion and backward eliminasion
# for selecting optimal multiple linear regresion model

# dataset descripson
# using boston dataset from mass package
# response variable is medv
# potential predictors are all other variables like crim on indus chas nox rm age dis rad tax ptratio black lstat

# question 1 data exploration
# 1 loading datset
library(MASS)
data(Boston)

# 2 displaying structre and summary stats
print("structure of boston datset")
print(str(Boston))
print("summry statistics")
print(summary(Boston))

# 3 identify variables
# response variable medv
# predictors are remaining 13 variables

# question 2 full and null models
# 1 fiting full model
full_model <- lm(medv ~ ., data = Boston)

# 2 fiting null model
null_model <- lm(medv ~ 1, data = Boston)

# 3 extracting r squared and adjusted r squared
print("full model summary")
print(summary(full_model))
print("r sq of full modle")
print(summary(full_model)$r.squared)
print("adj r sq of full modle")
print(summary(full_model)$adj.r.squared)

# 4 comment on adequacy
# full model is signifcant but inclusion of non signifcant variables might increase complexity

# question 3 forward selecion
# 1 applying forward selecion using aic
print("startin forward selecion")
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# 2 order of variables entring is shown in output
# 3 report final seleted model
print("final summary of forward modle")
print(summary(forward_model))

# 4 extract r squared values
print("r sq forward")
print(summary(forward_model)$r.squared)
print("adj r sq forward")
print(summary(forward_model)$adj.r.squared)

# 5 interpretation
# model includes only predictors that contribute signifcantly to lowering aic

# question 4 backward eliminasion
# 1 applying backward eliminasion using aic
print("startin backward eliminasion")
backward_model <- step(full_model, direction = "backward")

# 2 variables removed shown in step output
# 3 report final model
print("final summary of backward modle")
print(summary(backward_model))

# 4 extract r squared valus
print("r sq backward")
print(summary(backward_model)$r.squared)
print("adj r sq backward")
print(summary(backward_model)$adj.r.squared)

# 5 interpretation
# insiginficant predictors removd to simplify model without loseing fit

# question 5 model comparison
# 1 comparing full forward and backward using aic
print("aic comparison")
print(AIC(full_model, forward_model, backward_model))

# 2 table of comparison
print("model comparison table")
results <- data.frame(
    Model = c("Full", "Forward", "Backward"),
    R2 = c(summary(full_model)$r.squared, summary(forward_model)$r.squared, summary(backward_model)$r.squared),
    Adj_R2 = c(summary(full_model)$adj.r.squared, summary(forward_model)$adj.r.squared, summary(backward_model)$adj.r.squared),
    AIC = c(AIC(full_model), AIC(forward_model), AIC(backward_model))
)
print(results)

# 3 identify best model
# looking at table forward and backward models are same and better than full model due to lower aic
# they are preffered due to simplicity (parsimony)

# result
# optimal model seleted using aic criterion
# forward and backward methods yielded simlar results

# conclusion
# variable selecion helps in removing noise variables and finding best fit
# step function in r makes it easy to perfom these algorithm
