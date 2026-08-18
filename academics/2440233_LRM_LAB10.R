# labassignment10
# multicollinearity in multiple linear regression

# introduction
# multicollinearity occurs when independent variables in a regression model are highly correlated
# it does not reduce the predictive power or goodness of fit of the model as a whole
# however it can make the estimates of individual regression coefficients erratic and unreliable
# detecting multicollinearity is crucial for interpreting the role of each predictor accurately

# datadescription
# using longley dataset built in in r
# dataset contains macroeconomic variables observed yearly
# response variable is employed
# predictors are gnp deflator gnp unemployed armed forces population and year

data(longley)

# question1
# displaying structure and variables
print("structure of dataset")
print(str(longley))
print("variables list")
print(names(longley))

print("inference the response variable is employed and there are six potential explanatory variables")

# interpretation
# the structure function reveals that all variables are numeric
# this makes it readily applicable for multiple linear regression without dummy coding

# question2
# fitting multiple linear regression model
model <- lm(Employed ~ ., data = longley)

print("regression model coefficients")
print(coef(model))

print("inference estimated regression equation is employed = -3482258.6 + 15.06*gnp.deflator - 0.0358*gnp - 2.02*unemployed - 1.03*armed.forces - 0.51*population + 1829.15*year")

# interpretation
# the coefficients indicate the expected change in employment for one unit change in a predictor
# holding all other predictors constant
# some coefficient like gnp and population are negative which might be counter intuitive due to multicollinearity

# question3
# computing correlation matrix for explanatory variables
cor_matrix <- cor(longley[, -which(names(longley) == "Employed")])
print("correlation matrix")
print(cor_matrix)

print("inference variables like gnp.deflator gnp population and year show extremely high correlation near 0.99 with each other")

# interpretation
# the correlation matrix shows several off diagonal values very close to one
# this indicates strong linear relationships between pairs of predictors
# for example the correlation between gnp and year is approx 0.995
# this suggests a possibility of severe multicollinearity in the model

# question4
# drawing scatterplot matrix
pairs(longley[, -which(names(longley) == "Employed")], main = "scatterplot matrix of predictors")

print("inference scatterplots confirm strong positive linear relationships among year gnp population and gnp.deflator")

# interpretation
# the scatterplot matrix visually corroborates the findings of the correlation matrix
# tight linear clusters between specified variables indicate high collinearity
# unemployed and armed forces seem to have weaker linear relationships compared to the others

# question5
# computing variance inflation factor
if (!require(car)) install.packages("car")
library(car)
vif_values <- vif(model)
print("vif values")
print(vif_values)

print("inference gnp year gnp.deflator and population exhibit severe multicollinearity with vif greater than 10")

# interpretation
# vif quantifies how much the variance of an estimated regression coefficient is increased
# a vif value threshold of 10 is commonly used to indicate severe multicollinearity
# here gnp 1788 and year 758 show extreme vifs confirming severe inflation
# unemployed and armed forces have moderate vifs less than 40

# question6
# examining standard errors
print("standard errors of original model")
print(summary(model)$coefficients[, "Std. Error"])

print("inference standard errors are relatively large compared to coefficients especially for variables with high vif")

# interpretation
# multicollinearity inflates the variances and standard errors of the coefficient estimates
# large variances make it difficult to detect statistical significance for the affected variables
# this is highly correlated predictors provide redundant information making it hard to isolate their individual effects

# question7
# refitting model after removing gnp due to high vif and correlation
model_reduced <- lm(Employed ~ . - GNP, data = longley)

print("reduced model coefficients and standard errors")
print(summary(model_reduced)$coefficients)

print("inference removing gnp generally decreased standard errors for remaining collinear variables like year and population")

# interpretation
# by dropping a heavily collinear predictor practical multicollinearity is reduced
# the standard errors of the coefficients in the partially collinear set improved
# this makes the remaining estimates slightly more precise and reliable
# the overall model is now more parsimonious while addressing the collinearity issue

# conclusion
# multicollinearity poses a significant challenge in multiple linear regression by inflating standard errors
# using tools like variance inflation factor and correlation matrices helps in effectively diagnosing this issue
# addressing it through variable removal or other techniques leads to more reliable and interpretable models
