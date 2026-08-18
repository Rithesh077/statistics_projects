# labassignment9
# autocorrelation of residuals in regression analysis
?longley
# introduction
# autocorrelation refers to the correlation between successive residuals in a time series data
# it violates the assumption of independence of errors in linear regression model leading to bias
# detecting autocorrelation is essential for valid statistical inference
# common methods of detection include residual plots acf plots and statistical tests like durbin watson

# data description
# using longley dataset from r base library
# dataset contains macroeconomic variables observed yearly from 1947 to 1962
# response variable is employed representing the number of people employed
# predictors are gnp deflator gnp unemployed armed forces population and year

data(longley)

# structure and summary
print("structure of data")
print(str(longley))
print("summary of data")
print(summary(longley))

# inference
# dataframe with 16 observations and 7 numeric variables showing macroeconomic trends

# regression model fitting
# fitting multiple linear regression model of employed on all other predictors using lm function
model <- lm(Employed ~ ., data = longley)

# model summary
print("regression model summary")
print(summary(model))

# inference
# model has r2 value of 0.99 indicating excellent fit and p value is less than 0.05 indicating significance

# interpretation
# the high r squared value implies that 99 percent of the variation in the response variable employed
# can be explained by the linear relationship with the predictors in the model
# the f statistic is significant meaning the overall model is valid statistically
# however individual t tests for coefficients may show non significance due to potential multicollinearity among predictors

# residual analysis time plot
# extracting residuals from the fitted model and plotting against time to visually check for patterns
res <- residuals(model)
plot(longley$Year, res, type = "b", main = "residuals vs year", xlab = "year", ylab = "residuals")
abline(h = 0, col = "red")

# inference
# residuals show a distinct wave like pattern over time instead of random scatter around zero

# interpretation
# the meandering pattern of residuals suggests that positive autocorrelation is present in the error terms
# successive residuals tend to have similar signs and magnitudes rather than being independent
# this visual pattern is a strong indicator that the assumption of independent errors is violated

# autocorrelation function
# calculating and plotting the autocorrelation function acf of residuals to measure serial correlation at different lags
acf(res, main = "acf of residuals")

# inference
# acf plot shows significant positive correlation at lags 1 and 2

# interpretation
# the bars for lag 1 and lag 2 extend beyond the blue dashed confidence limits
# this indicates statistically significant autocorrelation at these lags meaning current error is related to previous errors
# this confirms the presence of positive autocorrelation in the residuals consistent with the time plot

# durbin watson test
# performing durbin watson test to statistically confirm the presence of first order autocorrelation
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)
print("durbin watson test result")
print(dwtest(model))

# hypothesis
# null hypothesis h0 autocorrelation is zero meaning errors are independent
# alternative hypothesis h1 autocorrelation is greater than zero meaning positive autocorrelation exists

# inference
# p value is less than 0.05 and dw statistic is significantly less than 2

# interpretation
# since the p value is less than the significance level of 0.05 we reject the null hypothesis of no autocorrelation
# the durbin watson statistic being close to zero rather than two further supports this conclusion
# we conclude that there is significant evidence of positive autocorrelation in the residuals of the regression model
