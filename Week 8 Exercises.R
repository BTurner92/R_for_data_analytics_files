# Exercises for week 8 ---------------------------------------------------------
# Relationships in Data. Correlation and Linear Regression


# Please try to complete tasks listed below. Type your code in each section.


# clean up global environment
rm(list = ls())

library(tidyverse)

### Task 1 ---------------------------------------------------------------------

# Load data set "divorce_margarine" from the package "dslabs". Check description
# in the help file. Run correlation analysis. Provide your interpretation for
# the results. 

# Note: correct interpretations are more important than the code!







### Task 2 ---------------------------------------------------------------------

# Load data set "murders" from the package "dslabs". Check description
# in the help file. Run correlation analysis and regression analysis. 
# Provide your interpretation for the results.

# Make a prediction about expected number of gun murders in a state with
# population 10,000,000

df1 <- dslabs::murders
?murders
ggplot(df1, aes(x = population, y = total)) + geom_point() #appears linear
cor(df1$population, df1$total) # 0.963
model <- lm(total~population, df1) #NOTE: order is imortant!
summary(model)
# lm(formula = total ~ population, data = df1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -112.889  -25.656   -3.687   25.505  217.780 
# 
# Coefficients:
#               Estimate   Std. Er    t value   Pr(>|t|)    
# (Intercept) -1.713e+01  1.198e+01   -1.43    0.159    
# population   3.316e-05  1.315e-06   25.23   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 63.77 on 49 degrees of freedom
# Multiple R-squared:  0.9285,	Adjusted R-squared:  0.9271 
# F-statistic: 636.5 on 1 and 49 DF,  p-value: < 2.2e-16
#########===========###########
# Intercept indicates when pop = 0, there are -17 murders...
# pop estimate = increase in murder numbers per increase of 1 in population.
# best to round up ∴ 3.3 murders per 10,000 population

# PREDICT
#using predict function
predict(model, new= data.frame(population = 10000000))
# 314.5174 predicted average murders in a state with a pop of 10 million.






### Task 3 ---------------------------------------------------------------------

# Load data set "gapminder" from the package "dslabs". Check description
# in the help file. Study the relationship between "life_expectancy" (target)
# and "infant_mortality" (predictor). Run correlation analysis and regression 
# analysis. Provide your interpretation for the results.

# Make a prediction for life expectancy in the country with infant mortality 
# rate 3.1 death per 1000 live birth (Australia in 2019)

df2 <- dslabs::gapminder
?gapminder
# target = y = dependent
# predictor = x = independent

ggplot(df2, aes(x = infant_mortality, y = life_expectancy)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

cor(df2$infant_mortality, df2$life_expectancy, use = "complete.obs") # -0.9185
?cor # cannot compute NA values, returns NA therefore requires "use" argument

fit <- lm(life_expectancy~infant_mortality, df2)
summary(fit)
# lm(formula = life_expectancy ~ infant_mortality, data = df2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -35.946  -2.176   0.359   2.717  17.838 
# 
# Coefficients:
#                   Estimate     Std. Err   t value   Pr(>|t|)    
# (Intercept)      76.1542888  0.0673162  1131.3   <2e-16 ***
# infant_mortality -0.2041411  0.0009215  -221.5   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.193 on 9090 degrees of freedom
# (1453 observations deleted due to missingness)
# Multiple R-squared:  0.8437,	Adjusted R-squared:  0.8437 
# F-statistic: 4.908e+04 on 1 and 9090 DF,  p-value: < 2.2e-16

# When you increase infant mortality by 1 per 1000 live births, life expectancy 
# decreases by 0.2 years
# When infant mortality is 0, life expectancy = 76.15 years
# Good R^2 value of 0.843, ∴ model is good

# PREDICTION - Interpolation
predict(fit, new = data.frame(infant_mortality = 3.1))
# life expectancy on average = 75.52



### Task 4 ---------------------------------------------------------------------

# Work the same data as in Task 3 but consider not the raw data but logarithm
# of "life_expectancy" as a target and original value of "infant_mortality" 
# as a predictor. Run correlation and regression analysis. No interpretation 
# is required for this task. Compare the log-based model with the model in Task 3.

# Make a prediction for life expectancy in the country with infant mortality 
# rate 3.1 death per 1000 live birth (Australia in 2019)

ggplot(df2, aes(x = infant_mortality, y = log(life_expectancy))) + 
  geom_point(alpha = 0.3) 
# line has become flatter but outliers are more pronounced
cor(df2$infant_mortality, log(df2$life_expectancy), use = "complete.obs")
# [1] -0.9155713
log_fit <- lm(log(life_expectancy) ~ infant_mortality, df2)
summary(log_fit)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.31287 -0.02956  0.00675  0.04248  0.28615 
# 
# Coefficients:
#                   Estimate   Std. Error  t value   Pr(>|t|)    
# (Intercept)       4.347e+00  1.155e-03  3763.8   <2e-16 ***
# infant_mortality -3.432e-03  1.581e-05  -217.1   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07195 on 9090 degrees of freedom
# (1453 observations deleted due to missingness)
# Multiple R-squared:  0.8383,	Adjusted R-squared:  0.8383 
# F-statistic: 4.712e+04 on 1 and 9090 DF,  p-value: < 2.2e-16

## for every increase of 1 in infant mortality per 1000 live births, log of 
# life expectancy decreases by 0.0034
# R2 value has decreased ever so slightly to 0.83, does not improve model, 
# this is likely due to the severity of the outlier values

prediction <- predict(log_fit, new = data.frame(infant_mortality = 3.1))
exp(prediction) # need to return to values to regular scale (remove log transform)
# 76.43647 
# Very little difference between the two ∴ easier to use the first model, simpler
# and does not require log transform.








