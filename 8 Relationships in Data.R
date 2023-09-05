# Covers Predictive Analytics
# Correlation, Regression Analysis etc. to predict future behaviour

# STEPS ---------
# 1 Produce a scatterplot of the variables - Check linearity
# 2 Compute the correlation (correlation coefficient) between variables
# If linear relationship apparent complete 3 and 4....
# 3 Produce a regression model of the data and checking the goodness of fit:
#     coefficient of determination
# 4 Predict future behaviour.

# Essentially, distilled down to analysis of cause and effect

# Correlation Analysis - the consideration of whether there is an association 
# between 2 variables.
# Correlation coefficient - the index which defines the strength or association 
# between 2 numerical variables.

# ---- Scatterplot setup:
# Dependent variable = Y --- Want to predict
# Independent variable = X --- Use for prediction

# Correlation coefficient = normalised, always between -1 and 1
# positive correlation, large of x = large of y
# negative correlation, large of x = small of y
# perfect correlation =1 (+ or -) whether positive or negative correlation
# = random, no association, no predictive analysis

# r Value interpretation
# Very Strong = 0.8 - 1
# Strong = 0.5 - 0.79
# Moderate = 0.3 - 0.49
# Weak/None = 0 to 0.29

# Things to remember - Don't always look at the number!
# Correlation does not always mean causation (chocolate consumption to nobel 
# laureates example). Here, strong r but no relationship between variables.
# Relationships can be strong but not necessarily linear (fuel consumption 
# example), weak r value but neat curve and data distribution, non-linear rel.

# May also be increasing or decreasing, non-linear (eg sigmoidal)
# OR curvilinear relationships (parabolic)



#### REGRESSION ANALYSIS ----------------------------------------------------
# Least squares regression is the method usedto calculate line of best faithful
# looks at distance between each point and the line, chooses the best line 
# (with smallest cumulative distance from points (squared) to line)

# ŷ = a + bx
# predicted y = intercept (calculated by averages) + 
#                                               gradient (how steep is change)

# Multiple R-squared = R2 value as seen in excel graphs, how closely do the data
# fit the model.

# Correlations must be worded by one average if using slope to determine them.
# Intercept can sometimes be used to determine minimum or starting point, again
# on average

summary(data)$r.squared # will provide R2 value
# R2 value can be used to explain the relationship between variables,
# eg. Distance explains 74.56% of the variation in ticket price = good fit.


# Extrapolation and Interpolation -------------
# Inter = inside the range of observed values that were used to determine model
# Extra = using values from outside data that was used to determine model
?predict()
# use function predict to determine results based on a generated model.

######## APPLYING KNOWLEDGE TO DATA
df <- mtcars

# categorical vs. categorical
table(df$am, df$cyl)

df %>% count(cyl, am) %>%
  pivot_wider(id_cols = am, names_from = cyl, values_from = n)

df %>% ggplot(aes(x = factor(cyl), fill = factor(am))) +
  geom_bar() +
  ggtitle("Count of cars for transmissions / number of cylinders") +
  xlab("Numberr of cylinders") +
  scale_fill_discrete(name = "Transmission", labels = c("Manual", "Auto"))
# note the nice and clean labelling and where the syntax fits/is used
# Factor used to best prepare the data on the fly, makes it far easier to 
# understand and run further analysis later.

df %>% group_by(cyl) %>%
  summarise(n = n(), mean = mean(mpg), sd = sd(mpg), 
            min = min(mpg), max = max(mpg))
# counting how many within in samples and provides statistics for the mpg 
# with respect to their number of cylinders.

# numerical vs. numerical
df %>% ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  ggtitle("fuel consumption versus horsepower") +
  xlab("Horsepower") +
  ylab("Miles per gallon")

df %>% select(mpg, hp) %>% cor()
#         mpg         hp
# mpg  1.0000000 -0.7761684
# hp  -0.7761684  1.0000000

# Good relationship but not exactly straight line therefore not linear
#in these situaitons best to use kendall or spearman

cor(df %>% select(mpg, hp), method = "kendall")
#         mpg         hp
# mpg  1.0000000 -0.7428125
# hp  -0.7428125  1.0000000

cor(df %>% select(mpg, hp), method = "spearman")
#         mpg         hp
# mpg  1.0000000 -0.8946646
# hp  -0.8946646  1.0000000

# Run graph again, this time, add linear regression line
df %>% ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  ggtitle("fuel consumption versus horsepower") +
  xlab("Horsepower") +
  ylab("Miles per gallon") +
  geom_smooth(method = "lm", formula = "y ~ x")

fit <- lm(mpg ~ hp, data = df)
# dependent (y) then independent (x)variable
summary(fit)
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.7121 -2.1122 -0.8854  1.5819  8.2360 
# 
# Coefficients:
#             Estimate   Std. Err  t value  Pr(>|t|)    
# (Intercept) 30.09886    1.63392  18.421   < 2e-16 ***
# hp          -0.06823    0.01012  -6.742   1.79e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.863 on 30 degrees of freedom
# Multiple R-squared:  0.6024,	Adjusted R-squared:  0.5892 
# F-statistic: 45.46 on 1 and 30 DF,  p-value: 1.788e-07

# R2 value is 0.6024 ∴ strong relationship
# On average, for every increase of 1 hp, mpg decreases by 0.068
# Can also say, if horsepower increases by 100, there is a decreases of 6.8mpg
#  1 of hp         = -0.06823 of dependent variable

plot(fit)
#provides a series of plots that illustrate in this case that our linear model
# is not a great fit, not a true linear relationship.

predict(fit, newdata = data.frame(hp = c(0, 200, 500)))
#         1         2         3 
# 30.098861 16.453205 -4.015278 
# First and third values evidently don't make any sense, middle does, it is 
# interpolation, 500hp does not as it is an extrapolation
# in reality, curve is likely a sqrt/asymptotic line (log scale)

#### Plotting log transformed data
df %>% ggplot(aes(x = log(hp), y = log(mpg))) +
  geom_point() +
  ggtitle("fuel consumption versus horsepower") +
  xlab("Log of Horsepower") +
  ylab("Log of Miles per gallon") +
  geom_smooth(method = "lm", formula = "y ~ x")

# significantly better fit, check with cor
df %>% select(mpg, hp) %>% mutate(mpg = log(mpg), hp = log(hp)) %>% cor()
#           mpg         hp
# mpg  1.0000000 -0.8460043
# hp  -0.8460043  1.0000000

fit <- lm(log(mpg) ~ log(hp), data = df) 
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.38189 -0.05707 -0.00691  0.10815  0.37501 
# 
# Coefficients:
#             Estimate  Std. Err   t value Pr(>|t|)    
# (Intercept)  5.54538    0.29913  18.538  < 2e-16 ***
# log(hp)     -0.53009    0.06099  -8.691  1.08e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1614 on 30 degrees of freedom
# Multiple R-squared:  0.7157,	Adjusted R-squared:  0.7062 
# F-statistic: 75.53 on 1 and 30 DF,  p-value: 1.08e-09

# Better R2 value but now have the constants based around log of values.
# can revert this using exp()

exp(fit$coefficients)
plot(fit)
exp(predict(fit, newdata = data.frame(hp = c(0, 200, 500))))
#Values look significantly more realistic here once log transform is reverted.

##### Plotting this model with the original scatter plot ----------------
df2 <- data.frame(hp = seq(50,500))
df2<- df2 %>% mutate(mpg = exp(predict(fit, newdata = .)))
# using a sequence of points in a new df, calculating the associated mpg values
# using the log transform model, creates new column in df2

df %>% ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  ggtitle("Fuel consumption versus horsepower") +
  xlab("Horsepower") +
  ylab("Miles per gallon") +
  geom_line(data = df2, aes(x = hp, y = mpg, colour = "red"))


