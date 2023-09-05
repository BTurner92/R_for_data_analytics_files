rm(list = ls())
library(dplyr)

# Categorical Data Analysis =-------------------------------------------------

df<- iris
head(df)
summary(df)

# For categorical can only count initially
table(df$Species)
# Special object of table data type
df %>% count(Species)
#From dplyr creates a data frame

df <- df %>% mutate(Length = if_else(Sepal.Length > 6, "Long", "Short"))
head(df)
View(df)
summary(df)
# This way creates the variables as character values, for data analysis, best
# to create these as a factor .... 
df <- df %>% mutate(Length = factor(if_else(Sepal.Length > 6, "Long", "Short")))
View(df)
summary(df)

# Appropriate data vis for categorical data is barplot.
# Using table here applies the counts.
barplot(table(df$Length))

# When piping two categorical variables together, will produce a df containing
# all possible combinations.
df %>% count(Species, Length)

barplot(df %>% count(Species, Length))
# Need to use the formula syntax for generating the barplot.
barplot(n ~ Species + Length, data = df %>% count(Species, Length))
# Creates a nice stacked barplot. VERY USEFUL SYNTAX
# Second argument will be the x axis, this determines how the categories are
# grouped.
barplot(n ~ Length + Species, data = df %>% count(Species, Length))

# Can also do without dplyr
x <- table(df$Length, df$Species)
x
barplot(x, legend.text = rownames(x), args.legend = list(x = "topleft"))
# LEGEND = ROWNAME SYNTAX!!!!!



# GROUP AND AGGREGATE =--------=====-----=------------------------------------
# Common terms across all data analysis languages/packages
head(df)

# Aggregate provides a way to separate a data set BY a categorical variable in 
# relation to a numerical variable here, separate species, calculate mean of SL
aggregate(df$Sepal.Length, by = list (df$Species), FUN = mean)

# using dplyr with group_by and summarise functions, 
# this produces result as a tibble
df %>% group_by(Species) %>% summarise(MeanLength = mean(Sepal.Length))

# Can also be used to group numerical variables if they have limited levels.
# Here calculating mean of mpg and number of observations in each group.
mtcars %>% group_by(am, cyl) %>% summarise(mean_mpg = mean(mpg), n=n())

# Same sort of idea using the iris dataset
df %>%
  group_by(Species, Length) %>%
  summarise(MeanWidth = mean(Sepal.Width), MedianWidth = median(Sepal.Width))


# Now with a larger a data set

df<- dslabs::us_contagious_diseases
head(df)

res <- df %>% group_by(disease, year) %>% #initial grouping
  summarise(rate = mean(count / population * 100000)) %>% # normalising = RATE
  group_by(disease) %>%
  summarise(mean = mean(rate), sd = sd(rate)) #calculating mean and sd of rate
res

# easy way to check for any missing data in a dataframe
sum(is.na(df))
# number is the amount of NAs therefore better to filter out the NAs early
res <- df %>% tidyr::drop_na() %>% #FROM TIDYVERSE
  group_by(disease, year) %>% #initial grouping
  summarise(rate = mean(count / population * 100000)) %>% # normalising = RATE
  group_by(disease) %>%
  summarise(mean = mean(rate), sd = sd(rate)) #calculating mean and sd of rate
# Used dplyr version here, no more NAs in the result. 
res

# More data Manipulations - converting between long and wide tables -----------
rm(list = ls())

library(tidyverse)
df <- relig_income
head(df)
View(df)
# Looks like the df has 11 variables but there are actually only 3
# Religion, level of income and count of people in each category.

# Wide table common for data aggregation and comparative viewing.
# Long table better for data vis and analysis
?pivot_longer
df_long <- df %>%
  pivot_longer(col = -religion, names_to = "Income", values_to = "Counts")

# minus in religion means that religion column will staty as is..
head(df_long)
View(df_long)

# Now to convert back into the wide format..
# This method is far better for presentation, easier to understand
df_wide <- df_long %>%
  pivot_wider(names_from = "Income", values_from = "Counts")
head(df_wide)
View(df_wide)

df <- iris
head(df)
# Similarly to relig, this also has only 3 types of variables that can be 
# converted to via pivot
df_long <- df %>%
  pivot_longer(col = -Species, names_to = "Type_of_measurement", 
               values_to = "Size_cm")
df_long
View(df_long)

# Conversion to long form data frame makes complex, detailed data visualisation
# much easier as all the variables are of the correct type.
# For example
boxplot(Size_cm ~ Species + Type_of_measurement, data = df_long)
# Creates a series of boxplots for each vartiable



# Purr - for working with lists and vectors ----------------------------------
rm(list = ls())
library(tidyverse)

df <- mtcars
head(df)

sapply(df, mean) # applies function to a vector

# same thing using piping and purr/dplyr
?map # transforming a vector/list
df %>% map(mean)
# returns values as a list

# if you want as a vector used map double
df %>% map_dbl(mean)
# OR 
# even map to a dataframe (a tibble to be exact)
df %>% map_df(mean)

# MAP_IF
iris %>% map_if(is.numeric, mean) #is numeric = the true condition, if false
# nothing occurs as can be seen in the Species list...

# Using anonymous functions within purr
seq(1:10) %>% map_dbl(sqrt)
seq(1:10) %>% map_dbl(~ sqrt(.x)) # ~ = function(x), . before argument
# this is the same as...
seq(1:10) %>% map_dbl(function(x) sqrt(x))
# Another example, don't forget the dot! . . .
seq(1:10) %>% map_dbl(~ sqrt(.x) + .x^2 + log(.x))


# Advanced data analysis methods ---------------------
df %>%
  group_split(am) %>%
  map(~ lm(hp ~ mpg, data = .x))
# Avoids using for loops when iterating and repeating multiple complex 
# analysis steps.
# Stores data as a list as many of the complex analysis methods cannot simply 
# be stored as a numerical value in a data frame and require the ability of
# lists to store and aggregate multiple data types into the one object.









