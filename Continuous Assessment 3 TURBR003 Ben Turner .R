################################################################################
# MATH 2032 - Continuous Assessment Test 3
# SOLUTIONS
################################################################################

# This is a graded test worth 15% of your final mark.

# Complete tasks listed below. Type your code in each section.
# Provide your discussion where required as comments.
# Meaningful discussion is equally important as a correct code
# and will be marked accordingly.

# Save this file with your code and submit it through LearnOnline

# For the questions where you have to provide comments, one paragraph 
# means between 3 and 6 rows. Your answers should be brief but complete. 




# Task 1 - (15 points) ---------------------------------------------------------

# This is a repeat of the question 4 (part 1) from Test 2 (text is below). 
# Produce the same calculations and appropriate data visualisation 
# BUT using different tools:
# 1. use apply() family functions from base package, no loops
# 2. use dplyr or purrr package, no loops
# 3. use ggplot2 for data visualisation
# 4. no discussion required on the results

# Hint: Don't forget to load libraries you use!

################################################################################
# Some researchers propose an alternative measure of tailedness. It is
# calculated as a standard deviation divided by mean absolute deviations.
# Please google "how to calculate mean absolute deviations in R".

# 1. Make a code that calculates kurtosis and new measure proposed above for
# each column of the data set below. You will get values of kurtosis and 
# corresponding values of new measure. Plot the graph to study a relationship 
# between these two variables.
################################################################################

mydata <- datasets::volcano

# ---- your code here ---- #
library(tidyverse)
# Apply Method - create vectors of each calculation using apply, merged into a 
# new dataframe.
mydata_kurtosis <- apply(mydata, 2, e1071::kurtosis)
mydata_sdmad <- apply(mydata, 2, function(x) sd(x) / mad(x))
apply_df <- as.data.frame(rbind(mydata_kurtosis, mydata_sdmad))
head(apply_df)

# Dplyr Method - Dplyr allows for easy manipulation of data without needing to
# store it as a variable, hence the lines below this are piped instead.
dplyr_df <- as.data.frame(mydata) 
dplyr_df %>% map_dbl(e1071::kurtosis)
dplyr_df %>% map_dbl(~ sd(.x) / mad(.x))
class(graphing_df) 
# GGPlot
graphing_df <- as.data.frame(t(apply_df))
ggplot(graphing_df, aes(x = seq(1:61), y = mydata_kurtosis)) +
  geom_line(colour = "red") +
  geom_line(aes(x = seq(1:61), y = mydata_sdmad), colour = "blue") +
  ylab("Value") + xlab("Volcano Column #") +
  ggtitle("Comparison of Kurtosis and SD/MAD Data Tailedness Measures")
# As the data points are not classifications, I could not figure out how to 
# get the legend to display correctly...





# Task 2 - (40 points) ---------------------------------------------------------

# Load data set "brca" from the package "dslabs". Check the help file for the
# description. The data are provided as a list, we need it as data frame

temp <- dslabs::brca
?dslabs::brca

df <- cbind(as.data.frame(temp$x), outcome = temp$y) 
View(df)

# We are interested what variables might be the best indicators for the "outcome"
# malignant ("M") or benign ("B"). There are 30 features (variables) and we 
# want to select three variables that have the largest difference between 
# groups M (malignant) and B (benign). 

# 1) Use "dplyr" functionality to do calculations and find three variables with
# largest differences - no loops.
# 2) Create data visualisation to show the difference in distributions for these 
# three variables in two groups (M/B) - use ggplot2 package, plot them all 
# on one graph.
# 3) Briefly discuss the result - can you predict "outcome" based on these
# distributions? (1 or 2 paragraphs)
# 4) Build a predictive model for the type of cancer by comparing selected 
# variables to their means. Then report the quality of your predictions by 
# comparing your prediction and true value. It is OK to make prediction for
# one variable at a time in the long table you created by following hints below.

# Hints: 
# 1. All variables are non-symmetrical and right skewed. So, analysis will benefit
# from log-transformation, use not raw data but a logarithm - log(). 
# 1. All variables have different measurements, to be able to compare
# them you need to "standardise" the data - check function scale()
# 2. To find the difference you can use subtraction or function diff(). Remember
# that difference can be positive or negative - you need four largest 
# differences by absolute values. That is, positive 100 and negative -100 are 
# equally good.
# 3. To plot three selected variables together you might need to transform
# the data set from wide table into long table.
# 4. Use your common sense and data understanding for results reporting and 
# interpretation. E.g., if the graph does not make sense or does not help you
# to see the difference, then you have to change it.

# ---- your code and comments here ---- #

# The following block applies log transform, turns all -inf values to NA and 
# omits them, scales values, groups by tumour type, calculates mean and largest 
# differential between groups before listing all attributes in descending order
# 
df %>% mutate_if(is.numeric, ~ log(.x)) %>%
  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
  na.omit %>%
  mutate_if(is.numeric, ~ (scale(., scale = TRUE) %>% as.vector)) %>%
  group_by(outcome) %>%
  summarise_if(is.numeric, mean) %>%
  summarise_if(is.numeric, diff) %>%
  t(.) %>%
  as.data.frame(.) %>%
  arrange(., desc(V1))
# The three largest differences are:
# perimeter_worst at 1.638
# radius_worst at 1.619 
# and area_worst at 1.617

# 2 - Plotting / Data Visualisation
# Data is filtered by the variables with greatest variance, normalised again due
# to the area values being significantly larger, and converted to long table
# format. Next, data are plotted in boxplot form with a faint jitter plot over 
# the top to better illustrate the distribution of results by outcome.
# Note I did the scaling and long table conversion in this order to keep data
# on a similar scale. Plots were far easier to read that way.
df %>% select(perimeter_worst, radius_worst, area_worst, outcome) %>%
  mutate_if(is.numeric, ~ log(.x)) %>%
  mutate_if(is.numeric, ~ (scale(., scale = TRUE) %>% as.vector)) %>%
  pivot_longer(col = -outcome, names_to = "Attribute", values_to = "Size_cm") %>%
  ggplot(aes(x = Attribute, y = Size_cm)) +
  geom_boxplot() +
  geom_jitter(width = 0.25, alpha = 0.4, aes(colour = outcome)) +
  ggtitle("Boxplot comparing the distribution in the three variables of BRCA data
with highest variance (all sizes normalised)")

# 3 - Discussion
# There is a neat distribution present here (aided by the data normalisation)
# that indicates, in most cases, a point below the median is somewhat likely to be 
# benign and a point above, malignant. The distinction becomes more stark when
# considering point either side of the 1st and 3rd quartiles. If a point is below
# the first quartile, it appears very likely to be benign, where points
# above the 3rd quartile appear very likely to be malignant.

# Around the median of the data sets, it is much more difficult to accurately 
# predict the nature of a tumour as outliers are very hard to predict. 
# Comparison against more variables may be required to improve predictions. 

# 4 - Revised Model
# Despite not using this form of the data frame outside of generating the
# long version within piped operation, I have included it here in case it is 
# necessary for marks...
df_long <- df %>% 
  pivot_longer(col = -outcome, names_to = "Attribute", values_to = "Size_cm")
head(df_long)

# I have read this question over and over and still have absolutely no idea 
# exactly what it is asking, nor do I know how to generate a predictive model,
# something that has not been touched on in ANY of the lectures or tutes.
# I have run out of time to ask for guidance hence, no answer here...

# I hope this is covered in a zoom catch up, I am really curious as to what the
# expected answer is.



# Task 3 - (25 points) ---------------------------------------------------------

# Load data set "ToothGrowth" from the package "datasets". 
# Check the help file for the description. 

df <- datasets::ToothGrowth
?datasets::ToothGrowth

# We are interested in the relationship between tooth length and supplement 
# type/dosage. The variable "dose" - dose in milligrams per day - is a numerical
# variable, however it has a very limited variability.

print(unique(df$dose))

# There are just three values for "dose" - that is, we can treat them as 
# categorical data and then we can use them to make three groups.
# You need to do appropriate data conversion for analysis/plotting.

# 1) Get statistical and graphical summaries for two groups of supplements 
# ("OJ" and "VC"). Provide a discussion what supplement is better for tooth 
# growth. (1 or 2 paragraphs).
# 2) Get statistical and graphical summaries for groups of supplements and dosages.
# Provide a discussion what combinations of supplement and dosages are better 
# for tooth growth (1 or 2 paragraphs).

# ---- your code and comments here ---- #
head(df)
df <- df %>% mutate(dose = as.ordered(dose))
summary(df)

# 1 - Summaries for the data, only factoring in supplement type
df %>% group_by(supp) %>%
  summarise(Mean = mean(len), SD = sd(len), Median = median(len), 
            IQR = IQR(len), Range = sum(max(len) - min(len)), Max = max(len),
            Min = min(len), Skew = e1071::skewness(len), 
            Kurt = e1071::kurtosis(len))
# supp     Mean   SD   Median  IQR  Range   Max   Min    Skew    Kurt
# 1 OJ     20.7  6.61   22.7  10.2  22.7  30.9   8.2   -0.523  -1.03
# 2 VC     17.0  8.27   16.5  11.9  29.7  33.9   4.2   0.276  -0.927
df %>% select(len, supp) %>%
  ggplot(aes(x = len)) +
  geom_histogram(bins = 10, fill = "red", colour = "black") +
  scale_y_continuous(name = "Count", breaks = seq(0,10,1)) +
  facet_grid(supp ~ .) +
  scale_x_continuous(name = "Tooth Length", breaks = seq(0,36,2)) +
  ggtitle("Split histograms comparing tooth length across supplement type")

# Discussion
# Based on the numerical and graphical summaries it would appear that Orange
# Juice provides a more consistent results in terms of improving tooth length.
# The mean and median for OJ are both slightly higher than VC (20.7 to 17 and
# 22.7 to 16.5 respectively) but the VC group has a larger range (29.7 to 22.7)
# and thus smaller min (4.2 to 8.2) and greater max values (33.9 to 30.9).
# The graph histogram clearly indicates that the OJ group is slightly 
# left-skewed and the VC group slightly right skewed, as is confirmed by the 
# skewness values. In spite of the similar kurtosis values, the VC group does 
# appear to have heavier tails than the OJ group.

# Overall, the results suggest that the OJ supplement produces more consistent
# tooth growth across a guinea pig population, but VC can lead to greater 
# outliers within a population.

# 2 - Summaries for supplement and dosages
df %>% group_by(supp, dose) %>%
  summarise(Mean = mean(len), SD = sd(len), Median = median(len), 
            IQR = IQR(len), Range = sum(max(len) - min(len)), Max = max(len),
            Min = min(len), Skew = e1071::skewness(len), 
            Kurt = e1071::kurtosis(len))
# supp   dose   Mean    SD Median   IQR  Range   Max   Min   Skew    Kurt
# 1 OJ    0.5   13.2   4.46  12.2   6.48  13.3  21.5   8.2  0.438  -1.37  
# 2 OJ    1     22.7   3.91  23.5   5.35  12.8  27.3  14.5 -0.680  -0.680 
# 3 OJ    2     26.1   2.66  26.0   2.50   8.5  30.9  22.4  0.369  -1.09  
# 4 VC    0.5   7.98   2.75  7.15   4.95   7.3  11.5   4.2  0.133  -1.81  
# 5 VC    1     16.8   2.52  16.5   2.03   8.9  22.5  13.6  0.926   0.0762
# 6 VC    2     26.1   4.80  26.0   5.43  15.4  33.9  18.5  0.161  -1.23  
df %>% ggplot(aes(x = len, fill = dose)) +
  geom_histogram(bins = 10, colour = "black", alpha = 0.5) +
  scale_y_continuous(name = "Count", breaks = seq(0,10,1)) +
  facet_grid(supp ~ .) +
  scale_x_continuous(name = "Tooth Length", breaks = seq(0,36,2)) +
  ggtitle("Split Histograms Comparing Tooth Length 
across Supplement Type by Dosage")

# Discussion

# By comparing the supplement types by dosage it is clear to see that a higher 
# dose of either supplement benefits tooth growth with the 2mg/day group for
# both supplement groups taking up the majority of the upper quartiles on the 
# histograms and containing the maximum values and highest mean and medians
# when compared to the lower dosages. 

# The VC supplement at 2mg/day produces a more dispersed result than the OJ 
# supplement. Despite both sharing identical mean and median values (26.1 and 
# 26.0 respectively), the VC group has larger values for the numerical summary 
# values indicative of dispersion/centrality. This includes standard deviation, 
# interquartile range, and range, as well as being clearly visible in the 
# yellow sections of the histogram.









# Task 4 - (20 points) ---------------------------------------------------------

# Load data set "trump_tweets" from the package "dslabs". 
# Check the help file for the description. 

df <- dslabs::trump_tweets
?dslabs::trump_tweets

# Former US president Donald Trump is remembered as a prolific tweeter.
# The data set includes all tweets from Donald Trump's twitter account 
# from 2009 to 2017. We want to analyse how "productive" was Donald Trump.
# You have to find out:
# 1) Study the distribution for the number of tweets per week. 
# Provide a brief summary of your findings (1 or 2 paragraphs).
# 2) Make a historical plot of weekly number of tweets created by Donald Trump over
# eight years? Provide brief comments on his "performance" (1 or 2 paragraphs).
# 3) Make a graph showing a relationship between the number of tweets and the
# number of re-tweets per week. Provide brief comments on a possible relationship
# (1 or 2 paragraphs).

# Hint: There is a variable "created_at" that you can use to group data in weeks.
# Package "lubridate" has a set of functions to deal with date/time related
# variables. E.g. day(), week(), month(), year(), etc. You can find them useful. 
# However you are free to use any other functions or packages.
# Beware: there are several years of data, so week number 1 might appear 
# in the data several times.


# ---- your code and comments here ---- #
library(lubridate)
# 1 - Study tweets per week
# Creating new columns for year and week and sorting data in chronological order
df <- df %>% mutate(year = year(created_at), week = isoweek(created_at)) %>%
  arrange(created_at)

# creating weekly count data frame
weekly_count <- df %>% select(created_at, year, week) %>%
  group_by(year, week) %>%
  summarise(Weekly_Count = n())
weekly_count <- as.data.frame(weekly_count)
weekly_count

# Generating summaries
weekly_count %>%
  summarise(Mean = mean(Weekly_Count), Median = median(Weekly_Count), 
            SD = sd(Weekly_Count), IQR = IQR(Weekly_Count), 
            Min = min(Weekly_Count), Max = max(Weekly_Count), 
            Range = sum(max(Weekly_Count) - min(Weekly_Count)), 
            Skew = e1071::skewness(Weekly_Count), 
            Kurt = e1071::kurtosis(Weekly_Count))
#      Mean Median       SD   IQR  Min  Max  Range     Skew     Kurt
# 1 47.0771     42  40.60098  47    1  263   262   1.539693 3.955141

# Discussion
# Judging by the numerical summary generated here Trump was a prolific tweeter
# But that was not always the case. The most obvious statistic illustrating this
# is the range. A maximum of 263 weekly tweets with a minimum of only 1. The 
# mean and median values of 47 and 42 respectively alongside this large range 
# value of 262 indicate that there are some definite peaks and troughs in Trump's
# twitter activity. 
# Standard deviation and IQR values of 40 and 47 would suggest
# a fairly centre-heavy data set, skewed slightly to the right as is confirmed
# by the skewness value of 1.54 (within the values expected for normal 
# distribution). Finally, the kurtosis value of 3.9 is slightly on the 
# leptokurtic side, which makes sense considering the range and skewness values 
# discussed earlier.


# 2 - Historical plot and performance analysis
weekly_count %>% mutate(cumul_week = as.numeric(rownames(.))) %>%
  arrange(year) %>%
  ggplot(aes(x = cumul_week, y = Weekly_Count)) +
  geom_line(group = 1) +
  scale_y_continuous(name = "Weekly Number of Tweets", breaks = seq(0,270,20)) +
  scale_x_continuous(name = "Week", breaks = seq(0,450,50)) +
  ggtitle("Historical Plot of Weekly Number of Tweets by Donald Trump:
May 2009 - Jan 2018")
# Not sure how to keep the week values as Dates, hence the week labels are 
# numbers not dates

# Performance Analysis
# For the past 6-7 years, Trump has maintained a very high weekly tweet frequency.
# He was relatively quiet on the platform until a rise in tweet number around 
# 2011, leading to an even larger spike in 2012-2013, peaking at a level of over
# 260 tweets per week! From 2014-2018, his election campaign and presidential 
# years he had settled somewhat into a consistent 40-100 tweets per week, with 
# the odd outlier week likely due to some manner of political controversy.

# 3 Tweets vs. Retweets
# Separation and tabulation of weekly retweets
weekly_tweet <- df %>% select(created_at, year, week, retweet_count) %>%
  group_by(year, week) %>%
  summarise(Weekly_Count = n(), Weekly_RT = sum(retweet_count))
weekly_tweet <- as.data.frame(weekly_tweet)

# Calculation of retweet:tweet ratio, saved as new column
weekly_tweet <- weekly_tweet %>% mutate(rt_ratio = Weekly_RT / Weekly_Count)


# Graphing RT:T ratio
weekly_tweet %>% mutate(cumul_week = as.numeric(rownames(.))) %>%
  arrange(year) %>%
  ggplot(aes(x = cumul_week, y = rt_ratio)) +
  geom_line(group = 1) +
  scale_y_continuous(name = "Weekly Number of Retweets per Tweet", 
                     breaks = seq(0,40000,5000)) +
  scale_x_continuous(name = "Week", breaks = seq(0,450,50)) +
  ggtitle("Historical Plot of Weekly Number of Donald Trump's Retweets per Tweet:
May 2009 - Jan 2018")

# Comments on relationship
# When compared to the number of tweets, there is a massive jump up in the 
# number of retweets starting around week 300 (early 2015). This correlates with
# the beginning of Donald Trumps presidency where his twitter activity became 
# mainstream news. This explains the massive jump in the ratio of retweets 
# per tweet. 

# At the time where this large jump in numbers occurred (as seen in the previous 
# plot), this was not at Trump's peak in tweet frequency. His highest tweets per 
# week occured around 2012, which hardly registers as a blip on this graph due 
# to the immense popularity seen by his activities on the platform once he was
# elected President of the USA.

# THE END - DON'T FORGET TO SAVE YOUR R-SCRIPT ---------------------------------



