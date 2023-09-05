# Ben Turner Assessment 2.2
# R Version - 4.3.1
# OS = Mac OS Ventura 13.1

# lines to clear working memory and check working directory matches the 
# correct file locations.
# Loading of all packages required within this file.
rm(list = ls())
gc()
getwd()
library(tidyverse)
library(e1071)
library(zoo)
library(RColorBrewer)


##### PART 1 File Import and Data Cleaning #####-------------------------------


# File Import -----------------------------------------------------------------
############# ATM_DATA
# Importing of excel file via tidyverse function with filtering applied to 
# only import required data. 
columns <- c("Date", "num_withdrawal", "val_withdrawal")
atm_data <- readxl::read_xlsx(path = "c04-1-hist.xlsx", sheet = "Data", 
                              col_names = columns, 
                              col_types = c("date", "numeric", "numeric"), 
                              range = "A12:C359")
# Simplification and conversion of Date values
atm_data$Date <- as.yearmon(atm_data$Date, format = "%b %y")


############# ATM TERMINALS
# Importing ATM Terminals as a new data frame.
# redefining columns variable as it is no longer needed for atm_data.
columns <- c("Year", "March", "June", "September", "December")
atm_terminals <- readxl::read_xlsx(path = "Device_Statistics.xlsx", 
                              sheet = "ATM Terminals", 
                              col_names = columns, 
                              col_types = c("text", "numeric", "numeric", 
                                            "numeric", "numeric"), 
                              range = "A2:E36")
# Creation of long form data frame and combining year and month to create a date
# column in the correct data type/format
atm_terminals <- atm_terminals %>%
  pivot_longer(col = -Year, names_to = "Month", values_to = "Counts") %>%
  unite(., Date, c(Year, Month))
atm_terminals$Date <- as.yearmon(atm_terminals$Date, "%Y_%B")
atm_terminals <- atm_terminals %>% arrange(Date)
# Filling in NA values using last object carried forward, and fixing the
# "leading NA"
atm_terminals$Counts <- na.locf(atm_terminals$Counts, na.rm = FALSE, 
                                fromLast = FALSE, maxgap = 5)
atm_terminals[1,2] <- 4073 

############### EFTPOS TERMINALS
# Importing EFTPOS Terminals as a new data frame.
# reusing columns variable as they share the same names.
eftpos_terminals <- readxl::read_xlsx(path = "Device_Statistics.xlsx", 
                                   sheet = "EFTPOS Terminals", 
                                   col_names = columns, 
                                   col_types = c("text", "numeric", "numeric", 
                                                 "numeric", "numeric"), 
                                   range = "A2:E36")
# Creation of long form data frame and combining year and month to create a date
# column in the correct data type/format as above
eftpos_terminals <- eftpos_terminals %>%
  pivot_longer(col = -Year, names_to = "Month", values_to = "Counts") %>%
  unite(., Date, c(Year, Month))
eftpos_terminals$Date <- as.yearmon(eftpos_terminals$Date, "%Y_%B")
eftpos_terminals <- eftpos_terminals %>% arrange(Date)
# Filling in NA values using last object carried forward, and fixing the
# "leading NA" as was done for atm-terminals
eftpos_terminals$Counts <- na.locf(eftpos_terminals$Counts, na.rm = FALSE, 
                                fromLast = FALSE, maxgap = 5)
eftpos_terminals[1,2] <- 11452 


# Cleaning and Filtering Data -------------------------------------------------
# Matching data types and dates in preparation to merge all into one data frame.

# This block takes atm_data and stores it into a new data frame in preparation 
# for the merging of the three imported frames. It also removes the first and 
# last rows as they were incomplete quarters.
# This way all three of the data frames are kept as separate data frames for 
# later use (specifically, atm_data is needed in monthly form for the final 
# analysis task)
combined_data <- atm_data
combined_data <- combined_data %>% mutate(Quarter = as.yearqtr(Date)) %>%
  group_by(Quarter) %>%
  summarise(num_wd = sum(num_withdrawal), val_wd = sum(val_withdrawal)) %>%
  filter(Quarter >= "1994 Q3") %>%
  filter(Quarter <= "2023 Q1")

# The following blocks convert the Date columns to Quarter form, rename the
# the columns in preparation for merging, and filter to match the atm_data
# date range
atm_terminals <- atm_terminals %>% mutate(Date = as.yearqtr(Date)) %>%
  rename(Quarter = Date, num_atms = Counts) %>%
  filter(Quarter >= "1994 Q3") %>%
  filter(Quarter <= "2023 Q1")

eftpos_terminals <- eftpos_terminals %>% mutate(Date = as.yearqtr(Date)) %>%
  rename(Quarter = Date, num_eftpos = Counts) %>%
  filter(Quarter >= "1994 Q3") %>%
  filter(Quarter <= "2023 Q1")

# I repeated the filtering steps across all three data frames in order to make 
# the merging process as simple as possible with all of the columns being of the
# length to begin with.


### Creating new columns  and managing units/orders of magnitude ###

# Merging of all data frames into atm_data and calculations for average 
# withdrawal amount and average withdrawal value per ATM terminal.
combined_data <- combined_data %>% full_join(atm_terminals, 
                                             by= c("Quarter" = "Quarter")) %>%
  full_join(eftpos_terminals, by= c("Quarter" = "Quarter")) %>%
  mutate(val_wd = val_wd*1000000,
         num_wd = num_wd*1000,
         mean_wd = val_wd / num_wd,
         mean_atm = val_wd / num_atms)

# Altering the column units to be easier to read and understand.
# number of withdrawals in millions,
# value of withdrawals in $AUD billions, and
# average withdrawal amount per ATM in $AUD millions 
# Steps will be run again for Analysis part 5 involving just the monthly stats
# from the atm_data data frame...
combined_data <- combined_data %>%
  mutate(num_wd = num_wd / 1000000,
         val_wd = val_wd / 1000000000, 
         mean_atm = mean_atm / 1000000)

# The following function is a custom function to save code duplication with all
# of the numerical summary readouts that will be required for the analysis to
# follow
data_summaries <- function(df, col){
  summ <- df %>% summarise(Mean = mean({{col}}), Median = median({{col}}), 
                           StDev = sd({{col}}), Skewness = skewness({{col}}), 
                           Kurtosis = kurtosis({{col}}), IQR = IQR({{col}}),
                           Min = min({{col}}), Max = max({{col}}), 
                           Range = sum(max({{col}}) - min({{col}})))
  return(summ)
}

##### PART 2 Average Withdrawal per ATM #####---------------------------------

# Overall numerical summary generated using function immediately above.
data_summaries(combined_data, mean_atm)
# Grapical summaries: symmetry of data would indicate that a histogram would be
# most suitable but I provided both to ensure my interpretation was not being 
# skewed by the histogram bin size
combined_data %>%
  ggplot(mapping = aes(y = mean_atm)) +
  geom_boxplot(width = 1, col = "purple", fill = "forestgreen", alpha = 0.7) +
  ggtitle("Boxplot of average quarterly withdrawals per ATM
($AUD millions)") +
  xlim(-1, 1) +
  ylab("Amount withdrawn per quarter per ATM") +
  theme(axis.text.x = element_blank())
#Histogram code for same mean withdrawal per ATM
combined_data %>%
  ggplot(aes(x = mean_atm)) +
  geom_histogram(col = "purple", fill = "forestgreen", alpha = 0.7, bins = 24) +
  ggtitle("Histogram of average amount withdrawn per quarter, per ATM
($AUD millions)") +
  geom_vline(xintercept = median(combined_data$mean_atm), lty = 2) +
  geom_vline(xintercept = mean(combined_data$mean_atm), lty = 1) +
  scale_y_continuous(name = "Counts/Frequency", breaks = seq(0,10,2)) +
  xlab("Amount withdrawn per quarter per ATM")

# Creating a new column, a factor to separate entries by decade
combined_data <- combined_data %>%
  mutate(decade = as.factor(case_when(
    between(Quarter, as.yearqtr("1994 Q3"), as.yearqtr("1999 Q4")) ~ "1990's",
    between(Quarter, as.yearqtr("2000 Q1"), as.yearqtr("2009 Q4")) ~ "2000's",
    between(Quarter, as.yearqtr("2010 Q1"), as.yearqtr("2019 Q4")) ~ "2010's",
    between(Quarter, as.yearqtr("2020 Q1"), as.yearqtr("2023 Q1")) ~ "2020's")))

# This block generates the numerical summaries grouped by decade.
combined_data %>%
  select(mean_atm, decade) %>%
  group_by(decade) %>%
  data_summaries(., mean_atm)

# The following 2 blocks generate graphical summaries for the mean ATM 
# withdrawal per quarter per terminal.
# This block creates a colour-coded histogram comparing the decade groupings.
combined_data %>%
  ggplot(aes(y = mean_atm, x = decade, fill = decade)) +
  geom_boxplot() +
  ggtitle("Comparison of average quarterly withdrawals per ATM
($AUD millions) by decade") +
  xlab("Decades") +
  ylab("Amount withdrawn per quarter per ATM")
#Histogram code for same groupings by decade.
combined_data %>%
  ggplot(aes(x = mean_atm, fill = decade)) +
  geom_histogram(alpha = 0.7, bins = 24) +
  ggtitle("Comparison of average quarterly withdrawals per ATM
($AUD millions) by decade") +
  scale_y_continuous(name = "Counts", breaks = seq(0,10,2)) +
  xlab("Amount withdrawn per quarter per ATM")
    





##### PART 3 Number of ATMs vs Number of Withdrawals #####---------------------
# Splitting of the combined_data data frame into before 2010 and after 2010 sets.
# Created separate data frames as to make graphing and analysis simpler without
# having to use group_by functionality for each set or create a new categorical
# column in the combined_data frame. This way the decade factored column is 
# reused.
pre_2010 <- combined_data %>%
  filter(decade == "2000's" | decade == "1990's") %>%
  select(Quarter, num_wd, num_atms)
head(pre_2010)
post_2010 <- combined_data %>%
  filter(decade == "2010's" | decade == "2020's") %>%
  select(Quarter, num_wd, num_atms)
head(post_2010) 

### Before 2010
# Numerical Summaries
# number of ATM terminals numerical summary
data_summaries(pre_2010, num_atms)
# number of withdrawals numerical summary
data_summaries(pre_2010, num_wd)

# Correlation and regression analysis
pre_2010 %>% select(num_atms, num_wd) %>% cor()   # 0.99
fit_pre <- lm(num_wd ~ num_atms, data = pre_2010)
summary(fit_pre)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.556e+01  1.890e+00   24.11   <2e-16 ***
# num_atms    6.417e-03  1.033e-04   62.12   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.084 on 60 degrees of freedom
# Multiple R-squared:  0.9847,	Adjusted R-squared:  0.9844 
# F-statistic:  3859 on 1 and 60 DF,  p-value: < 2.2e-16

# Graph with added linear regression line
pre_2010 %>% ggplot(aes(x = num_atms, y = num_wd)) +
  geom_point(shape = 15, alpha = 0.7, size = 2, colour = "forestgreen") +
  ggtitle("Scatter plot comparing number of ATMs to number of withdrawals 
(in millions) per quarter from Q3-1994 to Q4-2009") +
  xlab("Number of ATMs") +
  scale_y_continuous(name = "Number of withdrawals (millions)", 
                     breaks = seq(75, 250, 25)) +
  geom_smooth(method = "lm", formula = "y ~ x")


### After 2010
# Numerical Summaries
# number of ATM terminals numerical summary
data_summaries(post_2010, num_atms)
# number of withdrawals numerical summary
data_summaries(post_2010, num_wd)

# Correlation and regression analysis with graph 2
post_2010 %>% select(num_atms, num_wd) %>% cor()      # 0.71
cor(log(post_2010$num_atms), log(post_2010$num_wd))   # 0.79
fit_post <- lm(log(num_wd) ~ log(num_atms), data = post_2010)
summary(fit_post)
# Coefficients:
#               Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)   -25.6062     3.3393  -7.668  4.81e-10 ***
# log(num_atms)   2.9762     0.3246   9.168  2.29e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.195 on 51 degrees of freedom
# Multiple R-squared:  0.6224,	Adjusted R-squared:  0.615 
# F-statistic: 84.06 on 1 and 51 DF,  p-value: 2.286e-12
exp(fit_post$coefficients)
# (Intercept)    log(num_atms) 
# 7.574770e-12   19.61302 

# Graph with added regression line based on log-log transform model
# Creating a new data frame here with values based on the model generated by
# the log transform of post-2010 data.
post_2010_lt <- data.frame(num_atms = seq(25000, 33000, 10))
post_2010_lt<- post_2010_lt %>% mutate(num_wd = exp(predict(fit_post, 
                                                            newdata = .)))
# Graphing of model as a line with scatterplot of withdrawal and ATM counts.
post_2010 %>% ggplot(aes(x = num_atms, y = num_wd)) +
  geom_point(shape = 17, alpha = 0.7, size = 2, colour = "purple") +
  ggtitle("Scatter plot comparing number of ATMs to number of withdrawals 
(in millions) per quarter from Q1-2010 to Q4-2023") +
  xlab("Number of ATMs") +
  ylab("Number of withdrawals (millions)") +  
  geom_line(data = post_2010_lt, aes(x = num_atms, y = num_wd), lwd = 1)







##### PART 4 Number of ATMs over time #####------------------------------------
### Historical Graph
# First, create a new variable to make regression calculations simpler in terms
# of syntax. Made a graph to assess the curve shape.
atms_over_time <- combined_data %>% select(Quarter, num_atms)
atms_over_time %>% ggplot(aes(x = Quarter, y = num_atms)) +
  geom_point()
# Based on shape of the curve, calculated the log of each variable before 
# continuing with regression analysis. This allows for comparison of multiple 
# models to see which has the best R^2
atms_over_time <- atms_over_time %>% 
  mutate(log_atms = log(num_atms), 
         log_quarter = log(as.numeric(Quarter)))
# After assessing impact of each variable (with summary and visually inspecting 
# the plot), only need to use log of the number of ATMs, keeps analysis and 
# interpretation of results simple, trade off is a very small decrease in R^2 value.

# Generating models to compare
# log-log transformation
aot_fit <- lm(log_atms ~ log_quarter, data = atms_over_time)
summary(aot_fit) # R-squared:  0.7115
# log transformation
aot_fit <- lm(log_atms ~ Quarter, data = atms_over_time)
summary(aot_fit) # R-squared:  0.7099
# no transformation
#Best R-squared value therefore will be used for the model.
aot_fit <- lm(num_atms ~ Quarter, data = atms_over_time)
summary(aot_fit) # R-squared:  0.7336
# Residuals:
#   Min     1Q Median     3Q    Max 
# -10159  -3901   2344   3737   5768 
# 
# Coefficients:
#               Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) -1.749e+06  1.004e+05  -17.42   <2e-16 ***
# Quarter      8.817e+02  4.998e+01   17.64   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4448 on 113 degrees of freedom
# Multiple R-squared:  0.7336,	Adjusted R-squared:  0.7313 
# F-statistic: 311.2 on 1 and 113 DF,  p-value: < 2.2e-16

# Creating a data frame to fit the model predictions to
aot_lt <- atms_over_time %>% select(Quarter)
aot_lt <- aot_lt %>% mutate(num_atms_trans = predict(aot_fit, newdata = .))
# Scatter plot with with model applied
atms_over_time %>% ggplot(aes(x = Quarter, y = num_atms)) +
  geom_point(alpha = 0.7) +
  geom_smooth(data = aot_lt, aes(x = Quarter, y = num_atms_trans), 
              method = "loess") +
  ggtitle("Scatter plot outlining the change in number of ATMs over time") +
  scale_y_continuous(name = "Number of ATMs", breaks = seq(7000, 35000, 5000)) +
  scale_x_yearqtr(name = "Year-Quarter", format = "%Y-%q", n = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






##### PART 5 Average Withdrawal Aggregated by Month #####-----------------------
### Monthly Aggregation
# Using original atm_data set as none of the ATM or EFTPOS terminal data is 
# required here and it was already segregated by month.
# In this block the mean withdrawal size is calculated and values are rounded 
# again to make for easier interpretation.
atm_data <- atm_data %>% 
  mutate(val_withdrawal = val_withdrawal*1000000,
         num_withdrawal = num_withdrawal*1000,
         mean_withdrawal = val_withdrawal / num_withdrawal)
atm_data <- atm_data %>% mutate(num_withdrawal = num_withdrawal / 1000000,
                                val_withdrawal = val_withdrawal / 1000000000)

# Conversion of Date into a new column of month names as an ordered factor.
atm_data <- atm_data %>% mutate(Month = Date) %>%
  mutate(Month = as.character(Month), 
         Month = as.factor(str_remove(Month, " \\d+"))) %>%
  mutate(Month = factor(.$Month, levels = c("Jan", "Feb","Mar", "Apr", "May", 
                                            "Jun", "Jul", "Aug", "Sep", "Oct", 
                                            "Nov", "Dec"), ordered = TRUE))

### Stats on months, December in particular
# Creating numerical summaries for each month
atm_data %>% select(Month, mean_withdrawal) %>% 
  group_by(Month) %>%
  data_summaries(., mean_withdrawal)

# Creating side-by-side boxplots. I chose to use a set of boxplots here to 
# better showcase the small difference between months that would likely have
# been lost in a histogram.
atm_data %>% ggplot(aes(y = mean_withdrawal, x = Month, fill = Month)) +
  geom_boxplot() +
  ggtitle("Boxplots for the average withdrawal amount ($AUD) organised by month 
from 1994 - 2023") +
  scale_fill_brewer(palette = "Set3") +
  xlab("Month") +
  scale_y_continuous(name = "Average withdrawal sizes by month $AUD",
                     breaks = seq(100, 300, 20)) 

# Combined line graph
atm_data %>% ggplot(aes(y = mean_withdrawal, x = Date, colour = Month)) +
  geom_line(lwd = 2, alpha = 1, lty = 1) +
  scale_colour_brewer(palette = "Set3") +
  theme_classic() +
  ggtitle("Line graphs for the average withdrawal amount ($AUD) organised by month 
from 1994 - 2023") +
  ylab("Average withdrawal sizes by month ($AUD)")
  






























