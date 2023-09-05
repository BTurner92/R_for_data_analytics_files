# Ben Turner Assessment 2.1
# R Version - 4.3.1
# OS = Mac OS Ventura 13.1
# This code should work correctly with the default c04-1-hist.xlsx file,
# I made no (intentional) edits throughout generating and testing this code.
# I left the lines immediately below this at the top to make it easier to run 
# straight away.
# The only packages I used were within the tidyverse family and e1071, 
# all other functions were from base version of R4.3.1

# lines to clear working memory and check working directory matches the 
# file locations.
rm(list = ls())
gc()
getwd()
library(tidyverse)


# File Import -----------------------------------------------------------------
# Importing of excel file via tidyverse function with filtering applied to 
# only import required data. 
columns <- c("Date", "num_withdrawal", "val_withdrawal")
atm_data <- readxl::read_xlsx(path = "c04-1-hist.xlsx", sheet = "Data", 
                              col_names = columns, 
                              col_types = c("date", "numeric", "numeric"), 
                              range = "A12:C359")

# converting the tibble created by read_xlsx function into a data frame for 
# compatibility with other tidyverse functions.
atm_data <- as.data.frame(atm_data) 
head(atm_data)



# Preparation of Data for Analysis ------------ -------------------------------
# This block creates a new column for calculation of average withdrawal amount,
# and alters units for easier interpretation of results. 
atm_data <- mutate(atm_data, avg_withdrawal = 
                  (val_withdrawal*1000000) / (num_withdrawal*1000),
                  val_withdrawal = val_withdrawal/1000,
                  num_withdrawal = num_withdrawal/1000) 
# Rounding of the average withdrawal amount to dollars and cents value. 
# And reformatting dates into "Date" format
atm_data[,4] <- sapply(atm_data[,4], round, 2)
atm_data$Date <- as.Date(atm_data$Date, format = "%b %y")
 

# Graphical and Numerical Summaries -------------------------------------------
# This function calculates a series of statistics for each of the columns from 
# data set, mean, median, standard deviation etc. Written as a function to
# reduce code duplication.
data_summaries <- function(df, col){
  summ <- df %>% summarise(Mean = mean({{col}}), Median = median({{col}}), 
                           StDev = sd({{col}}), Skewness = e1071::skewness({{col}}), 
                           Kurtosis = e1071::kurtosis({{col}}), IQR = IQR({{col}}),
                           Min = min({{col}}), Max = max({{col}}), 
                           Range = sum(max({{col}}) - min({{col}})))
  return(summ)
}

# Custom function to check column for any outliers. Written as a function to
# reduce code duplication.
outlier_check <- function(df, col, x){
  q1_q3 <- quantile(x, probs = c(0.25, 0.75))
  low_ol <- sum(q1_q3[1]-(1.5*IQR(x)))
  high_ol <- sum(q1_q3[2]+(1.5*IQR(x)))
  outliers <- filter({{df}}, {{col}} < low_ol | {{col}} > high_ol)
  return(outliers)
}

# Custom function to generate histograms for the required variable, also 
# includes lines for median and mean values.
histogram_with_lines <-function(x, title, axis_label){
  hist(x, main = title, xlab = axis_label)
  abline(v = mean(x), col = "red", lwd = 2)
  abline(v = median(x), col = "blue", lwd = 2)
}

# Summaries for number of withdrawals:
num_summary <- data_summaries(atm_data, num_withdrawal)
outlier_check(atm_data, num_withdrawal, atm_data$num_withdrawal) #no outliers
num_summary
histogram_with_lines(atm_data$num_withdrawal, 
                     "Histogram of Number of Monthly ATM Withdrawals in Australia",
                     "ATM Withdrawals (millions)")
# Summaries for value of withdrawals:
val_summary <- data_summaries(atm_data, val_withdrawal)
outlier_check(atm_data, val_withdrawal, atm_data$val_withdrawal) #no outliers
val_summary
histogram_with_lines(atm_data$val_withdrawal, 
                     "Histogram of Value of Monthly ATM Withdrawals 
in Australia (billions of $AUD)",
                     "Value Withdrawn ($AUD billions)")
# Summaries for average withdrawal amount:
avg_summary <- data_summaries(atm_data, avg_withdrawal)
outlier_check(atm_data, avg_withdrawal, atm_data$avg_withdrawal) #31 outliers
avg_summary
histogram_with_lines(atm_data$avg_withdrawal, 
                     "Histogram of Average ATM Withdrawal Amount
per Month in Australia ($AUD)",
                     "Average Withdrawal Amount ($)")
# As the only variable with outliers was Average Withdrawal, it is the only 
# variable to have a boxplot generated. I chose to produce a boxplot as it more 
# clearly highlights the outliers within this category.
boxplot(atm_data$avg_withdrawal, 
        main = "Boxplot of Average ATM Withdrawal/Month 
        in Australia ($AUD)",
        ylab = "Average Withdrawal Amount ($)")
abline(h=mean(atm_data$avg_withdrawal), col = "red", lwd = 2)

# Merging all summaries together to create a clear table for export to the
# accompanying report.
stat_table <- rbind(num_summary, val_summary, avg_summary)
row.names(stat_table) <- c("Number of Withdrawals", "Value of Withdrawals",
                           "Average Withdrawal Amount")
stat_table


# Historical Graphs of Each Variable ------------------------------------------

# Historical graphs were done for each variable as separate code blocks. 
# This made the most sense as opposed to writing a functions due to the number 
# of variations in format required for each.

# Number of Withdrawals Graph
ggplot(data = atm_data, aes(Date, num_withdrawal, group = 1)) +geom_line() +
  scale_x_date(date_breaks = "1 years", date_labels = "%b-%Y") + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  scale_y_continuous(name = "Number of Withdrawals (millions)", 
                   breaks = seq(20,80,10)) +
  ggtitle("Historical Plot of Number of ATM Withdrawals (millions) 
over Time in Australia ")
     
# Value of Withdrawals Graph
ggplot(data = atm_data, aes(Date, val_withdrawal, group = 1)) +
  geom_line(col = "maroon") +
  scale_x_date(date_breaks = "1 years", date_labels = "%b-%Y") + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  scale_y_continuous(name = "Value of Withdrawals (billions $AUD)", 
                     breaks = seq(2,16,2)) +
  ggtitle("Historical Plot of Value of ATM Withdrawals (billions $AUD) 
over Time in Australia ")

# Average Withdrawal Graph
ggplot(data = atm_data, aes(Date, avg_withdrawal, group = 1)) +
  geom_line(col = "seagreen") +
  scale_x_date(date_breaks = "1 years", date_labels = "%b-%Y") + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  scale_y_continuous(name = "Average Withdrawal Amount ($AUD)", 
                     breaks = seq(100,300,25)) +
  ggtitle("Historical Plot of Average ATM Withdrawal Amount ($AUD) 
over Time in Australia")


# Scatterplot of the Two Variables --------------------------------------------

# Scatterplot compare the number and value of withdrawal variables on a scatter
# plot with a linear regression line added to assess any trends.
ggplot(data = atm_data, aes(num_withdrawal, val_withdrawal, group = 1)) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Number of Withdrawals (millions)", 
                     breaks = seq(20,80,10)) +
  geom_smooth(method=lm) +
  scale_y_continuous(name = "Value of Withdrawals (billions $AUD)", 
                     breaks = seq(2,16,2)) +
  ggtitle("Scatter Plot of Australian ATM Withdrawal Data:
Number of Withdrawals (millions) vs. Value of Withdrawals (billions $AUD)")

# Scatterplot with data split into two groups via filtering. 
# One set for Prior to 2020 depicted as blue crosses,
# and the other for 2020 and beyond depicted as seagreen triangles.
ggplot() +
  geom_point(data = atm_data %>% filter(year(Date) <= 2019), 
             aes(num_withdrawal, val_withdrawal), shape = 3, col = "blue") +
  geom_point(data = atm_data %>% filter(year(Date) >= 2020), 
             aes(num_withdrawal, val_withdrawal), shape = 2, col = "seagreen") +
  scale_x_continuous(name = "Number of Withdrawals (millions)", 
                     breaks = seq(20,80,10)) +
  scale_y_continuous(name = "Value of Withdrawals (billions $AUD)", 
                     breaks = seq(2,16,2)) +
  ggtitle("Scatter Plot of Australian ATM Withdrawal Data:
Number of Withdrawals (millions) vs. Value of Withdrawals (billions $AUD)")














