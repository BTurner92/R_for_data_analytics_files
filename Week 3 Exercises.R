# Exercises for week 3 ---------------------------------------------------------
# Data loading, data visualisation, descriptive statistics


# Please try to complete tasks listed below. Type your code and results 
# interpretations in each section.



# clean up global environment
rm(list = ls())



### Task 1 ---------------------------------------------------------------------

# Check your working directory (and/or adjust your working directory) to ensure
# that data file "week03_data_oxygen.csv" is placed in the working directory.
# Then load the data contained in the "week03_data_oxygen.csv" file.
# Provide comments about the data set.

# Data information: The oxygen distribution gives important information 
# on the general biochemistry of marine life. The data measures the oxygen 
# saturation (mg/L^10) at 22 marine parks in Australia. 
getwd()
x <-readr::read_csv("week03_data_oxygen.csv")
df <- data.frame(x)
df
summary(df)
o_range <- max(df$Oxygen) - min(df$Oxygen)
#The data set is only a single column of 22 entries. The median and mean values
#are very close together, and the range (between highest and lowest values)
#is relatively small at 5.6. This would indicate that the data set is quite 
#symmetrical.


### Task 2 ---------------------------------------------------------------------

# Visualise data distribution by histogram and boxplot
# Take care about a clear and informative titles and axis labels for all 
# your visualisations.
# Provide your comments about the shape of the distribution as a comment.

hist(df$Oxygen, main="Oxygen saturation at 22 marine parks \nacross Australia", 
     xlab = "O2 Saturation (mg/L^10)", 
     col = "cyan", breaks=seq(7,14,0.5))

O2_box <- boxplot(df$Oxygen, main = "Oxygen saturation  
     at 22 marine parks across Australia", xlab = "O2 Saturation (mg/L^10)", 
     col = "darkorange", horizontal=TRUE)

#From looking at both plots, most results seem to be unimodal and clustered 
#clustered around the 8-10 mark with one outlier, the maximum value of 13.3, 
#skewing the results.
#That said, even with the outlier removed using outline=FALSE, the boxplot still 
#leans towards the higher end of the results as can be seen by the fact that the
#median line sits only ~1/4 of the way through the central box.



### Task 3 ---------------------------------------------------------------------

# Calculate descriptive statistics for the data. 
# Get both options for centrality and dispersion measures. 
# Comment what measures are more appropriate for these data, provide 
# an interpretation for these measures.
xbar <- mean(df$Oxygen) #mean = 9.35


med <- median(df$Oxygen) #median = 8.95

stdev <- sd(df$Oxygen) #standard deviation = 1.35

abline(v=xbar, col=("red"), lty=2, lwd=2)
distro_90 <- xbar + c(-1, 1) * stdev
abline(v=distro_90, col=("green"), lty=2)
#Two lines functions here applied to the histogram...

#INTERPRETATION
#The outlier on the higher end of the dissolved O2 values unfortunately skews
#the data set away from being an otherwise good example of normal distribution.
#This is surprising given the small sample size of n=22.
#The fact that the median and mean values are quite close, only having
#difference of 0.4 is evidence of good data distribution and I would argue with 
#a larger sample size (to compensate for outliers), the data would trend even 
#closer towards normal distribution.








