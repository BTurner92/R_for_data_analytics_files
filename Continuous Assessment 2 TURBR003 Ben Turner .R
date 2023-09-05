################################################################################
# MATH 2032 - Continuous Assessment Test 2
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



# Task 1 - (20 points) ---------------------------------------------------------

# One of the commonly used rules for outlier detection says that a data point 
# is an outlier if it is more than 1.5 IQR above the third quartile or below 
# the first quartile. 
# If you want to read more just google for "1.5 IQR rule for outliers"

# Create a custom function that takes numerical vector and returns an appropriate
# measures for centrality and dispersion, and data visualisation. 
# Your function should do the following:
# 1) If skewness is greater than 2 or less than -2, or if there are any 
# outliers, then your function returns median and IQR, otherwise - mean and 
# standard deviation. Please include names for each measure in your function output.
# 2) Plot a histogram for the data with added vertical lines for mean and median.

# Test your function on some numerical columns from the data set "mtcars"
df <- datasets::mtcars

# ---- your code here ---- #
correct_stats_with_hist <-function(x, title){
  x <- na.omit(x)
  hist(x, main = title)
  abline(v = mean(x), col = "red", lwd = 2)
  abline(v = median(x), col = "blue", lwd = 2)
  data_summary <- NULL
  skewness_check <- abs(sum((x-mean(x))^3) / length(x) / sd(x)^3)
  q1_q3 <- quantile(x, probs = c(0.25, 0.75))
  low_ol <- sum(q1_q3[1]-(1.5*IQR(x)))
  high_ol <- sum(q1_q3[2]+(1.5*IQR(x)))
  outliers <- NA
  if(any(x<low_ol) == TRUE || any(x>high_ol) == TRUE){
    outliers <- TRUE
  } else{
    outliers <- FALSE
  }
  if(skewness_check >= 2 || outliers == TRUE){
    data_summary <- data.frame(Median = median(x), IQR = IQR(x))
  } else{
    data_summary <- data.frame(Mean = mean(x), StDev = sd(x))
  }
  return(data_summary)
}
correct_stats_with_hist(df$hp, "Graph of Horsepower") #calls median/IQR
correct_stats_with_hist(df$mpg, "Histogram tallying MpG") #calls median/IQR
correct_stats_with_hist(df$disp, "Histogram tallying displacement") #calls 
                                                                    #mean/sd


# Task 2 - (25 points) ---------------------------------------------------------
rm(list = ls())
# Look at the data set created below.

temp <- datasets::islands
df <- data.frame(island = names(temp), area = temp, row.names=NULL)

# 1) Provide appropriate graphical and numerical summaries for the "area".
# 2) The data look to be leptokurtic. Create a code that removes some rows from
# the data frame to push kurtosis down and make data mesokurtic.
# 3) Provide appropriate graphical and numerical summaries for the "area"
# in the updated data set. 
# 4) Comment on the differences between old and new data set. Think about the
# number of observations removed, change to centrality and dispersion measures,
# change to some other statistics, change to the shape of the distribution. 
# 2 to 4 paragraphs should be sufficient.

#### Note: In general this is a bad idea to remove observations this way.
####       This is just a programming exercise.


# ---- your code and comments here ---- #
#### (1) ####
summary(df$area)
#Min.  1st Qu.  Median   Mean   3rd Qu.    Max. 
#12.0    20.5    41.0  1252.7   183.2  16988.0
range <- sum(max(df$area) - min(df$area)) #16976
IQR(df$area) #163
sd(df$area) #3371
df_skewness <- sum((df$area-mean(df$area))^3) / length(df$area) / sd(df$area)^3
#3.1
df_kurtosis <- sum((df$area-mean(df$area))^4) / length(df$area) / 
  sd(df$area)^4 - 3 #9.6

hist(df$area, main = "Histogram of Island Area", xlab = "Area (1000 km^2)", 
     breaks = seq(0, 17000, by=1000), xlim=c(0,17000), xaxt="n")
axis(1, at=c(0,17000), labels=c("",""), lwd.ticks=0)
axis(1, at=seq(0 , 17000, by=1000), lwd=0, lwd.ticks=1)
abline(v=mean(df$area), col = "red")
abline(v=median(df$area), col = "blue")

#### (2) ####

#I tried two ways of doing this, initially by just subtracting the maximum
#value as the data appears very left-skewed. Then I tried a more detailed loop 
#that compared the min and max to see which would lower the kurtosis the most,
#then subtract that value from the data set. Surprisingly, the simple method of 
#removing the maximum actually left a data set with more entries than the 
#one involving preferential subtraction... I have left the code for both methods
#in, but will use the result from the simple subtraction of maximum method for
#the rest of the task.

area_kurtosis <- e1071::kurtosis(df$area)
while(abs(area_kurtosis) > 3){
  maxi <- which.max(df$area)
  df <- df[-c(maxi), ]
  area_kurtosis <- e1071::kurtosis(df$area)
}
df

#line to reset data for testing both loops.
df <- data.frame(island = names(temp), area = temp, row.names=NULL)


area_kurtosis <- e1071::kurtosis(df$area)
while(abs(area_kurtosis) > 3){
  maxi <- which.max(df$area)
  mini <- which.min(df$area)
  df_max_temp <- df[-c(maxi), ]
  df_max_temp_k <- e1071::kurtosis(df_max_temp$area)
  df_min_temp <- df[-c(mini), ]
  df_min_temp_k <- e1071::kurtosis(df_min_temp$area)
  ifelse(df_max_temp_k < df_min_temp_k, 
         df <- df_max_temp, 
         df <- df_min_temp)
  area_kurtosis <- e1071::kurtosis(df$area)
}
df


#### (3) ####
summary(df$area)
#Min.  1st Qu.  Median    Mean  3rd Qu.    Max. 
#12.00   16.00   29.00   34.83   43.00   89.00 
range <- sum(max(df$area) - min(df$area)) #77
IQR(df$area) #27
sd(df$area) #23
df_skewness <- sum((df$area-mean(df$area))^3) / length(df$area) / sd(df$area)^3
#1.08
df_kurtosis <- sum((df$area-mean(df$area))^4) / length(df$area) / 
  sd(df$area)^4 - 3 #0.006

hist(df$area, main = "Histogram of Island Area v2", xlab = "Area (1000 km^2)", 
     breaks = seq(0, 100, by=20), xlim=c(0,100), xaxt="n")
axis(1, at=c(0,100), labels=c("",""), lwd.ticks=0)
axis(1, at=seq(0 , 100, by=20), lwd=0, lwd.ticks=1)
abline(v=mean(df$area), col = "red", lwd = 2)
abline(v=median(df$area), col = "blue", lwd = 2)



#### (4) ####

#There are major differences between the data sets both in centrality and 
#dispersion. By nature of the loop used to create the second set, the kurtosis 
#improved to a mesokurtic range, extremely close to optimal in fact with 
#kurtosis going from 9.6 to 0.01. Skewness also improved alongside this, 
#decreasing from a 3.1 into the optimal/symmetrical range at 1.08, but 
#remaining slightly right-skewed.

#There were drastic improvements in terms of dispersion with range decreasing
#from 16976 to only 77! Overall number of entries in the data set was reduced 
#from 48 to 35, with those 13 removals comprising the upper quartile of the 
#initial data set.

#By removing this upper quartile, the centrality of the data set was also
#subsequently improved, the gap between mean and median improved from over 1200
#to only 16, and interquartile range also dropping significantly from 163 to 27.






# Task 3 - (30 points) ---------------------------------------------------------

# For this task you will analyse self-reported heights of students (in inches). 
# Data were added manually, so there are a lot of mistakes.

df <- dslabs::reported_heights

# There are two questions to investigate:
# First, you will compare accuracy of data entry for males and females.
# Second, you compare distributions for heights for males and females.

# You will need to think very carefully what numbers are legitimate and
# what are not, and then do data cleaning. 
# For example, 1.5 IQR rule can be useful here.


# ---- your code and comments here ---- #

summary(df$height)
df <- df[ ,-c(1)] #removing time stamps
df$height <- as.numeric(df$height) #converting heights to numeric
df$height <- round(df$height, digits = 1) #rounding to 1 decimal place
table(df$sex) #Counts = 248 Female, 847 Male
summary(df$height) #81 NA's = all results containing non-numeric characters
#Min.  1st Qu.   Median     Mean   3rd Qu.     Max.     NA's 
#0.00    65.00    69.00    94.59    72.00  11111.00       81 

dirty <- (which(is.na(df$height))) #collect dirty data 
dirty_df <- data.frame(df[c(dirty), ]) #store in separate data frame

df <- df[-c(which(is.na(df$height))), ] #remove NA's from df
q1_q3 <- quantile(df$height, probs = c(0.25, 0.75))
low_ol <- sum(q1_q3[1]-(1.5*IQR(df$height))) #54.5
high_ol <- sum(q1_q3[2]+(1.5*IQR(df$height))) #82.5
#I decided to run the 1.5*IQR rule across the whole data set as opposed to 
#splitting into the sexes as the known small difference between the average 
#heights of males and females is not going to interfere with the outlier 
#calculation. The values for heights in centimetres, metres or feet do not
#overlap with the outlier limits as calculated above (low = 54.5, high = 82.5),
#therefore it is safe to assume that all of the remaining heights are all of 
#those provided in inches.


dirty2 <- which(df$height > high_ol | df$height < low_ol) 
#219 observations outside of 1.5 IQR
dirty_df2 <- data.frame(df[c(dirty2), ])
dirty_data <- rbind(dirty_df, dirty_df2) #Combining two data frames into one
df <- df[-c(dirty2), ] #Remove outliers via 1.5 IQR rule

df$valid <- TRUE
dirty_data$valid <- FALSE
dfedit <- rbind(df, dirty_data)
#I chose to separate the data across a series of overlapping data frames as this
#allows me to keep track of what has been done to the "main" data set and keeps
#certain subsets of the original data, such as the outliers and incorrectly 
#entered data, in their own frames for further manipulation in the future.
#I understand many of the actions I have taken here could be done with filtering
#and vectorisation but I am more comfortable partitioning the data set into 
#smaller subsets .


############Table to create stacked barplot and calculate reporting proportions
height_table <- table(dfedit$valid, dfedit$sex, dnn=c("valid", "sex"))
height_table
female_prop <- height_table[2,1] / height_table[1,1] #3.07
female_percent <- (height_table[1,1] / sum(height_table[ ,1])) *100 #24.6%
male_prop <- height_table[2,2] / height_table[1,2] #2.54
male_percent <- (height_table[1,2] / sum(height_table[ ,2])) *100 #28.3%
barplot(height_table, 
        main = "Barplot comparing valid to invalid heights between sexes", 
        xlab = "Sex", ylab = "Frequency", col = c("red", "green"), 
        legend = rownames(height_table))
#The proportion of valid:invalid entries across the sexes are quite similar.
#Female participants entered valid data at a 3:1 ratio compared to an invalid 
#entry, where males had a slightly lower value at a 2.5:1 ratio. 
#It is easier to understand in terms of percentage and looking at the barplot, 
#with 24.6% of female entries being invalid, compared to 28.3% of the male 
#entries. 
#This slight discrepancy could be explained by some of the more immature male 
#participants entering in junk data for a bit of a joke such as "1111" or 
#">9000". That said, it is not a significant difference between the two sexes 
#in terms of validity of data entry.

#####Graphs to compare the heights of males vs females with numerical summaries.
hist(df$height[df$sex == "Male"], breaks=seq(50,90,5), col=rgb(1,0.3,0,1/4),
     main="Comparison of height between \n males (orange) and females (purple)",
     xlab = "Height (inches)")
hist(df$height[df$sex == "Female"], breaks=seq(50,90,5), col=rgb(0.5,0,1,1/4),
     add = TRUE)
abline(v = mean(df$height[df$sex == "Male"]), col="orange", lwd = 2)
abline(v = median(df$height[df$sex == "Male"]), col="orange", lty = 2, lwd = 2)
abline(v = mean(df$height[df$sex == "Female"]), col="purple", lwd = 2)
abline(v = median(df$height[df$sex == "Female"]), col="purple", lty =2, lwd =2)
#This graph shows the comparative heights of females and males from the study.
#The colour-coded lines represent the mean(solid) and median(dotted) values for
#each sex. The space between the two in both cases, and the general shape of 
#the bars of the histogram indicate that the data is symmetrical and close to 
#normal distribution.

################# Numerical Summaries for comparing male to female entries
# --- MALE --- #
summary(df$height[df$sex == "Male"])
#Min.   1st Qu.  Median    Mean  3rd Qu.   Max. 
#58.00   68.00   70.00   69.68   72.00    81.00 
sum(max(df$height[df$sex == "Male"]) - min(df$height[df$sex == "Male"])) 
#Range =23
sd(df$height[df$sex == "Male"]) #3.2
e1071::skewness(df$height[df$sex == "Male"]) #-0.12
e1071::kurtosis(df$height[df$sex == "Male"]) #1.03

# --- FEMALE --- #
summary(df$height[df$sex == "Female"])
#Min.   1st Qu.  Median   Mean  3rd Qu.    Max. 
#55.00   63.00   65.00   65.09   67.00   79.00 
sum(max(df$height[df$sex == "Female"]) - min(df$height[df$sex == "Female"])) 
#Range =24
sd(df$height[df$sex == "Female"]) #3.3
e1071::skewness(df$height[df$sex == "Female"]) #0.66
e1071::kurtosis(df$height[df$sex == "Female"]) #2.14
#As can be seen from the above numerical summaries of the male and female
#entries, both sets have similar values and are within the bounds of what would 
#be considered symmetrical. They share many of the same characteristics with 
#the only real difference being that males and females have a height difference
#of 5 inches on average. The distribution of heights across each sex are very
#similar, as will be discussed below.

#A Standard deviation of 3.2 and 3.3 is indicative of good centrality, 
#especially when considered alongside the range values (23 and 24) for 
#dispersion.

#The distribution of both of the sexes can be said to be symmetrical, similar 
#to that of normal distribution with the male and female skewness values (-0.12
#and 0.66) falling within the -3->3 range of what is considered symmetrical. 
#The same can be said for the kertosis values which sit in the mesokurtic range.












# Task 4 - (25 points) ---------------------------------------------------------

# Some researchers propose an alternative measure of tailedness. It is
# calculated as a standard deviation divided by a mean absolute deviations.
# Please google for "calculate mean absolute deviations in R".

# 1) Make a code that calculates kurtosis and new measure proposed above for
# each column of the data set below. You will get values of kurtosis and 
# corresponding values of the new measure. Plot the graph to study a relationship 
# between these two variables. Provide your comments on the findings. 
# Does the new measure do the same job as kurtosis? (1 to 3 paragraphs) 

# Your data set:

mydata <- datasets::volcano

# ---- your code and comments here ---- #

kurt <- NULL
sd_mad <- NULL
x <- 1
while(x <= ncol(mydata)){
  kurt <- c(kurt, e1071::kurtosis(mydata[ ,x]))
  sd_mad <- c(sd_mad, sd(mydata[ ,x]) / mad(mydata[ ,x]))
  x <- x+1
}

x_axis <- 1:61
plot(x_axis, sd_mad, type = "o", 
     main = "Comparison of Kurtosis to StDev/MAD 
     values for volcano dataset", ylab = "Value", 
     xlab = "Column # of Volcano dataset", col = "red", lty = 1,
     ylim = c(-2, 2))
points(x_axis, kurt, col = "blue")
lines(x_axis, kurt, col = "blue")
legend("right", legend = c("StDev/MAD", "Kurtosis"), col = c("red", "blue"), 
       lty = 1, lwd = 2)

#The plot comparing the kurtosis (excess kurtosis), to SD/MAD for the given
#data set shows that the two functions do generally follow the same pattern
#as can be seen by the similar shape of the curves. The kurtosis line appears 
#lower than the SD/MAD method by a value of ~2 and there is less variation
#between the values obtained by neighbouring columns (ie. the line appears less
#jagged).
#Another point to note, the amplitude of the two lines look to be of a 
#comparable level meaning that there is a similar range across the two sets 
#of values. This is confirmed by the range calculations below, both of which 
#returned values of 0.99
#The similar range in values and similar curve shape of the kurtosis and SD/MAD
#values obtained from the volcano set would indicate that they are very 
#comparable in terms of quantifying tailedness.

sum(max(kurt) - min(kurt)) # Range = 0.99
sum(max(sd_mad) - min(sd_mad)) # Range = 0.99



# 2) Kurtosis for Normal distribution is equal 3. Excess kurtosis for Normal
# distribution is 0. What is the value of a new measure for Normal distribution?
# Run numerical tests using function rnorm() to generate Normal distribution
# values, check excess kurtosis and the new measure for generated data. Report
# the values (1 paragraph).

# ---- your code and comments here ---- #
x=1
test_kurtosis <- NULL
test_sdmad <- NULL
repeat{
  test <- rnorm(1000)
  test_kurtosis <- c(test_kurtosis, e1071::kurtosis(test))
  test_sdmad <- c(test_sdmad, sd(test) / mad(test))
  x <- x+1
  if(x>100){
    break
  }
}
mean(test_kurtosis) # Mean = -0.04
mean(test_sdmad) # Mean = 0.99

#I created a repeat loop to run kurtosis and SD/MAD calculations on 100 normal
#distribution data sets of length 1000 then took the mean of these vectors to
#determine the values. (This was probably unnecessary but an interesting 
#experiment nonetheless).
#The kurtosis value for Normal distribution is 0 and the SD/MAD value is 1.
#Both functions appear to serve their purpose quite well, at least in the case 
#of Normal distribution. Either is a good choice to assess the tailedness of a 
#data set, so long as you remain aware of their 'Normal' values.




# THE END - DON'T FORGET TO SAVE YOUR R-SCRIPT ---------------------------------



