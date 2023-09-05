df <- mtcars
View(mtcars) #Clear view of entire data as a table

par(mfrow = c(1,2)) #prepare a graphics space for the 2 graphs.
hist(df$mpg, main = "Histogram of MpG")
boxplot(df$mpg, main = "Boxplot of MpG")

par(mfrow = c(1, 1)) #back to one graph per window

summary(df$mpg)



########## SKEWNESS ##########
#A measure of the data symmetry, specifically around direction of data skew.
#eg. leans to left or right.
#it is a function of variance and third moment, involves length of data set.

#manual skewness calculation
x <- df$mpg
sum((x-mean(x))^3) / length(x) / sd(x)^3
e1071::skewness(x) #function from package, results are the same.

#IF results positive, positively skewed or right skewed, lies more towards
#high end of results.
#IF results negative, negatively skewed, leans more towards lower end of 
#results.
#ZERO is symmetrical data (generally considered relatively sym if between -2-2)


########## KURTOSIS ##########
#A measure of the tailedness of a data set, how much is present outside of the
#norm at the top and tail (higher extremes or little/flat extremes.
#Tails defined as heavy (leptokurtic) or light (platykurtic).
#Same as skewnesss but to power of 4
#Indicates likelihood of finding strange values, outside the norm.
x <- df$mpg
sum((x-mean(x))^4) / length(x) / sd(x)^4 #traditional calculation, requires
#subtraction of 3, turns excess kurtosis into true kurtosis       
e1071::kurtosis(x)
sum((x-mean(x))^4) / length(x) / sd(x)^4 - 3
#Result can range from -3 to positive infinity
#3 is the kurtosis of a set w/ normal distribution.






#################### VIDEO EXAMPLES ####################
########## Centrality and Degree of Dispersion ##########
df <- dslabs::us_contagious_diseases
head(df)
table(df$disease)

# only rows for measles in new data frame
df <-df[df$disease == "Measles", ]
head(df)

#only from years 1960 via creation of a logical index
df <- df[df$year == 1960, ]
head(df) #went from 16000 down to 51 results

#removing disease and year columns as they are now unnecessary
df <-df[ , -c(1,3)]
head(df)

#create a histogram of this new data
hist(df$count)
#doesn't factor in population, therefore create new variable for proportion of 
#cases per state, per 100,000
df$prop <- df$count / df$population *100000
head(df)

#further normalisation required as each state did not report for the entire
#year. Must calculate # of cases per 100,000 per year
df$prop <- df$prop / df$weeks_reporting *53
#use 53 as it does not matter that this is too many for a year, but will
#normalise data to same point
head(df)

########## Looking at the statistics ##########
hist(df$prop)
boxplot(df$prop)
summary(df$prop)
df <- na.omit(df)

median(df$prop)
IQR(df$prop)
View(df)

#finding the outliers
df$state[df$prop >900]
#outliers are Hawaii, Vermont, Wisconsin


########## Shape of distribution: Skewness and Kurtosis ##########
boxplot(df$prop)
abline(h= mean(df$prop), col = "red")

hist(df$prop)
abline(v = mean(df$prop), col = "red")
abline(v = median(df$prop), col = "blue")
#large gap between mean and median indicates that data are not symmetrical
#can also tell by the shape of the graphs, appears data are skewed

e1071::skewness(df$prop)
#NOTE round up data, no one cares about your decimal places
#positive answer here, therefore right-skewness (due to tail)

par(mfrow = c(1, 2))
#alters space to provide two graphs side by side, parameters row, column

e1071::kurtosis(df$prop)
# = 1.36, outside of norm (zero) therefore slightly heavy tails (leptojurtic)
#BUT not outside -3->3 values therefore kurtosis is not significant











