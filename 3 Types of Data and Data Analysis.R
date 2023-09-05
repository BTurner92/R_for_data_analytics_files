rm(list = ls())

#need to download package "readr" to use the following...
#DONE
x <- 1:5
readr::write_csv(x, file = "countingNumbers.csv")
df <- readr::read_csv("countingNumbers.csv")

#note read.csv is a default function, different functionality to read_csv from readr
?read.csv
#Can use in built read.table function when dealing with csv files.
mtcars
write.csv(mtcars, "mtcars.csv")
my.data <- read.table(file = "mtcars.csv", header = TRUE, sep = ",", row.names = 1)
head(my.data)

my.data$am <- factor(my.data$am, labels = c("auto", "manual"))
my.data$cyl <- factor(my.data$cyl, levels = c(4,6,8), labels = c("4cyl","6cyl", "8cyl"),
                      ordered = TRUE)
my.data$hp.cat <- factor(my.data$hp > 140, labels = c("low", "high"), ordered =TRUE)
head(my.data)




#loading data from packages

##### LOAD THESE TWO ######
help(package = "datasets")

help(package = "dslabs")

#these contain many predefined datasets for use in weekly exercises
#can import these entirely and call them
#can import via double colon :: syntax.

#scan is simple way to create a new vector by scanning the terminal for numeric
#or character values. DO NOT NEED COMMAS uncomment the lines below fro an example.

animals <- scan(what="")
#bird cat dog fish giraffe
#ox pig lion
              





######## .RDATA  #########
x <- 2
y <- list(a = 1:3, b=LETTERS[1:3])

save(x, y, file = "mydata.RData")
rm(list=ls())
load(file="mydata.RData")
print(y)
print(x)

#many different additions to be made when saving data.

#Saving as an RData file
save(x, y, z, file = "filename.RData") #stores RData of whatever you desire
load(file = "filename.RData") #brings it back into the R file/RStudio to be analysed


#R will always store and load data to/from the folder designated as the working
#directory. WD can be checked at any time using the function below
getwd()

#Using RData file type can save vectors, functions, even an entire snapshot of a
#session working environment, allowing you to essentially import one R code file
#into another if you desire.






################# DATA VISUALISATION #####################
?plot()
mtcars
df.cars <- mtcars

#simplest = scatter plot
plot (x = df.cars$hp, y = df.cars$mpg)

#LINE
plot (x = df.cars$hp, y = df.cars$mpg, type = "l")
#looks pretty funky but that's the syntax
#a better example
df.air <- datasets::airquality
head(df.air)

plot(df.air$Temp, type = "l", xlab = "Days", ylab = "Temp in F")
#with no other indication of axes, will automatically plot vs indexes as x axis
#can also very easily rename the axes.

#Can get counts of instances within a dataframe, list etc. using table function
#that would've been super handy to know last week. Assign to new variable...
head(df.cars)
cyl.count <- table(df.cars$cyl)
cyl.count

#can use data like this for a plot as well
# BARPLOT #
barplot(cyl.count, xlab = "# of cylinders")



# HISTOGRAM 3
hist(df.cars$hp)
#use breaks to set the different value gaps between histogram points 
#using seq function here, low limit, upper limit, gap
#col rgb values given as r, g, b, saturation between 0-1
hist(df.cars$hp, breaks = seq(0, 400, 20), col=rgb(1,0,1,1))
#can also combine histogram data together (stacked)
hist(df.cars$mpg[df.cars$am == "auto"], breaks=seq(10,40,4), col = rgb(0,0,1,1/2),
     main = "Distribution of fuel consumption \nfor cars with auto and manual transmission", 
     xlab = "Fuel consumption (MpG)", legend = names(df.cars$am))
hist(df.cars$mpg[df.cars$am == "manual"], breaks=seq(10,40,4), col = rgb(1,0,0,1/2),
     add=TRUE)




# BOXPLOT #
#Interesting plot, lots of different points to take from these
boxplot(df.cars$hp, horizontal = TRUE)

#Can use ~ to indicate dependence (ie. variable a is dependent on variable b)
df.cars$am <- factor(df.cars$am, labels = c("auto", "manual"))
boxplot(hp ~ am, data = df.cars, horizontal = TRUE)


##### EXERCISE ######
#Calculate the average mpg for 4cyl, 6 cyl, and 8cyl cars and plot them together
#in a bar plot (using vectorisation only)
mpg_per_cyl <- c("4cyl" = mean(df.cars$mpg[df.cars$cyl == 4]),
                 "6cyl" = mean(df.cars$mpg[df.cars$cyl == 6]),
                 "8cyl" = mean(df.cars$mpg[df.cars$cyl == 8]))
barplot(mpg_per_cyl, main = "Comparison of average mpg for cars with 4, 6, and 
        8 cylinders", col = c("red", "chartreuse", "blue"), 
        legend = names(mpg_per_cyl), beside = TRUE)
#barplot above has included a lot of the extra available modifications to be 
#made to chart elements

# ADDING LINES #
?abline
hist(df.cars$mpg)
abline(h=6, col=c("orange"), lty=1)
#lty for line type
abline(v=16, col=c("green"), lwd=2)
#lwd for line width

?dnorm
#normal distribution density curve...
#this seems a little complex and foreign right now...
xfit <- seq(min(df.cars$mpg), max(df.cars$mpg), length = 40)
yfit <- dnorm(xfit, mean = mean(df.cars$mpg), sd = sd(df.cars$mpg))
yfit <- yfit * length(df.cars$mpg) * 5

lines(xfit, yfit, col = "red", lwd=3)









################################################################################
############################ ANALYSING DATA WITH STATISTICS#####################
rm(list = ls())

df.air <- datasets::airquality
?datasets::airquality
head(df.air)

hist(df.air$Ozone, main = "Ozone Level (ppb)\n1300 - 1500 at Roosevelt Island, 1973")
#centrality measures, simple stats tools
summary(df.air$Ozone)
#provides info on min, max, median, mean, 1 and 3 quartiles, #NAs 
#remember to use na.rm = TRUE to remove NAs if need be

#Could also include a line to better explain these stats
abline(v = median(df.air$Ozone, na.rm=TRUE), col = "magenta", lty = 2, lwd = 3)
range() #min and max values
IQR() #interquartile range (values between 1st and 3rd quartile)
sd() #for standard deviation, not always applicable with data such as this.


####### Ex 2
head(df.air)
hist(df.air$Temp)
boxplot(df.air$Temp) #reasonably well distributed/symmetrical
summary(df.air$Temp) #median and mean very close together

mu <- mean(df.air$Temp, na.rm =TRUE)
sigma <- sd(df.air$Temp, na.rm = TRUE)
temp_values <- mu + c(-2, 2) * sigma
abline(v = temp_values, col = "blue") #90% of the results within these 2 bars
                                      #values 59, 97 (assumes bell curve distro)
abline(v = mu, col="red") #representation of mean
#observations based on the 68-95-99.7% rule, see notes.






#################  Topic Notes Sections ##################
data(USArrests)
head(USArrests)
summary(USArrests)
dim(USArrests)

#turn the names for each entry into categorical data
USArrests$State <- row.names(USArrests)
row.names(USArrests) <- NULL

#focus on UrbanPop
hist(USArrests$UrbanPop, col="purple", freq=FALSE) #shows density not frequency
#Density is the proportion of total within the histogram column.

#some simple DISTRIBUTION functions
min(USArrests$UrbanPop)
max(USArrests$UrbanPop)
range(USArrests$UrbanPop)
length
#Now for CENTRALITY MEASURES
mean(USArrests$UrbanPop)
median(USArrests$UrbanPop)

#Measures of DISPERSION.... and how to calculate them
#mean and sd when data displays symmetrical characteristics.
mean_data <- mean(USArrests$UrbanPop) #take mean, store as vector
deviations <- USArrests$UrbanPop - mean_data #deviations from the mean

variance_data <- sum(deviations^2) / (length(deviations) - 1) #variance of data
var(USArrests$UrbanPop) #function for variance

sqrt(variance_data) #sqrt of variance is standard deviation
sd(USArrests$UrbanPop) #standard deviation function

#95% of normal distribution values
temp1 <- mean(USArrests$UrbanPop) + sd(USArrests$UrbanPop) *c(-2,2)
temp2 <-mean(USArrests$UrbanPop)
abline(v=temp1, col="red", lty=2)
abline(v=temp2, col="blue", lty=2)

#when data are asymmetrical, best to use median and interquartile range
#QUANTILES
quantile(USArrests$UrbanPop, probs = c(0.1, 0.9)) #will show results for value with
#10% below it and the other with 90% below
quantile(USArrests$UrbanPop, probs = c(0.25, 0.75))#first and third quartile
#second quartile obviously is median!
IQR(USArrests$UrbanPop) #Interquartile range = difference between third and first
#IQR is important measure, shows the spread of the data
#If IQR is small, all data are relatively close together (to median)
#If IQR is large, data are spread further away from median.

#USE BOXPLOT FOR THESE TYPES OF DATASETS TO GET THESE DATA EASILY



#STATS SUMMARIES
#Install fbasics? displays summary in easiest way to read many rows, 2 columns
fBasics::basicStats(USArrests$UrbanPop)

#psych::describe for many columns, 2 rows



