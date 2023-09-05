## Week 5 Notes and examples
rm(list = ls())
gc() # cleans up everything on computer/GE provides readout of R usage

######## APPLY FUNCTION #########
# Allows for simplified syntax when conducting analysis on larger sets of data.

df <- mtcars
# Apply allows for batch calculations/operations to be done across many sets.
# Designed to work on arrays/matrices (will convert vectors or dfs)
?apply

apply(df, 2, mean)
colMeans(df) #this does the same thing but is a rare exception...

apply(df, 2, sd) # vector, dimension you want it to run, function to run

#can also be used with custom functions OR temp functions
apply(df, 2, function(x) mean(x) / sd(x))

#-----------------------------------------------------------------------------#
df <- iris # Time to try a new data set

head(df) # Note that the 5th column contains char vector Species
apply(df, 2, mean) #THIS WILL FAIL.... instead
apply(df[, -5], 2, mean) #leave out Species, data is categorical

# Variations
sapply(df, 2, mean) # For working with vectors, will run but excuse incorrect
# data types
lapply(df, 2, mean) # Same but for lists.

#use table to get species counts of iris instead
table(df$Species)

my.data <- rnorm(5)
sapply(my.data, round, 2) #both will do the same thing
sapply(my.data, function(x) round(x, 2))

# Can also be done as a for loop but this is resource intensive
res <- c()
for(x in my.data){
  res <- c(res, round(x, 2))
}
res

# Can use a sequence of numbers as the input to act as a counter, similarly to 
# how you would in a for loop.
sapply(seq(1, 5, 2), function(i) round(my.data[i], 2)) #extracts only odd values

#does the same thing as 
res <- c()
for(i in seq(1,5,2)){
  res <- c(res, round(my.data[i], 2))
}
res



#-----------------------------------------------------------------------------#
#################### DATA MANIPULATION ####################
# dplyr package
# Revision to start...
df <- USArrests
df2 <- df[df$UrbanPop > 50, ] # trim low population centres
df3 <- df[ , c("Murder", "Assault")] #filter for violent crime

df$Dangerous <- ifelse(df$Murder > mean(df$Murder) & df$Assault > mean(df$Assault),
                       "Dangerous", "Not too bad") 
# Selecting based on above average values for violent crime.
table(df$Dangerous) # 21 Dangerous, 29 Not too bad

############ RESET ############
df <- USArrests
library(tidyverse)
# Big, common, very useful series of packages including ggplot2, dplyr, 
# tibble, stringr, tidyr

# The following do the same as the Arrests set above
df2_dplyr <- filter(df, UrbanPop > 50, Rape > mean(Rape)) 
#vector, column and condition(s)
df3_dplyr <- select(df, Murder, Assault) # column, column, column ... no "", $
df4_dplyr <- mutate(df, Dangerous = ifelse(Murder > mean(Murder) &
                                             Assault > mean(Assault), 
                    "Dangerous", "Not too bad"))
df2_dplyr
res <- count(df4_dplyr, Dangerous)
res 
#     Dangerous  n
# 1   Dangerous 21
# 2 Not too bad 29

#Key distinction, all of these dplyr functions are designed to be run on
# data frames and thus produce results that are data frames!

df5_dplyr <- arrange(df4_dplyr, Dangerous, desc(UrbanPop))
df5_dplyr # sorts by Dangerous then within, in descending order based on Pop

summ <- summarise(df, MM = mean(Murder), MSd = sd(Murder), 
                  MedR = median(Rape), round(summ, 2))
summ
#     MM  MSd MedR
# 1 7.79 4.36 20.1
# Allows you to include any number of variables that you want
# Even allows for running extra functions on results eg. rounding

#-----------------------------------------------------------------------------#
############## PIPING ################
# Flow of function results from one into another.

help(package = dplyr) #Theres a lot here...
#benefits of piping are to minimise number of active variables, maximising
# available RAM. Also, just a bad practice as there are many similar variables 
# with similar names, hard to tell them apart.
rm(list = ls())
df <- USArrests

# %>% is the symbol for piping
# Removes the need to store as a variable
# OR can pipe muultiple sets into a variable
df %>% summarise(MM = mean(Murder), MSd = sd(Murder)) # -> y

df %>%
  mutate(Dangerous = ifelse(Murder > mean(Murder) & Assault > mean(Assault), 
                            "Dangerous", "Not too bad"), 
         State = rownames(.)) %>% # dot notation used here, a reference to the 
                                  # original df. In this case, makes new column
                                  # using the row names, dot replaces dplyr notation
# Without the state = rownames code, rownames get replaced by indexes instead, 
# doing this keeps the rownames applicable and available
  arrange(Dangerous, desc(UrbanPop)) %>% # sort by dangerous, high to low pop
  filter(Dangerous == "Not too bad") %>% # remove dangerous states
  select(UrbanPop, Murder) %>% #select only these 2 columns
  plot()
#see the plot at the end BUT nothing is stored in memory!

# control shift c = comment out multiple lines with them selected.

############# Pull
df %>% select(UrbanPop) %>% pull() 
#pulls data frame info into a vector
#good if you don't want the data frame type provided by tidyverse functions.
# same as...
df$UrbanPop

#-----------------------------------------------------------------------------#
############### JOINING DATA #######
# base has function merge, dplyr has join
library(dplyr)

# JOIN and MERGE
producers <- data.frame(
  name = c("Spielberg", "Scorsese", "Hitchcock", "Polanski", "Zemeckis"),
  nationality = c("US", "US", "UK", "Poland", "US"), 
  stringsAsFactors = FALSE)

movies <- data.frame(
  surname = c("Spielberg", 
              "Scorsese", 
              "Hitchcock",
              "Hitchcock",
              "Spielberg",
              "Tarantino",
              "Polanski"),
  title = c("Super 8",
            "Taxi Driver",
            "Psycho",
            "North by Northwest",
            "Catch Me If You Can",
            "Reservoir Dogs", "Chinatown"),
  stringsAsFactors = FALSE)
producers
movies

merge(producers, movies, by.x = "name", by.y = "surname")
# Lots of different data is missing, only compiles data present in both.
?merge

merge(producers, movies, by.x = "name", by.y = "surname", all = TRUE)
#all = true fills in the gaps with NAs

####### 4 TYPES OF JOINING #########
left_join(producers, movies, by = c("name" = "surname"))
#Keeps all info from left data set and adds in the right: producers-movies
right_join(producers, movies, by = c("name" = "surname"))
#Keeps all info from right data set and adds in the left: movies-producers
inner_join(producers, movies, by = c("name" = "surname"))
#Only keeps rows common to both
full_join(producers, movies, by = c("name" = "surname"))
#Joins completely, fills in gaps with NAs

# --- Can do the same thing with the following syntax using piping
producers %>% full_join(movies, by = c("name" = "surname"))




#-----------------------------------------------------------------------------#
##### Other dplyr functions
# _all _at and _if variations on the core dplyr functions
df <- iris

df  %>% mutate_all(log) #does not work
df %>% mutate_if(is.numeric, log) %>% head(10) # mutate_if(condition, function)

df %>% summarise_all(mean) #does not work for numerical
df %>% summarise_if(is.numeric, mean) #Squashes into one row
df %>% summarise_if(is.numeric, list(M=mean, S = sd)) #Note the syntax!

# Despite advantages of not needing "" or $ notation, sometimes it is easier to
# use a variable
my.cols <- c("Sepal.Length", "Sepal.Width")
df %>% mutate_at(my.cols, sqrt) %>% mutate_at(my.cols, round, 3) %>% head(10)
# note the syntax, need to include mutate at every step if you want things to 
# flow nicely

df %>% select_at(my.cols) %>% head(10) #_at isn't necessary here
# can also index by name obviously
df %>% select(Sepal.Length, Sepal.Width) %>% head(10)

test_date <- as.Date(2020)








