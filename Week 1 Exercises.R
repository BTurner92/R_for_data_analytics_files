# Exercises for week 1 ---------------------------------------------------------
# Data types and data structures, indexing


# Please try to complete tasks listed below. Type your code in each section.


# clean up global environment
rm(list = ls())



### Task 1 ---------------------------------------------------------------------

# Create a character vector. 
# Use three different methods to get the character vector of length 4.

#Do not need to create variables for these types of tasks, merely asking for
#data output in console...

x <- vector("character", length=4)

x <- character(4)

x <- c(letters[1:4])




### Task 2 ---------------------------------------------------------------------

# Create a sequence of 8 numbers, then apply a mathematical
# operation that should result in the following output:
# [1]    8   16   32   64  128  256  512 1024


x <- seq(length=8, from=3, by=1)
y <- 2^x


### Task 3 ---------------------------------------------------------------------

# Create a data frame as below 
#   x y z
# 1 a 1 5
# 2 b 2 6
# 3 c 3 7

df <- data.frame(x = letters[1:3], y = 1:3, z = 5:7)

# Use different methods of indexing to extract values "a" and "c" together.
# You should get at least 4 different options.

#1
df$x[c(1,3)]

#2
df[[1]][c(1,3)]

#3
c(df[1,"x"], df[3,"x"])

#4
c(df[1,1], df[3,1])

#5
a_and_c <- df[[1]]
a_and_c[c(1,3)]

### Task 4 ---------------------------------------------------------------------


# Run the code below to load data set about passengers of Titanic.

data(Titanic)
Titanic

# Check the help file with data description

help(Titanic)

# Use indexing to extract the number of crew member of both genders 
# survived and died - to get the total count of crew members on Titanic

Titanic[4,1:2,2,1:2]
total_crew <- Titanic[4,1:2,2,1:2]    #did not need to create variable
sum(total_crew)





### Task 5 ---------------------------------------------------------------------



# Run the code below to load data set about car performance.

data(mtcars)
mtcars

# Check the help file with data description

help(mtcars)

# Variable "vs" do not look as truly numerical, it has a meaning
# (V-shape or Straight) but not a numerical value. Hence, it should be 
# converted to factor. Create a new variable in the data set "vs.f" 
# with corresponding factor values - V-shape or Straight.


#vs_edit <- c(mtcars[["vs"]])
#vs.f <- factor(vs_edit, levels = c("0", "1"), labels = c("V-shape", "Straight"))

mtcars$vs.f <- factor(mtcars$vs, labels = c("V-shaped", "Straight"))
head(mtcars)

# Hint: check examples for function "factor()"
help(factor)
