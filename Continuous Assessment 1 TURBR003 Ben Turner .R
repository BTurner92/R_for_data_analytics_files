################################################################################
# MATH 2032 - Continuous Assessment Test 1
# SOLUTIONS
################################################################################

# This is a graded test worth 10% of your final mark.

# Complete tasks listed below. Type your code in each section.
# Provide discussions where required.

# Save this file with your code and submit it through LearnOnline





# Task 1 - (15 points) ---------------------------------------------------------

# Create a variable "mydata" - a data structure with the output presented 
# below as comments. Pay attention to all details!

# > mydata
# [[1]]
# [1] 1 2 3 4 5
# 
# $mix
# $mix$a
# [1] "Create a variable 'mydata'"
# 
# $mix$b
#   first second third
# 1    97     98    99
# 
# 
# [[3]]
# [1] "a data structure with the output presented below"

# ---- your code here ---- #

mydata <- list(1:5, mix = list(a = "Create a variable 'mydata'", 
                                b = data.frame(first = 97, second = 98, third = 99)),
               "a data structure with the output presented below")
print(mydata)
# Use different ways of indexing to extract value 99 from that object.
# Provide at lease three different options.

# ---- your code here ---- #
mix[[2]][1,3]
mydata$mix$b$third
mix[["b"]][1,"third"]





# Task 2 - (20 points) ---------------------------------------------------------

# Create a variable "df" - a data structure with the output presented 
# below as comments

# > df
#    x  y
# 1 -3  3
# 2 -1  4
# 3  1 -5
# 4  3  6

# ---- your code here ---- #
df <- data.frame(x = seq(length= 4, from= -3, by= 2), y = c(3,4,-5,6))


# Use control flow operators only(!!!) to change values in "df" as follows: 
# all negative numbers should be multiplied by 3; 
# all even numbers should be divided by 2.
# Everything else should remain the same.

# ---- your code here ---- #
x_ <- c(-3,-1,1,3)
x_res <- c()

for(i in x_){
  if(i < 0){
    x_res <- c(x_res, i*3)
  } else if(i > 4){
    x_res <- c(x_res, i/2)
  } else {
    x_res <- c(x_res, i)
  }
}
x_res

y_ <- c(3,4,-5,6)
y_res <- c()

for(i in y_){
  if(i < 0){
    y_res <- c(y_res, i*3)
  } else if(i >= 4){
    y_res <- c(y_res, i/2)
  } else {
    y_res <- c(y_res, i)
  }
}
y_res

df <- data.frame(x = x_res, y = y_res)
df
#a pretty clumsy way to go about it I will admit...


# Get back to the original values in "df" and do the same changes
# by using as much vectorisation as possible. That is, your new code
# should be shorter and use less "for" and "if" statements.
# Ideally, there should be no "for" and "if" at all.

#returning to original values for df
df <- data.frame(x = seq(length= 4, from= -3, by= 2), y = c(3,4,-5,6))

# ---- your code here ---- #
df$x[c(1,2)] <- df$x[c(1,2)] * 3
df$y[3] <- df$y[3] * 3
df$y[c(2,4)] <- df$y[c(2,4)] / 2
df



# Task 3 - (15 points) ---------------------------------------------------------

# There are two very useful functions - max() and min(). They return maximal 
# and minimal values of the numerical vector. 

# Create your own custom function minmax() that does the same job. That
# is, it takes a numerical vector as an input and outputs a vector with minimal 
# and maximal values from this vector. 
# Use only control flow operators - don't use build-in min and max functions.

# Prepare data and check min/max values

test.vector <- rnorm(20)
test.vector

# These are results you should get from your function
c(min = min(test.vector), max = max(test.vector))


# ---- your code here ---- #
minmax <- function(x){
  MIN <- NULL
  MAX <- NULL
  for(i in x){
    if(is.null(MIN) == TRUE){
      MIN <- i
      result <- c("min"= MIN, "max" = MAX)
    } else if (is.null(MAX) == TRUE){
      MAX <- i
      result <- c("min"= MIN, "max" = MAX)
    } else if(i < MIN){
      MIN <- i
      result <- c("min"= MIN, "max" = MAX)
    } else if (i > MAX){
      MAX <- i
      result <- c("min"= MIN, "max" = MAX)
    } else{
      MIN <- MIN
      MAX <- MAX
      result <- c("min"= MIN, "max" = MAX)
    }
  }
  return(result)
}
minmax(test.vector)



# Task 4 - (15 points) ---------------------------------------------------------

# Take any integer number. If it is even, divide it by 2; if it is odd, multiply
# it by 3 and add 1. This is step 1. Repeat this step many times and eventually
# you will get number 1, then you have to stop. Regardless, whatever your initial
# integer number, this process will end up with 1.
# Create a custom function that would run the above experiment for the given 
# integer number and return the number of steps it takes to get to 1.

X <- sample.int(100, 1)       # Just a random integer number


# ---- your code here ---- #
Steps_to_1 <- function(x){
  step_count <- 0
  while(X != 1){
    X_even <- X %% 2 == 0
    if(X_even == TRUE){
      X <- X / 2
      step_count <- step_count +1
    } else{
      X <- (X * 3) +1
      step_count <- step_count +1
    }  
  }
  return(step_count)
}

Steps_to_1(X)




# Task 5 - (15 points) ---------------------------------------------------------

# In this task you will do a text analysis. Run the code below.

# Take a vector of characters below - variable "text". 

text <- c("Take any integer number. If it is even, divide it by 2; if it is odd, multiply",
          "it by 3 and add 1. This is step 1. Repeat this step many times and eventually",
          "you will get number 1, then you have to stop. Regardless, whatever your initial",
          "integer number, this process will end up with 1.",
          "Create a custom function that would run the above experiment for the given ",
          "integer number and return the number of steps it takes to get to 1.")

# Split the text in individual words.
# You don't know this function - it is OK, you will learn it later (in week 9)
# Check the data structure of the variable "words"
words <- strsplit(text, split = " ")    
words


# Create an array with counts of how many times a word with one, two, 
# three, ... etc. letters appears in each sentences. That is, the array with 
# columns representing elements of "text" and rows representing lengths of words
# (1 letter, 2 letters, etc.). You need to get number of rows and columns from 
# the data. When you do counting, if there are no words with (for example) two 
# letters, then the count for two-letter words should be zero.

# Hint: use function nchar() to get a length of the word.

# ---- your code here ---- #

#function that converts a string into a vector of the number of characters
#in each word of the input data type.
char_per_string <- function(x){
  lengths_ <- c()
  for(i in x){
    lengths_ <- c(lengths_, nchar(i))
  }
  return(lengths_)
}
#while loop to run the character length function char_per_string until length of 
#input is reached.
x <- 1
conv_words <- list()
while(x <= length(words)){
  entry <- char_per_string(words[[x]])
  conv_words[[x]] <- c(entry)
  x <- x +1
}
conv_words

#calculation for the max character length
max(char_per_string(words))


#converting to a character frequency list in preparation for array creation
char_count <- list(rep(0, 11), rep(0, 11), rep(0, 11), rep(0, 11), rep(0, 11), 
                   rep(0, 11))
x <-1
while(x <= length(conv_words)){
  for(i in conv_words[[x]]){
    word_length <- conv_words[[x]][i]
    char_count[[x]][word_length] <- char_count[[x]][word_length] + 1
  }
  x <- x+1
}
#conversion from a list into a 2D array
text_data <- array(unlist(char_count), dim=c(11,6,1))
text_data






# Task 6 - (20 points) ---------------------------------------------------------

# Load dataset "Titanic" - count of passengers survived/died on Titanic.
# Run the following code:

data(Titanic)
print(Titanic)

?Titanic   # check the help file for this data set
?sum
# Use indexing to extract information and answer the following questions:

# ---- your code here ---- #

# 1. How many male crew members died?
Titanic[ 4, 1, 2, 1] 
  # 670




# 2. How many crew members were on Titanic in total?
sum(Titanic[4, 1, 2, ], Titanic[4, 2, 2, ]) 
  # 885




# 3. What proportion of female passengers survived? 
sum(Titanic[-4, 2, , 2]) / sum(Titanic[-4, 2, , ]) 
  # = 0.7248 or 72.48%





# 4. What proportion of male passengers survived? 
sum(Titanic[-4, 1, , 2]) / sum(Titanic[-4, 1, , ])
  # = 0.2013 or 20.13%




# 5. What proportion of crew members survived?
sum(Titanic[4, , , 2]) / sum(Titanic[4, , , ])
  # 0.2395 or 23.95%




# 6. First-class passengers were on the higher decks and, as a result, they had 
# a higher chance to survive. How much higher was the probability to survive 
# for the first-class passengers compared to the third class on the lower decks?

first <- sum(Titanic[1, , , 2]) / sum(Titanic[1, , , ]) # = 0.6246...
third <- sum(Titanic[3, , , 2]) / sum(Titanic[3, , , ]) # = 0.2521...
first / third # = 2.477...

  #Therefore the passengers in first class were 2.477 or almost 2.5 times as 
  #likely to survive than passenger in third class




# THE END - DON'T FORGET TO SAVE YOUR R-SCRIPT ---------------------------------



