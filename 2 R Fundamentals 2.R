x <- 10

if (x > 5) {            #condition
  print("run code")     #statement
}

#if and else statements
x <- 3
if (x > 5){
  print("run code")
} else{
  print("try a different code")
}

#if - else - if
x <- 20
if (x > 100){
  print("wow, so much")
} else if (x > 10){
  print("decent amount")
} else {
  print("hmmm, not enough")
}

#ifelse() function statement, if true, if false
x<-12
ifelse(x > 10, "large", "small")
#with multiple input values
x <- c(22,33,1,32,4,5)
ifelse(x > 10, "large", "small")

# SWITCH
#function switch is a series of elifs with final argument being the default
#if dealing with numerical data, responses are arrayed in ascending order, 
#therefore final will be highest number, any after that will be NULL, no return
x <- "angry"
y <- switch(x,
            happy = "Good to hear",
            afraid = "What has you scared?",
            angry = "Calm down",
            sad = "Cheer up, it's not that bad",
            "Cat got youor tongue?")
print(y)

x <- 2
y <- switch(x, "bad", "alright", "good", "great", "that's not possible")
print(y)

##########################

# LOOPS LOOPS LOOPS
# FOR
for(i in 1:10){
  print(i)
  print("=-=-=-=")
}

x <- c("one", "two", "three")
for(i in x){
  print(i)
}

#prints number of loop, element from vector x at number i, then a border =-=-=
for(i in seq(1, length(x))){
  print(i)
  print(x[i])
  print("=-=-=")
}

# a for loop where each run through concatenates a new entry to the 'res' vector
x <- 1:10
res <- c()
for(i in x){
  res <- c(res, i^2)
}
res

# WHILE
x <-5
while (x>0){
  print(x)
  x <- x-1
}

# REPEAT
#can be dangerous, will loop indefinitely until use BREAK condition
x <- 1
repeat{
  print(x)
  x <- x + 2
  if (x >10){
    break
  }
}

##################################
#FUNCTIONS
c() #concatenation function
ifelse() #ifelse, condition, true action, flase action
sum() #addition function
rnorm(100) #a random distribution of numbers

x <- rnorm(100)
hist(x) #creates histogram of vector in argument, find in plots

####
#function_name <- function()   #creates custom function called function_name
do_division <- function(x, y){
  temp <- x / y
  return(list(x=x, y=y, result = temp)) 
}  

do_division(136, 4)

# ANONYMOUS FUNCTIONS - when you do not plan on using a function again
m<- matrix(rnorm(12), ncol=4, nrow=3)
m

apply(m, 2,sum) #name of array, margin = row or column, function to apply
help(apply)

#create a temp function to find the difference between the min and max values
#of this matrix and round them to 2dp
apply(m, 2, function(x) round(max(x)-min(x), 2))

#could also be written out as a larger custom function
rounded_range <- function(i){
  temp <- max(i) - min(i)
  temp <- round(temp, 2)
  return (temp)
}
apply(m, 2, rounded_range)

#miles to km and back using custom functions
miles_to_km <- function(miles, n.round = 1){
  km <- round(miles *1.60934, n.round)
  return(km)
}
miles_to_km(500)

km_to_miles <- function(km, n.round = 1){
  miles <- round(km / 1.60934, n.round)
  return(miles)
}

km_to_miles(804.7)

#ANONYMOUS functions using dataframes
help(ifelse)

library(dplyr) #must run package first
x <- 3
if_else(x > 10, "large", "small")
??if_else    #belongs to newly downloaded dplyr package

install.packages("beepr") #can install this way OR using tools menu in RStudio
                          #OR can find and downloan manually using CRAN

beepr::beep(4)  #works because of double colon :: syntax
beep(1) #missing :: therefore does not play sound...












