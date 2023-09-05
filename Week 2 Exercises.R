# Exercises for week 2 ---------------------------------------------------------
# Control flow operators, Functions, Packages

# Please try to complete tasks listed below. Type your code in each section.
# Solutions are provided as a separate file but don't rush to check it. 
# There are multiple solutions possible and if your code delivers the same 
# result, then it is [probably] correct too.


# clean up global environment
rm(list = ls())



### Task 1 ---------------------------------------------------------------------

# Air temperature above 30 degrees might be considered as "too hot". 
# Temperature below 5 degrees is "too cold". Anything in between is "good".
# Use conditions to take a value of air temperature "Temp" and print
# the corresponding condition: "good" or "too hot" or "too cold".

Temp <- 34  # Try different values to test your code
if(Temp > 30){
  print("too hot")
} else if(Temp < 5){
  print("too cold")
} else{
  print("good")
}
Temp

### Task 2 ---------------------------------------------------------------------

# Use the code from Task 1 and convert it into a custom function that takes 
# as an input air temperature "Temp" and outputs the corresponding condition.
# Then use a loop with a function to get conditions for the following values
# of the air temperature and store them as a vector "conditions"

Temp <- c(0, 15, 35, 20, -10, 44)

Temp_check <- function(x){
  if(x > 30){
    condition <- c("too hot")
  } else if(x < 5){
    condition <- c("too cold")
  } else{
    condition <- c("good")
  }
  return(condition)
}
conditions <- c()
for(i in Temp){
  conditions <- c(conditions, Temp_check(i))
}
conditions

### Task 3 ---------------------------------------------------------------------

# Make a code to re-create a sequence of first 30 Fibonacci numbers
# https://en.wikipedia.org/wiki/Fibonacci_number
# Every next Fibonacci number equals to the sum of two previous numbers
# 1, 1, 2, 3, 5, 8, ...

fib <- function(x){
  if(x<=1){
    return(x)
}  else{
    return (fib(x-1) + fib(x-2))
  }
}
fib_seq <- c()
for(i in 1:30){
  fib_seq <- c(fib_seq, fib(i))
}
fib_seq


### Task 4 ---------------------------------------------------------------------

# Do a Google search and find ready-made package and function to output
# Fibonacci numbers. Get first 30 numbers and compare them to your own result.

numbers::fibonacci(30, sequence = TRUE)
#number are the same, mine was a lot slower than the numbers version.


### Task 5 ---------------------------------------------------------------------

# Take a sequence of normally distributed random numbers as below and 
# count how many times the value in sequence is greater than 2 or less than -2.
# e.g., a sample input c(-3, -2, -1, 0, 1, 2, 3, 4) should return 3
# Make 4 versions of the code by using: 
# (1) for loop; (2) while loop; (3) repeat loop; (4) no loop

X <- rnorm(1000)

########   Example using | operator, acts as OR statement
                            cnt <- 0
                            for(i in X){
                              if(i > 2 | i < -2){
                                cnt <- cnt + 1
                              }
                            }
                            print(cnt)
                            
## 1 for loop
for_count <- 0
for(n in X){
  if(n > 2){
    for_count <- for_count +1
  } else if(n < -2){
    for_count <- for_count +1
  } else {}
}
for_count

## 2 while loop
while_count <- 0
n <- 1
while(n < 1000){
  if(X[n] > 2){
    while_count <- while_count +1
    n <- n+1
  } else if(X[n] < -2){
    while_count <- while_count +1
    n <- n+1
  } else {
    n <- n+1
  }
}
while_count


## 3 repeat loop
repeat_count <- 0
n <- 1
repeat{
  if(n > length(X)){
    break
  }
    else if(X[n] > 2){
      repeat_count <- repeat_count+1
      n <- n+1
    } else if(X[n] < -2){
      repeat_count <- repeat_count+1
      n <- n+1
    } else {
      n <- n+1
    }
}
repeat_count

## 4 no loop 

###### ANSWER ######
cnt <- sum(abs(X) > 2)
print(cnt)


#duplicated numberset to not disrupt the answers for the loops above.
X_f <- c(X[1:1000])

X_ff <- factor(X_f, levels = c(-5, 0, 5), ordered = TRUE)
X_ff[X_f > 2] <- 5
X_ff[X_f < -2] <- -5

table(X_ff) #count via table results...


data_X <- data.frame(X_ff) #same thing but using df and count() function...
data_Xff <- dplyr::count(data_X, X_ff)
data_Xff$n[1] + data_Xff$n[2]


