# Exercises for week 9 ---------------------------------------------------------
# String manipulations and Regular expressions


# Please try to complete tasks listed below. Type your code in each section.



# clean up global environment
rm(list = ls())

library(tidyverse)



### Load data ------------------------------------------------------------------

# Load the full text of Romeo and Juliet by William Shakespeare.
# You need internet connection for the code below to work

con <- url("http://www.gutenberg.org/files/1513/1513-0.txt", encoding = "utf-8")
RnJ <- readLines(con)
close(con)

# You can download this text manually from http://www.gutenberg.org/ebooks/1513




### Task 1 ---------------------------------------------------------------------

# Report how many non-empty rows in the poem, how many words in total, 
# how many distinct words, what are the most popular words? 
# Be aware that there are upper and lower case words that are the same,
# e.g. And/and

# Hint: try to avoid any changes to the original variable "RnJ", so you don't 
# need to download it again later 







### Task 2 ---------------------------------------------------------------------

# Each phrase in the poem is attributed to someone listed in the UPPER case
# e.g.
# [982] "MERCUTIO."                                                                                 
# [983] "Nay, gentle Romeo, we must have you dance."                                                
# [984] ""                                                                                          
# [985] "ROMEO."                                                                                    
# [986] "Not I, believe me, you have dancing shoes,"                                                
# [987] "With nimble soles, I have a soul of lead"                                                  
# [988] "So stakes me to the ground I cannot move."

# List all "speakers" in the poem and how many times they said something.
# Who was the most "talkative"?











