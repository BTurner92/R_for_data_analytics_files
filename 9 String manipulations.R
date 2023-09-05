# STRING MANIPULATIONS AND REGULAR EXPRESSIONS

# Working with unstructured data
# Important with natural language analysis

# SPLIT
notes <- "String (or a character) is very common data type and a lot of data is 
stored as simple text."

strsplit(notes, split = " ")
wap <- readLines("warandpeace.txt")
head(wap)

wap.split <- strsplit(wap, split = " ")
head(wap.split)
# Lots of blank spaces...

word.count <- table(unlist(wap.split))
# Split all words into a table
word.count <- sort(word.count, decreasing = TRUE)
# Easy way to determine the most used words in war and peace.
head(word.count, 10)

library(wordcloud)
wordcloud(words = names(word.count),
                     freq = word.count,
                     min.freq = 100, max.words = 250,
                     random.order = FALSE, rot.per = 0.35)
wordcloud(words = names(word.count[50:200]),
          freq = word.count[50:200],
          min.freq = 100, max.words = 250,
          random.order = FALSE, rot.per = 0.35)
#Note differences between the words, in second includes a lot of character names

# COMBINE
paste("Good", "morning", sep = " ")
# two vectors become one

x <- c("John", "Tim", "Bill")
paste(x, "is a good student.", sep = " ")

paste(x, collapse = "_")
#collapses into one character

paste(x, "good student", sep = " ", collapse = "_")
# can combine the two together!

some.data <- rnorm(15)
print(paste("Mean value is ", round(mean(some.data), 2), " mm", sep = ""))
# creating a meaningful print combining multiple vectors and an operation

sprintf("Mean value is %f mm.", mean(some.data))
# %f value is replaced by the output of some other function

sprintf("Mean value is %.2f mm. Standard deviation is %.3f", 
        mean(some.data), sd(some.data))
#can expand on this format further by including .X for number of decimal 
# places for example

# PATTERN MATCHING
?grep
wap[1001:1010]
grep(pattern = "you", wap[1001:1010])
#Matches to pattern provided in quotation marks.
# Numbers provided are the lines indexes of the result that contain the pattern

grepl(pattern = "you", wap[1001:1010])
# Provides logical value return

#find how many times the name Pierre is mentioned in WaP
length(grep(pattern = "Pierre", wap)) # 1939

# SUB
#substituting and replacing
sub(pattern = "princess", replacement = "principessa", wap[1001:1010])
#pretty self explanatory here.... only removes first instance of pattern

gsub(pattern = ",", replacement = "", wap[1001:1010])
#useful as part of data cleaning operations, removes all instances of pattern
gsub(pattern = "-", replacement = "", wap[1001:1010])





#### REGULAR EXPRESSIONS -----------------------------------------------------
# Act similarly across all programming languages, almost acts like a language
# itself

test <- "The quick brown fox jumps over the lazy dog."

gsub(pattern = "[aeiou]", replacement = "", test) # x = test syntax also works
# Square brackets indicate that the expression is of a special class (regex)

?regex #list available here Regular Expressions as used in R

temp <- gregexpr(pattern = "[[:alpha:]]+", text = wap[1001:1010])
temp
# strange syntax here, readout of results is a bit convoluted

regmatches(x = wap[1001:1010], m = temp)
# splits everything exactly by words without any of the punctuation when used 
# in conjunction with the regex in previous line.

# https://regexone.com/

address <- "Barbara Hanrahan Building, 14/18 Fenn Pl, Adelaide SA 5000"

temp <- regexpr (pattern = "\\d{4}", text = address)
# extract digits with a length of 4
# Note R requires 2 \\, other language only need 1
temp
regmatches(x = address, m = temp)
# provides index, length and data type

temp <- gregexpr (pattern = "\\d+", text = address)
# plus syntax keeps char type together so long as char type stays the same.
# eg. it will output numbers so long as the next char is a digit and group 
# them together
# Needs greg as otherwise will only output the first!

"\\d+$" # starts from the end with reg, with greg will go in reverse

temp <- gregexpr (pattern = "[[:upper:]]{2,3}", text = address)
regmatches(x = address, m = temp)
# Extracting the state name abbreviation, case with length 2 or 3

temp <- gregexpr (pattern = "([[:upper:]]{2,3} \\d+$)", text = address)
# Combining the two

#
#phone number
phone <- "(08) 1234-5678"
temp <- gregexpr(pattern = "[[:digit:]]+", text = phone)
regmatches(x = phone, m = temp)
#produces numbers, separated into 3 sets

gsub(pattern = "\\D", replacement = "", x = phone)
# returns only the digits!
# d means digits, D means NOT digits



### STRINGR - stringr --------------------------------------------------------
library(tidyverse)
notes
str_split(notes, pattern = " ")
notes %>% strsplit(" ")
#can easily use piping functionality/syntax

str_c("a", "b", "c", sep = "_")
#simple and modifiable string concatenation (instead of paste or default c)

some_data <- rnorm(15)
mu <- mean(some_data)
sigma <- sd(some_data)

str_glue("Mean value is {mu}. Standard deviation is {sigma}")
# Mean value is 0.0655807416701379. Standard deviation is 0.969924810970597
str_glue("Mean value is {round(mu, 2)}. Standard deviation is {round(sigma, 2)}")
# Mean value is 0.07. Standard deviation is 0.97
#Any R code can be contained within the curly brackets when used with str_glue

# pattern matching

str_detect(string = wap, pattern = "Pierre")
# produces logical output for each object in string

sum(str_count(string = wap, pattern = "Pierre"))
# using sum just returns the number of counts
table(str_detect(string = wap, pattern = "Pierre"))
# Returns the same but also with # FALSE values

# BUT
# Pierre may appear multiple times within the same line
# therefore...
sum(str_count(string = wap, pattern = "Pierre") > 1)
# 24, number of times Pierre appears more than once

#string extract
str_extract(wap, "\\d+")
# lists per row

wap %>% str_extract_all("\\d+") %>% unlist()
#list every digit in the book



# Formatting 

test <- "    This       text   has       too     many         spaces."
str_remove_all(test, pattern = " ")
# "Thistexthastoomanyspaces."
str_replace_all(test, pattern = " ", replacement = " ")
# Fixes with correct single space, looks a bit naff, needs to be repeated

str_trim(test)
# takes spaces away from top and tail
str_squish(test)
# Removes all unnecessary spaces in between words
str_to_upper(test)
# ALL UPPER CASE
str_to_lower(test)
# all lower
str_to_title(test)
# capitalise each word

# combine with piping
test %>% str_to_upper() %>% str_squish()

?stringr
#MAAAAANY very useful string manipulation functions available within this package



