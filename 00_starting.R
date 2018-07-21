# Hello, I'm a comment. Nice to meet you. 
# You can recognize me by the "#" which is at the begin of the line
# If you execute me, nothing will happen
# So, do that just press control + enter on this line
# Also you can sum up new comments putting the # at the begin
# you can do that selecting something and pressing control + shift + c
# try this key combination on the line 8 and welcome to the scripts : )
print("Hello world")


############################ This is header ############################
# you can hide things just click the tiny triangle 
# hide me!


############################ Operations  ############################
3 + 4                       # sum!
pi                          # built in pi value 
pi_plus_five = pi + 5       # storing the result
pi_plus_five                # looking at the result
(pi_plus_five = pi + 3)     # storing and looking at the result
print(paste("the result is ", pi))


a = 1 # alternative to the <-, use it only for asign values 
b = 2 # or in functions like  rnorm(mean = 3, n = 100)
c(a, b) # c is concatenate

my_numbers <- c(1, 2, 3, 46)
(my_numbers <- my_numbers + c(3, 1, 4, 1))
my_numbers * 3


mean(my_numbers)
my_mean <- mean(my_numbers)
print(paste("My mean is", my_mean))
############################ Data types  ############################

text = "text!"
number = 1
(vector <- c(number, 3))
(vector_mixed <- c(number, text))
vector[1]
vector[2]
vector[1:2]
str(vector[1:2])
as.numeric(vector)
as.numeric(vector_mixed) # Does the best he can, but... see warning!

# most common structure is data.frame 
data(USArrests) 			# example included in base R
class(USArrests)
str(USArrests)
head(USArrests) 			# cheack the first lines
# USArrests
USArrests[1 , 2] 			# first row, second column
USArrests[, c(1,2)]			# first two columns
USArrests[,c("Murder", "Assault")]	# Murder and assault
USArrests[[2]]				# the second column
USArrests[, "Murder"]  			# Murder column
USArrests[["Murder"]] 			# Murder column
USArrests$Murder			# Murder column
					# Different ways to get same result 


############################ Functions and logic  ############################
# you can use T as TRUE, and F as FALSE, but don't do that. 
# 

if (TRUE) {
  print("hello!")
}

mean(c(1, 2, 3, 4, 5, 6, 7, 8), trim = 0.2)
mean(c(1, 2, 3, 4, NA), na.rm = TRUE)

?mean
vector <- c(1,2,4,5,6)
if (is.na(mean(vector) )) {
  print("something went wrong, NA included")
}


vector <- c(1,2,4,5,6, NA)
if (is.na(mean(vector) )) {
  print("something went wrong, NA included")
}
mean
mean(vector, na.rm = TRUE)


mean.na <- function(x){
  # returns mean of a vector with NA
  x <- mean(x, na.rm = TRUE)
  return(x)
}
u <- c(1,2,3,4,NA)
my_mean <- mean.na(u)
my_mean
############################ Here starts our template  ############################
rm(list=ls());gc() 
# rm() is a function that remove from our space an element. ls() lists every object in our space
# together they list everything and remove them. gc() is a function that cleans the RAM. 
# you probably noticed the ';', is not a god practice, is for executing two orders 
# in the same line

if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(yarrr)
# pacman is a meta-package. It helps us to manage our packages
# usually we use install.packages("pacman") and then library(pacman).
# in this trick, we check if pacman is installed, if not, install it
# then load it with library() and use the function p_load from pacman to install 
# and load the required packages. More info:
?p_load

p_load("yarrr")
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load( parallel, lattice, doBy, lme4, influence.ME , effects, lsmeans, reshape2, ez, arm, MASS, dplyr, ggplot2, psych, car , afex)


