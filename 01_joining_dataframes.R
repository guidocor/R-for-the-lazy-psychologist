#### Some annotations required for the 10_join_data.R file 
# first of all. Make sure that we have installed the packages 
# required for this session 
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, data.table, gdata)

#### Previous session we learnt  ####
# - Making basic operations
# - Making subsets of data.frames
# - Basic logic 
# - Making basic functions
 

are_we_ready <- FALSE # Don't change this until required :P

if(are_we_ready){
  cat("Go for it!")
} else {
 cat("Ok \t turn are_we_ready to TRUE in previous lines ;)") 
}

str(mtcars)
mtcars[["mpg"]]
df  <- mtcars[, c("mpg", "cyl", "hp")]
head(df)

# We can play with, like sum 100 to the hp column
df$hp_sum_100 <- df$hp + 100
df$hp
df$hp_sum_100

# Or we can do more useful things like centering
# This operation is useful 
# We create a new column with the original column 
# substracted the mean 
df$mpg_centered <- df$mpg - mean(df$mpg)
head(df)
df$mpg_centered 
# Look in visual summary and some descriptives provioded by summary()
hist(df$mpg_centered)
summary(df$mpg_centered)
summary(df$mpg)
summary(df)

# Also we know how to do basic functions
# look at this. It just 

print_my_name_ready <- function(my_name , ready = TRUE){
  if(is.character(my_name) & ready == TRUE){
    # In this case we use cat instead of print
    # this is because cat is easier to handle
    # it concatenates the given objects
    cat(my_name, " is ready! ")
  } else {
    cat("Something went wrong, not ready yet")
  }
}

print_my_name_ready("Guido", ready = FALSE)
print_my_name_ready(12)
print_my_name_ready("Guido")



#### In the last session we will learn ####
# - Loopings 
# - Use files
# - Using function to read and write files

# A loop is a structure wich takes the following schema
list_of_elements <- c("hola", "que", "tal")
for (element in list_of_elements){
  print(element)
}
for (number in 1:10){
   print(number*number)
}
# In short: walks trhough (iterates) the list of elements creating
# and pass this element to the {} where you can do things 
# Side note: loop are really slow on R. 
# Use it with caution or try using apply functions

# AVOID using "setwd" function. Instead, use Rprojects
# setwd("~/Dropbox")

# Listing files 
list.files()
?list.files


# Bind dataframes
# rbind() and cbind()
# Row binds 
# Column binds
# Must have same dimentions! check them with dim()

dim(mtcars)
head(mtcars)
str(mtcars)
str(df)
head(cbind(mtcars, df))
df.2 <- cbind(mtcars, df)
head(df.2)

rbind(mtcars, df)

read.csv()
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(readxl)
discr_demo <- read_excel("./raw/discr_demo.xlsx")
View(discr_demo)

read.xls() #gdata
write.csv(mtcars, file = "mtcars.csv")

