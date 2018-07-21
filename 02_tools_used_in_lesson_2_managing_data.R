# We have read the dplyr tutorial. Well we now know how to handle data quick
# Remember that dplyr uses an strategy:
# make special data.frames called tibbles and pass
# it to functions (verbs) with (or withour) the %>% operator 
# And uses verbs:
# mutare, summarise, group_by, filter..
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, reshape2, data.table, gdata)


# New operators!
# the '!' is the negation operator in R
# it will turn TRUE things in FALSE and vice versa
TRUE
!TRUE
!FALSE

is.na(NA) # gives TURE
!is.na(NA) # gives FALSE
is.na(1) # gives... ; )
!is.na(1) # gives .... ; ) 

# Also there is a %in% operator
# the %in% operator will check if the left part is in the roght part
# left %in% right
c("Guido", "B", "Corradi") %in% c("Guido", "is", "giving", "this", "lecture")
c(1,2,3, 4) %in% c(1,1,1,1,1,1,2,2,2,2,2,3,3,3,3)
!c(1,2,3, 4) %in% c(1,1,1,1,1,1,2,2,2,2,2,3,3,3,3)

# Hint: put on the left side the things you want to check and on the roght side those you 
# want to check 
all_participants <- c(1,1,2,2,3,4,4,5)
participants_that_I_want_to_hold <- c(1,3,4)

subset_participants <-  all_participants[all_participants %in% participants_that_I_want_to_hold]
subset_participants

# This will be useful for exlcluding and including participants, for example, see above
# (notice the use of which function: evaluates and returns the position of every TRUE 
# like 
head(mtcars)
summary(mtcars$hp )
which(mtcars$hp < 146.7)
mtcars[which(mtcars$hp < 146.7), ] 

# > mtcars[which(mtcars$hp < 130), ]
# mpg cyl  disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
&
|
m1 <- mtcars[which(mtcars$hp < 130 &  mtcars$carb %in% c(1,2)), ]
head(m1)

# > mtcars %>% head
# mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1

mtcars %>% group_by(mpg)


# > mtcars %>% group_by(mpg)
# Source: local data frame [32 x 11]
# Groups: mpg [25]
# 
# mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
# *  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1   21.0     6 160.0   110  3.90 2.620 16.46     0     1     4     4  

mtcars %>% group_by(mpg) %>% dplyr::summarise(., mean(drat))

means <- mtcars %>% group_by(mpg) %>% dplyr::summarise(., mean_drat = mean(drat))
means

# > mtcars %>% group_by(mpg) %>% summarise(., mean(drat))
# # A tibble: 25 × 2
# mpg `mean(drat)`
# <dbl>        <dbl>
#  1   10.4        2.965
#  2   13.3        3.730

# A special function from dplyr for counting cases
#  with group and summarise 
# (use always summarise with dplyr::summarise in order to explicit that you want use the summarise function from dplyr)
mtcars %>% group_by(., cyl) %>%  dplyr::summarise(., total_cases = n() )

# > mtcars %>% group_by(., cyl) %>%  dplyr::summarise(., total_cases = n() )
# A tibble: 3 × 2
# cyl total_cases
# <dbl>       <int>
# 1     4          11
# 2     6           7
# 3     8          14


# Now we introduce the tables with the function xtabs
?xtabs
xtabs()
xtabs(mpg~cyl, mtcars)

# cyl
# 4     6     8 
# 293.3 138.2 211.4 

# And finally the dcast function from reshape2 package 
# Our ally converting from long to wide
# thus is, form one row per observation (long)
# to one participant per row
#  http://seananderson.ca/2013/10/19/reshape.html
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
?reshape2::dcast

# generate a data.frame to practice
df <- data.frame(part = rep(1:3,10), 
                 task = rep(c("a", "a", "a", "b","b","b"), 15 ),  measure =  rnorm(30))



# "part" is the column we want to keep the same
# "task" is the column that contains the names of the new column to put things in
# "measure" holds the measurements
# the formula is thing_I_want_in_row + other_thing_I_want_in_row ~ things_in_columns
# then we need 
df %>% head
df_wide <- dcast(df, part ~ task, value.var = "measure")
df_wide
# Aggregation function missing: defaulting to length
# > df_wide
# part  a  b
# 1    1 15 15
# 2    2 15 15
# 3    3 15 15

df_wide <- dcast(df, part ~ task, value.var = "measure", fun.aggregate = median)
df_wide
# > df_wide
# part          a          b
# 1    1 0.74800820  0.3036768
# 2    2 0.56791538 -0.1164119
# 3    3 0.02014229  0.3706928


