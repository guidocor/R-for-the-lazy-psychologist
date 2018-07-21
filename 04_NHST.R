#### Script for learning to make some NHST ####
# First we clean the enviroment and load some packages  
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, yarrr, Rmisc, afex, MASS, psych, lsmeans, knitr)
options(scipen=20) #disable scientific notation for numbers smaller than x (i.e., 10) digits (e.g., 4.312e+22)
data("anorexia") # load data (comes with afex) 
str(anorexia)
# 0. Prepare the data
# 1. Check assumptions
#   a) Normality (Shapiro - Wilk & qqplot )
#   b) Homogeneity (Levene)*
# 2. Make test
# 3. Report it 

# We are going to use the anorexia data 
str(anorexia)
anorexia$Treat
# remember how to subset data with the which and  %in% operator
df <- anorexia[ which(anorexia$Treat %in% c("CBT", "Cont")),]

# Shapiro test will check normality
# remember the use of the pipes %>%, or control + shit + M to make Rstudio hot key use
# In this way we can make readable and reusable code
df[ df$Treat == "CBT", "Prewt"] %>% shapiro.test()
df[ df$Treat == "Cont", "Prewt"] %>% shapiro.test()

# but look, the output is ugly. And we need to use the same code twice
# So what about making a function to it? 

# First look at how your data is shaped 
head(df)

# Treat Prewt Postwt
# 1  Cont  80.7   80.2
# 2  Cont  89.4   80.1
# 3  Cont  91.8   86.4

# We have a column with the treatment and two measures
# we want to extract the values of each sub-group and test them

# We need 
# the by function is a way to make operation spliting things
# it three arguments: 
# by(data, factorlist, function)
# note that the function have to be specified as an anonymous one 


output <- by(df$Prewt , df$Treat, function(x) shapiro.test(x))


# it do the job, but maybe we want to get things easier with a function that prints
# the result just for the paper

# we already know how to do functions. 
# lists are a type of object which stores elements
# similar than a vector, but in a list you can put different 
# types of elements, a data.frame, a vector, or even another list!
# they are created with list() function and filled
# example of list:

list()
my_list <- list(my_name = "Guido", my_Nobel_prizes = c(1,2,3), df = df)

my_list
my_list[1]
my_list[2]
my_list["my_name"]

my_list["car"] <- list(my_car = list(brand = "Nissan", type = "4x4"))

my_list$car$brand


get_shapiro <- function(df.dat, splits){
  cat_list <- list() # we are going to use the list to store the results
  splits <- factor(splits) # make sure it is a factor
  dat <- by(df.dat , splits , function(x) shapiro.test(x)) # apply our known function 'by'
  # in the folowing loop we will extract the data we want 
  # and cat them with the format we want 
  for(i in names(dat)){ 
  sub_dat <- dat[[i]]
  stat = sub_dat$statistic
  p = sub_dat$p.value
  shapiro <- paste0(i , ": W = ",  stat, ", p = ", p, "\n")
  cat(shapiro) 
  cat_list <- c(cat_list , shapiro)
  }
  return(cat_list)
}

u <- get_shapiro(df$Prewt , df$Treat)
u
# Now we want to check the normality 

by(df$Prewt , df$Treat, function(x) qqnorm(x))
 

# this by that 
t.test(Prewt ~ Treat , df)

t.test(df$Prewt, mu = 0)

# create an object 
t <- t.test(Prewt ~ Treat , df)
t

t$statistic
t$p.value


cat("t(", t$parameter,")", " = ", t$statistic, ", p = ", t$p.value, 
    ", 95% CI [", t$conf.int[1], ";", t$conf.int[2], "]", sep = "")

# We are going to use an ANOVA trough the package afex
# it provides wrapper functions to use in diferent ways
# afex calls other packages to compute the ANOVA
# we are going to use the aov_ez, but we can get same results 
# with aov_4, aov_car, aov_ez. 
# Use the one that fits better with you : ) 


data("ToothGrowth")
str(ToothGrowth)
tg <-  ToothGrowth # create a data.frame with easy name


# create identifiers for subjects
tg$id <- 1:nrow(tg)
aov.1 <- aov_ez(id = "id", dv = "len" , between = c("dose", "supp"), data = tg)
aov.1

# now we want a table : )
# we are going to use the nice function of afex package
# and kable from knitr
knitr::kable(nice(aov.1))


# follow up tests with lsmeans (least-square means)
# you can use the anova-afex object to make post-hoc test or get marginal means

ls.1 <- lsmeans::lsmeans(aov.1, specs = c("dose"), by = "supp")
ls.1  %>% plot

# pairwaise comparisons with holm correction
# many possibilities. Strongly recomend to read the reference guide
ls.1 %>% pairs(.,  adjust = "holm")

# other kind of plot
lsmip(ls.1 , dose ~ supp)

# different way of seeing the results
ls.2 <- lsmeans::lsmeans(aov.1, specs = c("dose", "supp"),  adjust = "holm")
ls.2 <- summary(ls.2)
ls.2
plot(ls.2 , main = "Dose by Supp", ylab = "Mean ", xlab = " ", horizontal = FALSE)
