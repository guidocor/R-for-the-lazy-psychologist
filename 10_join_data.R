#### Script for JOINING DATA ####
# Here we will take the data from our folder raw and we will pass it 
# the folder data. Our aim is to have a nice dataset in one file

# First we clean the enviroment and load some packages  
rm(list=ls());gc()
# WARING! This line will install packages
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, data.table, gdata)

# my files to join are located in the raw folder
# Notice: 
# You can change your pattern and set to different names 
# this pattern makes the list.files function to just look
# at the files that follow the aforementioned pattern. 
# Also, notice that the "raw" folder starts with a dor
# this dot means: "inside our current folder, this is 
# the folder (raw) that I want you to look in. 
my.files <- list.files(path = "./raw/", pattern="subject")
my.files

# Our strategy is the following
# 1) List determined files in a folder
# 2) Create an empty data.frame object (df0)
# 3) Make a loop that walks into filenames of my.file list
# 4) Read the data and join 
df0 <- data.frame()
for (filename in my.files){
  # the names of the columns that we want to hold : ) 
  cols = c("subject_nr", "SOA",  "correct", "id", "stim_type", "response_time", "block_id")
  # Notice the use of fread from data.table package, is faster than alternatives like read.csv
  df <- fread(paste0("./raw/", filename), select =  cols, data.table = FALSE)
  
  # Bind the rows (rbind) of our previous data frame with the new information
  df0 <- rbind(df0,df) 
  rm(df) # delete old data.frame 
}

# !!! Attention: for loops in R are quite slow. As this is an introduction 
# !!! I tried to keep it simple. But if you are trying to read huge files 
# !!! or a big bunch of files is better to do that via data.table package
# !!! df.list <- list.files(pattern = "subject",path = where_are_the_files)
# !!! files <- paste0(where_are_the_files, df.list)
# !!! system.time({df1 <- lapply(files, fread)
# !!! df1 <- do.call(rbind, df1)})

rm(cols) # delete the columns object
dim(df0)
head(df0)
str(df0)

# Now we want to join a xls trhought the package gdata
??gdata

dat <- read.xls("./raw/discr_demo.xlsx")

library(readxl)
dat <- read_excel("~/Dropbox/R_course/raw/discr_demo.xlsx")

head(dat)
str(dat)
# look how subject_nr is parsed as int (integer)
# This will give troubles when joining...
dat$subject_nr <- as.character(dat$subject_nr)


# Join the two data.frames with left_join from dplyr package
# What does it make? 
# "left_join(x, y) includes all observations in x, regardless of whether 
# they match or not. This is the most commonly used join because it ensures 
# that you don’t lose observations from your primary table."
# More about joining with dplyr: https://cran.r-project.org/web/packages/dplyr/vignettes/two-table.html
df0 <- left_join(df0, dat)
df0$subject_nr <- as.character(df0$subject_nr)
length(unique(df0$subject_nr) )
head(df0)
str(df0)

# check if there is any na in the df
df0[is.na(df0$gender),]

# changing a column with incorrect name
df0$PT <- df0$SOA; df0$SOA <- rm()
 
# now we will write some csv data :)
write.csv(df0, file="./data/raw_matrix_disc.csv")
data.table::fwrite(df0, file = "./data/raw_matrix_disc.csv")
