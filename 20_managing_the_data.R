#### Script for CREATE A NICE DATASET ####
# We created a huge dataset with every trial of our experiment
# Now is time to 
# - Create summaries of the data
# - Manage this summaries 
# First we clean the enviroment and load some packages  
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, reshape2, data.table, gdata, doBy)
# call our data
df <- fread("./data/raw_matrix_disc.csv")
str(df)

# welcome to the pipes! 
df %>% head
head(df)
# Pipes are a functional programming tool adressed to concatenate a lot of functions
# It takes the object and passes to the function next to it
# object %>% function
# hint you can type the pipe with control + shift + m 
# try it  : ) 
# %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 
#### Prepare columns ####

# check how many trials there are for each
xtabs(~ subject_nr + stim_type, df)

# renaming columns
# notice that "dplyr::"
# this is the way to tell R to use the function form the package
# package::function. Really useful when working with many packages that
# have the same function name 
df <- dplyr::rename(df, rt = response_time)
df %>% head


# checking how many block_id items are in the df
df$block_id %>% unique
df$subject_nr %>% unique %>% length

# Exclude the practice trials
practice <- c("practice", "urprc")

df <- filter(df, !block_id %in% practice ) # Notice the ! operator. 
# also notice that we are using the %in% operator. Really powerful!
# What we are doing is "filter by the block_id that AREN'T in practice" 
# with pipes it would be: df <- df %>% filter(., !block_id %in% practice)

xtabs(~ subject_nr + PT , df)

# Also we want to join the d and e tipe of stimuli 
# two strategies: mutating with deplyr and switching with ifelse, or subsetting
df <- mutate(df, stim_type = ifelse(stim_type %in% c("d", "e"), "de", stim_type) )
# ifelse(check this statement , is true  , convert to that)
df$stim_type[which(df$stim_type %in% c("d", "e"))] <- "de"

# An alternative way of doing that is via dplyr and case_when
# df <- mutate(df, stim_type = case_when(stim_type %in% c("d", "e") ~ "de"))
# more info: https://www.rdocumentation.org/packages/dplyr/versions/0.7.6/topics/case_when

# Now we are going to use groups and summarise to get some summary from the dataset
df <- df %>% dplyr::group_by(., subject_nr, PT, stim_type) %>%
  dplyr::summarise(., mean_correct = mean(correct), sum_correct = sum(correct), trials = n()) %>% 
  left_join( df, .)
head(df)

#### Removing Outliers ####
source("./utils/utils.R")


# Custom function, we are not going to enter in details by now.
clean_rt
# returns a "clean.rt" column which have outliers (based on 1.5*IQR rule) marked as NA

df <- clean_rt(df, rt = "rt", subject = "subject_nr") 
df$clean.rt %>%  summary

trials_removed <- data.frame(
              total = sum(is.na(df$clean.rt)), 
              percentage = (sum(is.na(df$clean.rt))/length(df$clean.rt)*100))

trials_removed
# df %>% group_by(., subject_nr) %>% dplyr::summarise(., )

# save a complete dataset and clean the NA 
df.complete <- df
df <- filter(df, !is.na(clean.rt))

#### Converting data from long to wide ####
df.means.rt <- df %>% dplyr::group_by(., subject_nr, correct, PT, stim_type) %>% 
  dplyr::summarise(., median_rt = median(rt))
# "subject" and "sex" are columns we want to keep the same
# "condition" is the column that contains the names of the new column to put things in
# "measurement" holds the measurements
df.means.rt <- dcast(df.means.rt, subject_nr ~ stim_type + PT + correct, value.var = "median_rt")
df.means.rt %>% head

df.means <- df %>% dplyr::group_by(., subject_nr, PT , stim_type) %>% 
  dplyr::summarise(., mean_correct = mean(correct))
df.means <- dcast(df.means, subject_nr ~ stim_type + PT, value.var = "mean_correct")

df.summary <- left_join(df.means, df.means.rt)


save(df, df.complete, trials_removed, file = "./data/discr_df_clean.RData")
