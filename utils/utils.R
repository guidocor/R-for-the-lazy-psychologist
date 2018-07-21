### Functions to clean and read the data ####

clean_rt <- function(df, rt, subject, form_extra="", borders = "")
{
  # '@df = data.frame were execute the outlier detection
  # '@rt = string with the reaction time colname
  # '@form_extra = categories we want to split.
  # '@border = a vector with the upper and lower bounds, example: c("200", "4000") if we want to 
  #  delete rt lower than 200, and upper than 4000.
  if(form_extra != ""){
    function_to_apply = as.formula(paste(rt, "~", subject, "+", form_extra))
    key_of_joins = c(subject, form_extra)
  } else {
    function_to_apply = as.formula(paste(rt, "~", subject))
    key_of_joins = c(subject)
  }
  
  require(doBy)
  
  
  
  df <-data.frame(merge(df,
                        summaryBy(function_to_apply, data = df,
                                  FUN = function(x) { 
                                    # notice that changing the function
                                    # you can do other things
                                    c(lower = boxplot.stats(x)$stats[1],
                                      upper = boxplot.stats(x)$stats[5]) 
                                  }), key_of_joins))
  # Create two new response and rt variables to manipulate
  df$clean.rt<-df[, rt]
  lower_name = paste0(rt, ".lower")
  upper_name = paste0(rt, ".upper")
  
  
  # Remove from the new variables the extreme values -> Below Q1-1.5*IQR or Over Q3+1.5*IQR
  df$clean.rt[
    which(df[,rt] <= df[, lower_name] | df[,rt] >= df[, upper_name])]<-NA
  if(borders!= ""){
    df$clean.rt[
      which(df[,rt] <= df[, lower_name] | df[,rt] >= df[, upper_name]) | df[,rt] <= border[1] | df[,rt] >= border[2] ]<-NA
  }  
  
  
  # Make the new rt variable numeric again
  df$clean.rt<-as.numeric(as.character(df$clean.rt))
  
  
  return(df)
  
}
message("clean_rt is ready!")

center_mean <- function(object, log_it = FALSE){
  
  # function that return the centered on mean 
  if(log_it == TRUE) return ( log(object) - mean(log(object), na.rm = TRUE) )
  if(log_it == FALSE) return( object - mean(object, na.rm = TRUE))
}

message("center_mean is ready!")

 

convert_factor <- function(df, columns){
  df[,columns] <- lapply(df[,columns], as.character)
  df[,columns] <- lapply(df[,columns], as.factor)
  return(df)
}

message("convert_factor ready!")
 

#'Automatically detects character/factor variables and gives a comprehensive summary 
#'@param df name of your data frame
#'@return Returns the summary data frame
#'@examples
#'data(iris)
#'charSummary(iris)
#'@export

charSummary <- function(df){
  
  num        <- vector(mode = "character")
  char       <- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]]) == "numeric") {
      num    <- c(num, names(df[var]))
    }else if (class(df[[var]]) == "factor" || class(df[[var]]) == "character") {
      char   <- c(char, names(df[var]))
    }
  }
  
  if (length(char)!=0){
    dfchar   <- subset(df, select=char)
    E        <- sapply(dfchar, function(x) as.character(x))
    EE       <- as.data.frame(E)
    n        <- as.data.frame(sapply(EE, function(x) sum(!is.na(x))))
    n        <- data.frame(n)
    colnames(n) <- "n"
    
    n1       <- nrow(df)

    #missing value computation
    miss     <- sapply(EE, function(x) sum(is.na(x)))
    miss     <- as.data.frame(miss)
    g3       <- cbind(n, miss)
    perc     <- (miss/n1)*100
    m3       <- cbind(g3, perc)
    colnames(m3)[ncol(m3)] <- "miss%"
    
    #top-5 level count
    topfivelevel <- function(x){
     tbl_x             <- table(x)
     topfive           <- sort(tbl_x, decreasing = TRUE)[1:ifelse(length(tbl_x) >= 5, yes = 5, no = length(tbl_x))]
     topfivelevelcount <- paste0(names(topfive), ":", topfive)
    }
    
    unique     <- sapply(EE, function(x) length(unique(x)))
    unique_val <- sapply(EE, function(x) paste0(topfivelevel(x), collapse = ", "))
    m4         <- cbind.data.frame(m3, unique, "top5levels:count" = unique_val)
    
    return(m4)
  }
}


#'Automatically detects numeric variables and gives a comprehensive summary 
#'@param df name of your data frame
#'@return Returns the summary data frame
#'@examples
#'data(iris)
#'numSummary(iris)
#'@export

numSummary <- function(df){
  
  num       <- vector(mode = "character")
  char      <- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]])=="numeric" || class(df[[var]])=="integer") {
      num   <- c(num,names(df[var]))
    }else if (class(df[[var]])=="factor" || class(df[[var]])=="character") {
      char  <- c(char,names(df[var]))
    }
  }
  
  dfnum     <- subset(df,select=num)
  D         <- sapply(dfnum, function(x) as.numeric(x,na.rm=TRUE))
  DD        <- as.data.frame(D)
  
  #kurtosis computation
  kurtosis <- function(x,na.rm = TRUE){
    if(na.rm){
      x     <- x[which(!is.na(x))]
    }
    x_mean  <- mean(x)
    x_count <- length(x)
    s2      <- sum((x-x_mean)^2)
    s4      <- sum((x-x_mean)^4)
    m2      <- s2/x_count
    m4      <- s4/x_count
    res     <- ((m4 / m2^2 - 3) + 3) * (1 - 1 / x_count)^2 - 3
  }
  
  #skewness calculation
  skewness <- function(x,na.rm = TRUE){
    if(na.rm){
      x     <- x[which(!is.na(x))]
    }
    x_mean  <- mean(x)
    x_count <- length(x)
    s2      <- sum((x-x_mean)^2)
    s3      <- sum((x-x_mean)^3)
    m2      <- s2/x_count
    m3      <- s3/x_count
    res     <- (m3 / m2^(3.0/2)) * (1 - 1 / x_count)^(3.0/2)
  }
  
  options(digits = 3)
  n                   <- sapply(DD, function(x) sum(!is.na(x)))
  mean                <- sapply(DD, function(x) mean(x,na.rm=TRUE))
  sd                  <- sapply(DD, function(x) sd(x,na.rm=TRUE))
  max                 <- sapply(DD, function(x) max(x,na.rm=TRUE))
  min                 <- sapply(DD, function(x) min(x,na.rm=TRUE))
  range               <- max - min
  nzero               <- sapply(DD, function(x) length(which(x == 0)))
  nunique             <- sapply(DD, function(x) length(unique(x)))
  outliersummary      <- t(sapply(DD, function(x) {
                                                        iqr          <- IQR(x,na.rm = TRUE,type = 4)
                                                        lowerbound   <- quantile(x,0.25,na.rm=TRUE)-(1.5*iqr)
                                                        upperbound   <- quantile(x,0.75,na.rm=TRUE)+(1.5*iqr)
                                                        noofoutliers <- length  (which(x > upperbound | x <lowerbound))
                                                        return(c(iqr,lowerbound,upperbound,noofoutliers))
                                                      }))
  kurtosis_val        <- sapply(DD, function(x) kurtosis(x))
  skewness_val        <- sapply(DD, function(x) skewness(x))
  d2                  <- cbind.data.frame(n,mean,sd,max,min,range,nunique,nzero,outliersummary,kurtosis_val,skewness_val)
  colnames(d2)        <- c("n","mean","sd","max","min","range","nunique","nzeros","iqr","lowerbound","upperbound","noutlier","kurtosis","skewness")
  
  #mode computation
  Mode <- function(x) {
    ux      <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  mode      <- sapply(dfnum, function(x) Mode(x) )
  mode      <- as.data.frame(mode)
  
  n1        <- nrow(dfnum)
  c1        <- ncol(dfnum)
  numb      <- rep(n1,c1)
  numb      <- data.frame(numb)
  
  #missing value computation
  miss      <- sapply(dfnum, function(x) sum(is.na(x)) )
  miss      <- as.data.frame(miss)
  d3        <- cbind(d2,mode,miss)
  missPer   <- (miss/n1)*100
  d3        <- cbind(d3,missPer)
  colnames(d3)[ncol(d3)] <- "miss%"
  
  #percentile value computation
  q         <- sapply(DD, function(x) quantile(x, c(.01,.05,.25,.5,.75,.95, .99),na.rm=TRUE) )

  q         <- as.data.frame(q)
  q         <- t(q)
  d3        <- cbind(d3,q)
  
  return(d3)
}


message("numSummary and charSummary ready!  ")

#---------------------------------------------------------------
#### INSTALL MY PACKAGES SNIPPET install.R ####
#---------------------------------------------------------------

# Script to detach all previous packages loaded, install those who you
# use and import them. Change the needed argument to install others or paste to your script
# the install_my_packages() fucntion and give other argument

#
# USEFUL:
# use this code to your source script to 
# rm(list=ls())
# setwd("~/home/project/")
# source("./utils/install.R")
# install_and_detach()
options(scipen=15)




check_part <- function(id, frame, part = "participante"){
  print(frame[frame$part == id,  ])
  
}

message("check_part ready!  ")

clean_rt <- function(df, rt, subject, form_extra="", borders = "")
{
  # '@df = data.frame were execute the outlier detection
  # '@rt = string with the reaction time colname
  # '@form_extra = categories we want to split.
  # '@border = a vector with the upper and lower bounds, example: c("200", "4000") if we want to 
  #  delete rt lower than 200, and upper than 4000.
  if(form_extra != ""){
    function_to_apply = as.formula(paste(rt, "~", subject, "+", form_extra))  
    } else {
    function_to_apply = as.formula(paste(rt, "~", subject))
    }
  
   require(doBy)
  
  
  
  df <-data.frame(merge(df,
          summaryBy(function_to_apply, data = df,
                        FUN = function(x) { 
                        # notice that changing the function
                        # you can do other things
                          c(lower = boxplot.stats(x)$stats[1],
                            upper = boxplot.stats(x)$stats[5]) 
                          }), c(subject)))
   # Create two new response and rt variables to manipulate
  df$clean.rt<-df[, rt]
  lower_name = paste0(rt, ".lower")
  upper_name = paste0(rt, ".upper")
  
 
  # Remove from the new variables the extreme values -> Below Q1-1.5*IQR or Over Q3+1.5*IQR
  df$clean.rt[
    which(df[,rt] <= df[, lower_name] | df[,rt] >= df[, upper_name])]<-NA
  if(borders!= ""){
    df$clean.rt[
      which(df[,rt] <= df[, lower_name] | df[,rt] >= df[, upper_name]) | df[,rt] <= border[1] | df[,rt] >= border[2] ]<-NA
  }  
  
  
  # Make the new rt variable numeric again
  df$clean.rt<-as.numeric(as.character(df$clean.rt))
   
 
  return(df)
  
}

message("clean_rt is ready!  ")