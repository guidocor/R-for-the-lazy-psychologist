#### Script for learning to make some NHST ####
# First we clean the enviroment and load some packages  
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, yarrr, Rmisc, afex, MASS, psych, lsmeans, knitr)
options(scipen=20)
load("./data/discr_df_clean.RData")

df %>% str


df.means <- df %>% group_by(., subject_nr, PT, stim_type)%>% 
  dplyr::summarise(., mean_acc = mean(correct, trimm = 0.25))
df.means
df.means %>% group_by(PT, stim_type) %>% dplyr::summarise(m = mean(mean_acc))

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

df.means$pt_stim <- paste(df.means$PT, df.means$stim_type)

get_shapiro(df.means$mean_acc, df.means$pt_stim)

aov.1 <- aov_ez(id = "subject_nr", 
                dv = "mean_acc" , 
                within = c("stim_type", "PT"), 
                data = df.means)
aov.1
# Sphericity correction method: GG 

aov.1 %>% nice
aov.1 %>% nice %>% kable



ls <- lsmeans::lsmeans(aov.1, specs = c("PT", "stim_type"))

pairs(ls, adjust = "holm") %>% summary %>%   kable


save(ls, aov.1, file = "./data/anova.Rdata")
