#### Script for learning to make some Linear mixed effects models ####
# First we clean the enviroment and load some packages  
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, yarrr, Rmisc, afex, MASS, psych, emmeans, knitr, effects)
options(scipen=20)

# Data taken and MODIFIED 
df <- read.csv("./data/dvf_data.csv")
df %>% head

# A good compendium of knowledge about LMER : http://www.lrdc.pitt.edu/maplelab/statistics.html

# Linear Mixed Models models the variation of a variable grouped in other 
# (commonly: items and participante)
# As quick rule of thumb:
# Fixed part: general effect // between participants
# Random part: effect of each unit (items, subjects) and will be in (effect | unit) // within participants


# First of all we want to look at the questions we want to answer with our data and models
# I'm interested in:
# 1) the relation between rt by hemifield style and sex and the random effects part
# 2) the relation between the response (0, 1) by hemifield style and sex and the random effects part

#  Procedure

# 1) Check that variables are coded correctly (factors as factors, numerics as numerics...)
# 2) Center numeric predictors 
# 3) Set the contrasts
# 4) Stablish the maximal model justified by the data 


# 1) Check that variables are coded correctly (factors as factors, numerics as numerics...)
# 2) Center numeric predictors:
# to get meaningfull estimates of the regression we must center all predictors 
# this is because the beta value is the expected value when predictor is zero. Is not meaningful nor possible
# to have a predicted age of zero in our experiments!

# 3) Set the contrasts:
# Factors must have contrasts. We will use the package MASS to set some of them 
# Constrast will set which kind of comparisons we want to make.
# You can set contrasts to any factor with 
# contrasts(df$factor) <- contr.... or instroducing a matrix yourself
# There are more types of constrast than seeing here. Common ones:
# - Treatment. If you don't specify other, is the default. Compares a reference level with all the other
# contrasts(df$factor) <- contr.treatment
# - Effect coding. When you want to see the effect of a binary variable (i.e. sex)
# 1 unit change in contrast IS the difference between factors 
# contrasts(df$factor) <- c(-0.5, 0.5)
# - Sum contrast. If you have two or more levels, it will compare each level with the mean of all others
# contrasts(df$factor) <- contr.sum
# - Helmert, orthogonal contrast. 
# Each level is compared to the mean of all previous one
# Use when categories are ordered  

# 4) Stablish the maximal model justified by the data 
# One well stablished way of avoiding type-I error (pregnant man error) is 
# to set the maximal model think about what could be estimated inside each participant
# i.e.  the effect of visual complexity of each trial in each participant. 
# Think about the experiment and set up all the clusterings possibles
# What is inside each variable?
# Cuation: some models will fail to converge due to really complicated structures 
# more things to estimate than data available. In this case we will have to look 
# wich is relevant and which not. 

str(df)
df$style <- as.factor(df$style)
df$sex <- as.factor(df$sex)
df$hemifield <- as.factor(df$hemifield)
df$rt.c <- df$rt - mean(df$rt)

contrasts(df$style)
contrasts(df$sex)
contrasts(df$hemifield)

contrasts(df$style) <- c(.5, -.5)
contrasts(df$sex)  <- c(.5, -.5)
contrasts(df$hemifield) <- c(.5, -.5)
contrasts(df$hemifield)
contrasts(df$sex) 
table(df$part , df$hemifield)
table(df$part , df$style)
table(df$part , df$sex)

table(df$stim, df$hemifield)
table(df$stim, df$style)
table(df$stim, df$sex)

# load("./data/models.Rdata") # I ran the models before the course because they take long long time: ) 
m.rt.1 <- lmer(rt ~ hemifield * style * sex + (hemifield * style | part) + 
               (hemifield+sex |stim), data = df)
m.rt.1 %>% summary

m.rt.2 <- lmer(rt ~ hemifield * style * sex + (hemifield * style | part) + 
                 (sex |stim), data = df)

m.rt.2 %>% summary
# quick plot to see the results via the effects package
m.rt.2 %>% allEffects %>% plot
 
# Check:
# Correlations among factors (high correlations [~.95] means a bad thing )
# Check the residuals (must been centered to 0 and normally spread)

# Now we know hos to fit our model with lme4, so we'll try to do with afex::mixed
# it will give us p-values based on Likehood Ratio Tests
# more on p values https://www.ssc.wisc.edu/sscc/pubs/MM/MM_TestEffects.html
# !!! it will take long time, go for a coffee, don't worry
m.response <- mixed(response ~ hemifield * style * sex + (hemifield * style | part) +
                    (hemifield*sex |stim), 
                    data = df, 
                    family = binomial(), method = "LRT", expand_re = TRUE)

m.response
m.response %>% summary

# Now we can do some post-hoc tests with least square means via emmeans package
# more info: https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
# save(list=ls(), file = "./data/models.Rdata")

# first we take the categorial variables for which we want the marginal means 
# via emmeans, then we use pairs to test hypothesis. After that, some ready made plots
em.style <- emmeans(m.rt.2, "style", type = "response")
plot(em.style)
em.hemi <- emmeans(m.rt.2, "hemifield", type = "response")
plot(em.hemi)

pairs.style <- pairs(em.style)
pairs.style
plot(pairs.style)

# The same but with interactions

em.int <- emmeans(m.rt.2, c("hemifield", "style"), type = "response", adjust = "holm")
em.int
plot(em.int)
pairs.int <- pairs(em.int)
pairs.int
plot(pairs.int)

# ! I'm working on plotting things, by the moment
# ! you can visit my web page where I uploaded some code
# ! http://corradi.info/index.php/2018/03/15/plotting-glmer-effects-with-afex-emmeans-in-ggplot2-and-base-r/
# ! soon will be added more code here 