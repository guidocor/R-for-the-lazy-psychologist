#### Script for CREATE A NICE PLOTS ####

# ! THIS PLOT IS DEPRECATED. 
# ! SOON I WILL UPLOAD A SIMPLE EASY GGPLOT ONE : ) 

# First we clean the enviroment and load some packages  
rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(dplyr, yarrr, Rmisc)
load("./data/discr_df_clean.RData")

df %>% str
df %>% head



# Means for each participant
df.means <- df %>% group_by(subject_nr, stim_type) %>% 
  dplyr::summarise(., mean_acc = mean(correct))
df.means
# using pirate plot package
pirateplot(mean_acc ~ stim_type, df.means, inf.method = "ci",
           point.o = 1, pal = "black", 
           bean.f.o = .1, inf.b.o = 1, inf.within = "subject_nr",
           inf.f.o = 0.5, inf.disp = "bean",
           main = "a type stimuli mean accuracy",
           ylab = "Accuracy", xlab = "Stim type"
           )


# We want to graph the difference between tasks. 
# We have a within measures design, so we remove the between participant variation
# with the  Morey method trough the Rmisc package 
# check http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
df.norm <- Rmisc::normDataWithin(df.means, idvar = "subject_nr", measurevar = "mean_acc")
df.norm %>% head

df.norm.means <- df.norm %>% group_by(stim_type) %>% 
  dplyr::summarise(mean = mean(mean_accNormed))
df.norm.means %>% head

# Based on https://www.r-bloggers.com/visualizing-small-scale-paired-data-combining-boxplots-stripcharts-and-confidence-intervals-in-r/
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)


stripchart(mean_acc ~ stim_type, data = df.norm,
           vertical=TRUE,
           pch=16,
           method="jitter", 
           main = "Fig. 1", ylab = " ",
           col = yarrr::transparent("black", trans.val = .7),
           ylim = c(0,1) 
           )


# our custom grid
# 0.5 point
lines(c(0, 6.7), c(0.5,0.5) , lty = 2,lwd=2)

for (i in c(seq(0.1, 0.9,.2))){
  
  lines(c(0,6.7), c(i,i), lwd=0.3, lty=2)
}
# y   x1y1  
# |
# |       x0y0
#     x____

# Now we want to place the means
s = 1:4 # how many means we have to place
m = 0.25 # how long has the mean tick to be
s - m

segments(x = s-m, 
         x1 = s+m, 
         y = df.norm.means$mean ,
         y1 = df.norm.means$mean, lwd = 4)
 
points(s , df.norm.means$mean, lwd = 13)

#?normDataWithin
?summarySEwithin 

ci <- Rmisc::summarySEwithin(df.means, measurevar = "mean_acc", 
                             withinvars = "stim_type", idvar = "subject_nr") 
ci %>% head
for (i in 1:nrow(ci)){
  ci.upper = ci[i,"mean_acc"] + ci[i,"ci"]
  ci.lower = ci[i,"mean_acc"] - ci[i,"ci"]
  rect(xleft = i - m, 
       ybottom = ci.lower,
       xright = i + m, 
       ytop = ci.upper, lwd = 2, col = yarrr::transparent("black", trans.val = .9))
  
  
}



# Save it!
p1() <-function(){
  # put here all the plto code
}

pdf("my_plot.pdf", width = 8, height =  4)
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
stripchart(mean_acc ~ stim_type, data = df.norm,
           vertical=TRUE,
           pch=16,
           method="jitter", 
           main = "Fig. 1", ylab = " ",
           col = yarrr::transparent("black", trans.val = .7),
           ylim = c(0,1) 
)


# our custom grid
# 0.5 point
lines(c(0, 6.7), c(0.5,0.5) , lty = 2,lwd=2)
for (i in c(seq(0.1, 0.9,.2))){
  
  lines(c(0,6.7), c(i,i), lwd=0.3, lty=2)
}
# y   x1y1  
# |
# |       x0y0
#     x____

# Now we want to place the means
s = 1:4 # how many means we have to place
m = 0.25 # how long has the mean tick to be
segments(x = s-m, 
         x1 = s+m, 
         y = df.norm.means$mean ,
         y1 = df.norm.means$mean, lwd = 4)

points(s , df.norm.means$mean, lwd = 13)

#normDataWithin
#?summarySEwithin 

ci <- summarySEwithin(df.means, measurevar = "mean_acc", withinvars = "stim_type", idvar = "subject_nr") 

for (i in 1:nrow(ci)){
  ci.upper = ci[i,"mean_acc"] + ci[i,"ci"]
  ci.lower = ci[i,"mean_acc"] - ci[i,"ci"]
  rect(xleft = i - m, 
       ybottom = ci.lower,
       xright = i + m, 
       ytop = ci.upper, lwd = 2, col = yarrr::transparent("black", trans.val = .9))
  
  
}
dev.off()









par(mfrow = c(2, 2))
for (element in unique(df$stim_type)){
  color =  gray(0, .5)
  color_secondary = gray(1, .8)
  x_lim = c(min(df$rt) ,max(df$rt))
  breaks = 30
  hist(df$rt[df$stim_type == element], col = color,
     main = " ", xlab = element , border = color , xlim = x_lim,
     breaks = breaks)
  hist(df[which(df$stim_type == element & df$correct == 0) ,"rt"], col = color_secondary,
       main = " ", xlab = element , border = color , xlim = x_lim,
       breaks = breaks, add = TRUE)
  }
 


layout(mat = matrix(c(5, 5, 1, 2, 3, 4), byrow = TRUE,
                    nrow = 3, 
                    ncol = 2),
       heights = c(0.3, 1, 1),    # Heights of the two rows
       widths = c(2, 2))     # Widths of the two columns

mat = matrix(c(5, 5, 1, 2, 3, 4), byrow = TRUE,
             nrow = 3, 
             ncol = 2)
mat
layout.show(5)
par(mar = c(3,3,3,3))
for (element in unique(df$stim_type)){
  color =  col = gray(0, .5)
  color_secondary = gray(1, .8)
  x_lim = c(min(df$rt) ,max(df$rt))
  breaks = 30
  hist(df$rt[df$stim_type == element], col = color,
       main = " ", xlab = element , border = color , xlim = x_lim,
       breaks = breaks)
  hist(df[which(df$stim_type == element & df$correct == 0) ,"rt"], col = color_secondary,
       main = " ", xlab = element , border = color , xlim = x_lim,
       breaks = breaks, add = TRUE)
}
par(mar = c(0,0,0,0))
plot(x=0,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1), axes = FALSE)
text(-.5,0,paste("Fig 1. Reaction times"), cex=2)
