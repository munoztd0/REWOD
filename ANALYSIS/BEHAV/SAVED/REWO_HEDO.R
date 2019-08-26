## R code for FOR REWOD_HED
# last modified on Nov 2018 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }
# 
# # usage
# packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
# ipak(packages)
# 
# rm(list = ls())

# load library:

library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(plyr)
library(influence.ME)
library(plotrix)
library(car) #checkout
#library(Rmisc)
#library(doBy)
#library(afex)
#library(sjstats)
#library(gamlss)
#library(mvoutlier)
#library(HH)
#library(multcomp)
#library(pbkrtest)


#SETUP

# Set working directory
analysis_path <- '~/rewod/DATABASES/'# for this to work the script needs to be sourced
setwd(analysis_path)

# open dataset (session two only)
REWOD_HED <- read.delim(file.path(analysis_path,'REWOD_HEDONIC_ses_second.txt'), header = T, sep ='') # read in dataset

# define factors
REWOD_HED$id               <- factor(REWOD_HED$id)
REWOD_HED$trial            <- factor(REWOD_HED$trial)
REWOD_HED$session          <- factor(REWOD_HED$session)
REWOD_HED$condition        <- factor(REWOD_HED$condition)

## remove sub 1 & 8 
REWOD_HED <- filter(REWOD_HED, id != "1" & id != "8")

# PLOTS

##plot (non-averaged per participant) 

# # liking boxplot by condition
# boxplot(REWOD_HED$perceived_liking ~ REWOD_HED$condition, las = 1)
# 
# # liking boxplot by time
# boxplot(REWOD_HED$perceived_liking ~ REWOD_HED$trialxcondition, las = 1)
# 
# # intensity boxplot by condition
# boxplot(REWOD_HED$perceived_intensity ~ REWOD_HED$condition, las = 1)
# 
# # intensity boxplot by time
# boxplot(REWOD_HED$perceived_intensity ~ REWOD_HED$trialxcondition, las = 1)



#---------------LIKING-------------#
# get means by condition
lik.bc = ddply(REWOD_HED, .(condition), summarise,  perceived_liking = mean(perceived_liking, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))
# get means by condition and trial
lik.bct = ddply(REWOD_HED, .(condition, trial), summarise,  perceived_liking = mean(perceived_liking, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))

# get means by participant
lik.bs = ddply(REWOD_HED, .(id, trial), summarise, perceived_liking = mean(perceived_liking, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))

lik.bsc = ddply(REWOD_HED, .(id, condition), summarise,  liking = mean(perceived_liking, na.rm = TRUE))
lik.bsc.choc = lik.bsc %>% filter(condition == 'chocolate')
lik.bsc.neutral = lik.bsc %>% filter(condition == 'neutral')
lik.bsc.empty = lik.bsc %>% filter(condition == 'empty')

ggplot(lik.bsc.empty, aes(as.factor(id), liking)) +
  geom_col(fill = 'black') +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "Control liking by subject",
    x = "Subject",
    y = "Mean liking"
  )

ggplot(lik.bsc.neutral, aes(as.factor(id), liking)) +
  geom_col(fill = 'red') +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "Neutral liking by subject",
    x = "Subject",
    y = "Mean liking"
  )

ggplot(lik.bsc.choc, aes(as.factor(id), liking)) +
  geom_col(fill = 'blue') +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "Reward liking by subject",
    x = "Subject",
    y = "Mean liking"
  )

# ##plot n_grips to see by conditions
ggplot(lik.bsc , aes(reorder(id, -liking), y =  liking, color=condition)) +
  geom_point() +
  geom_line(aes(group = condition)) +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "liking by subject by condition",
    x = "Subject reordered",
    y = "Mean liking"
  )

plot(density(lik.bsc.choc$liking));
plot(density(lik.bsc.neutral$liking));
plot(density(lik.bsc.empty$liking));

# library("RNOmni")
# rankNorm()


#_________INTENSITY_______#

# get means by condition
int.bt = ddply(REWOD_HED, .(condition), summarise,  perceived_intensity = mean(perceived_intensity, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))
# get means by condition and trial
int.bct = ddply(REWOD_HED, .(condition, trial), summarise,  perceived_intensity = mean(perceived_intensity, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))

# get means by participant
int.bs = ddply(REWOD_HED, .(id, trial), summarise, perceived_intensity = mean(perceived_intensity, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))

# get means by sub & condition
int.bsc = ddply(REWOD_HED, .(id, condition), summarise,  intensity = mean(perceived_intensity, na.rm = TRUE))
int.bsc.choc = int.bsc %>% filter(condition == 'chocolate')
int.bsc.neutral = int.bsc %>% filter(condition == 'neutral')
int.bsc.empty = int.bsc %>% filter(condition == 'empty')


ggplot(int.bsc.empty, aes(as.factor(id), intensity)) +
  geom_col(fill = 'black') +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "Control intensity by subject",
    x = "Subject",
    y = "Mean intensity"
  )

ggplot(int.bsc.neutral, aes(as.factor(id), intensity)) +
  geom_col(fill = 'red') +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "Neutral intensity by subject",
    x = "Subject",
    y = "Mean intensity"
  )

ggplot(int.bsc.choc, aes(as.factor(id), intensity)) +
  geom_col(fill = 'blue') +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "Reward intensity by subject",
    x = "Subject",
    y = "Mean intensity"
  )

# ##plot n_grips to see by conditions
ggplot(int.bsc , aes(reorder(id, -intensity), y =  intensity, color=condition)) +
  geom_point() +
  geom_line(aes(group = condition)) +
  theme_bw() +
  scale_y_continuous(limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(
    title = "Intensity by subject by condition",
    x = "Subject reordered",
    y = "Mean intensity"
  )

plot(density(int.bsc.choc$intensity));
plot(density(int.bsc.neutral$intensity));
plot(density(int.bsc.empty$intensity));

# library("RNOmni")
# rankNorm()


## plot overall effect Liking

# perceived_liking average per trialxcondition and id
#boxplot(bs$perceived_liking ~ bs$trialxcondition, las = 1)

# 
# #plot perceived_liking to see the trajectory of learning ((overall average by trialxconditions)
# ggplot(bt, aes(x = trialxcondition, y = perceived_liking, fill = I('royalblue1'), color = I('royalblue4'))) +
#   geom_point() + geom_line(group=1) +
#   guides(color = "none", fill = "none") +
#   guides(color = "none", fill = "none") +
#   theme_bw() +
#   labs(
#     title = "Liking By Time",
#     x = "trialxcondition",
#     y = "perceived_liking"
#   )
# 
# #OR different representation
# ggplotRegression <- function (fit) {
#   
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point() +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        "Intercept =",signif(fit$coef[[1]],5 ),
#                        " Slope =",signif(fit$coef[[2]], 5),
#                        " P =",signif(summary(fit)$coef[2,4], 5)))
# }

# # plot perceived_likings by time with regression lign
# ggplotRegression(lm(perceived_liking ~ trialxcondition, data = bt))
# 
# #plot liking to see the trajectory of learning (by condition) 
# ggplot(bct, aes(x = trialxcondition, y = perceived_liking, color = condition)) +
#   geom_point() +
#   geom_line(aes(group = condition), alpha = .3, size = 1) +
#   scale_colour_manual("", 
#                       values = c("chocolate"="green", "empty"="red", "neutral"="blue")) +
#   theme_bw() +
#   labs(
#     title = "Liking By Time By condition",
#     x = "trialxcondition",
#     y = "perceived_liking"
#   )




# plot liking by time by condition with regression lign
ggplotRegression(lm(perceived_liking ~ trialxcondition*condition, data = bct)) + 
  facet_wrap(~condition)


## plot overall effect Intensity

# perceived_intensity average per trialxcondition and id
boxplot(bs$perceived_intensity ~ bs$trialxcondition, las = 1)

# #plot perceived_intensity to see the trajectory of learning ((overall average by trialxconditions)
# ggplot(bt, aes(x = trialxcondition, y = perceived_intensity, fill = I('royalblue1'), color = I('royalblue4'))) +
#   geom_point() + geom_line(group=1) +
#   guides(color = "none", fill = "none") +
#   guides(color = "none", fill = "none") +
#   theme_bw() +
#   labs(
#     title = "intensity By Time",
#     x = "trialxcondition",
#     y = "perceived_intensity"
#   )
# 
# # plot perceived_likings by time with regression lign
# ggplotRegression(lm(perceived_intensity ~ trialxcondition, data = bt))
# 
# #plot intensity to see the trajectory of learning (by condition) 
# ggplot(bct, aes(x = trialxcondition, y = perceived_intensity, color = condition)) +
#   geom_point() +
#   geom_line(aes(group = condition), alpha = .3, size = 1) +
#   scale_colour_manual("", 
#                       values = c("chocolate"="green", "empty"="red", "neutral"="blue")) +
#   theme_bw() +
#   labs(
#     title = "intensity By Time By condition",
#     x = "trialxcondition",
#     y = "perceived_intensity"
#   )

# # plot liking by time by condition with regression lign
# ggplotRegression(lm(perceived_intensity ~ trialxcondition*condition, data = bct)) + 
#   facet_wrap(~condition)
# 
# 
# REWOD_HED$Condition[REWOD_HED$condition== 'chocolate']     <- 'Chocolate'
# REWOD_HED$Condition[REWOD_HED$condition== 'empty']     <- 'Control'
# REWOD_HED$Condition[REWOD_HED$condition== 'neutral']     <- 'Neutral'

# # summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
# df1 <- summarySE(REWOD_HED, measurevar="perceived_liking", groupvars=c("trialxcondition", "Condition"))
# 
# ggplot(df1, aes(x = trialxcondition, y = perceived_liking, color=Condition)) +
#   geom_line(aes(linetype = Condition), alpha = .7, size = 1) +
#   geom_point() +
#   geom_errorbar(aes(ymax = perceived_liking + 0.5*se, ymin = perceived_liking - 0.5*se), width=0.1, alpha=0.7, size=0.4)+
#   scale_colour_manual(values = c("Chocolate"="blue", "Neutral"="red", "Control"="black")) +
#   scale_linetype_manual(values = c("Chocolate"="dashed", "Neutral"="twodash", "Control"="solid")) +
#   scale_x_continuous(breaks=c(1:18)) + 
#   scale_y_continuous(breaks=c(45,50,55,60,65,70,75)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
#   labs(
#   title = "Time Trajectory: Pleasantness  ",
#   x = "Trial",
#   y = "Liking Ratings"
#   )
#  
# ggplot(df, aes(x = trialxcondition, y = n_grips, color=Condition)) +
#   geom_line(aes(linetype = Condition), alpha = .7, size = 1) +
#   geom_point() +
#   geom_errorbar(aes(ymax = n_grips + 0.5*se, ymin = n_grips - 0.5*se), width=0.1, alpha=0.7, size=0.4)+


# ANALYSIS

REWOD_HED$id               <- factor(REWOD_HED$id)
REWOD_HED$condition       <- factor(REWOD_HED$condition)

#removing empty condition
REWOD_HED.woemp <- filter(REWOD_HED, condition != "empty")

#Assumptions:
my.model= lmer(perceived_liking ~ condition + (1|id) + (1|trial), data = REWOD_HED, REML = FALSE) #1+cvalue\id or 1\id
#1)Linearity (not good?)
plot(my.model)
#2) Absence of collinearity
#3)Homoscedasticity AND #4)Normality of residuals
qqnorm(residuals(my.model))
#5) Absence of influential data points (less visible butneed to check) 
alt.est.id <- influence(model=my.model, group="id")
alt.est.trial <- influence(model=my.model, group="trial")
dfbetas(alt.est.id) #“too much” is defined by the 2/sqrt(n) = 0.408 OR 1 # not too bad
dfbetas(alt.est.trial)


##contrasts

REWOD_HED$cvalue[REWOD_HED$condition== 'chocolate']     <- 2
REWOD_HED$cvalue[REWOD_HED$condition== 'empty']     <- -1
REWOD_HED$cvalue[REWOD_HED$condition== 'neutral']     <- -1
REWOD_HED$cvalue       <- factor(REWOD_HED$cvalue)


##1. Liking: do participants prefer to the reward (chocolate) condition? 

# lmer analyis ~ cvalue 
main.liking = lmer(perceived_liking ~ cvalue + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
anova(main.liking)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_liking ~ cvalue + Error(id / (cvalue)), data = REWOD_HED))

# model comparison
main.liking.0 = lmer(perceived_liking ~ (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
anova(main.liking.0, main.liking, test = 'Chisq')
#sentence => main.liking is signifincatly better than the null model

# lmer analyis cvalue and trialxcondition 
main.liking.1 = lmer(perceived_liking ~ cvalue + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
anova(main.liking.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_liking ~ cvalue + trialxcondition + Error(id / (cvalue)), data = REWOD_HED))

# model comparison
anova(main.liking, main.liking.1, test = 'Chisq')
#sentence => main.liking1 is signifincatly better than main.liking (adding trialxcondition makes the model predict better)

# lmer analyis (+interaction) # should I have used the condition*trialxcondition variable instead?
main.liking.2 = lmer(perceived_liking ~ cvalue*trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)#  # second 1+ would need to be condition*trialxcondition or not?
anova(main.liking.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_liking ~ cvalue*trialxcondition + Error(id / (cvalue)), data = REWOD_HED))

# model comparison
anova(main.liking.1, main.liking.2, test = 'Chisq')
#sentence => main.liking2 is signifincatly better than main.liking1 (adding interaction helps the model predict better)


## 2. Intensity: do participants find the reward (chocolate) condition more intense?


# factorise trialxcondition
REWOD_HED$trialxcondition            <- factor(REWOD_HED$trialxcondition)

# lmer analyis ~ condition 
main.intensity = lmer(perceived_intensity ~ cvalue + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
anova(main.intensity)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_intensity ~ cvalue + Error(id / (cvalue)), data = REWOD_HED))

# model comparison
main.intensity.0 = lmer(perceived_intensity ~ (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
anova(main.intensity.0, main.intensity, test = 'Chisq')
#sentence => main.liking is signifincatly better than the null model

# lmer analyis condition and trialxcondition 
main.intensity.1 = lmer(perceived_intensity ~ cvalue + trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
anova(main.intensity.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_intensity ~ cvalue + trialxcondition + Error(id / (cvalue)), data = REWOD_HED))

# model comparison
anova(main.intensity, main.intensity.1, test = 'Chisq')
#sentence => main.liking1 is signifincatly better than main.liking (adding trialxcondition makes the model predict better)

# lmer analyis (+interaction) # should I have used the condition*trialxcondition variable instead?
main.intensity.2 = lmer(perceived_intensity ~ cvalue*trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)#  # second 1+ would need to be cvalue*trialxcondition or not?
anova(main.intensity.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_intensity ~ cvalue*trialxcondition + Error(id / (cvalue)), data = REWOD_HED))

# model comparison
anova(main.intensity.1, main.intensity.2, test = 'Chisq')
#sentence => HOWEVER here main.liking2 is NOT signifincatly better than main.liking1 (adding interaction DOESNT help the model predict better)



## 3. Specific test without empty  



#contrasts
REWOD_HED.woemp$cvalue[REWOD_HED.woemp$condition == 'chocolate']     <- 1
REWOD_HED.woemp$cvalue[REWOD_HED.woemp$condition == 'neutral']     <- -1
REWOD_HED.woemp$cvalue       <- factor(REWOD_HED.woemp$cvalue)


## 3.1. Liking: do participants prefer to the reward (chocolate) condition? 

# lmer analyis ~ condition 
main.liking = lmer(perceived_liking ~ cvalue + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)
anova(main.liking)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_liking ~ cvalue + Error(id / (cvalue)), data = REWOD_HED.woemp))

# model comparison
main.liking.0 = lmer(perceived_liking ~ (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)
anova(main.liking.0, main.liking, test = 'Chisq')
#sentence => main.liking is signifincatly better than the null model

# lmer analyis condition and trialxcondition 
main.liking.1 = lmer(perceived_liking ~ cvalue + trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)
anova(main.liking.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_liking ~ cvalue + trialxcondition + Error(id / (cvalue)), data = REWOD_HED.woemp))

# model comparison
anova(main.liking, main.liking.1, test = 'Chisq')
#sentence => main.liking1 is signifincatly better than main.liking (adding trialxcondition makes the model predict better)

# lmer analyis (+interaction) # should I have used the condition*trialxcondition variable instead?
main.liking.2 = lmer(perceived_liking ~ cvalue*trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)#  # second 1+ would need to be condition*trialxcondition or not?
anova(main.liking.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_liking ~ cvalue*trialxcondition + Error(id / (cvalue)), data = REWOD_HED.woemp))

# model comparison
anova(main.liking.1, main.liking.2, test = 'Chisq')
#sentence => main.liking2 is signifincatly better than main.liking1 (adding interaction helps the model predict better)


## 3.2. Intensity: do participants find the reward (chocolate) condition more intense? 


# lmer analyis ~ condition 
main.intensity = lmer(perceived_intensity ~ cvalue + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)
anova(main.intensity)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_intensity ~ cvalue + Error(id / (cvalue)), data = REWOD_HED.woemp))

# model comparison
main.intensity.0 = lmer(perceived_intensity ~ (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)
anova(main.intensity.0, main.intensity, test = 'Chisq')
#sentence => main.liking1 is signifincatly better than the null model

# lmer analyis condition and trialxcondition 
main.intensity.1 = lmer(perceived_intensity ~ cvalue + trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)
anova(main.intensity.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_intensity ~ cvalue + trialxcondition + Error(id / (cvalue)), data = REWOD_HED.woemp))

# model comparison
anova(main.intensity, main.intensity.1, test = 'Chisq')
#sentence => main.liking1 is signifincatly better than main.liking (adding trialxcondition makes the model predict better)

# lmer analyis (+interaction) # should I have used the condition*trialxcondition variable instead?
main.intensity.2 = lmer(perceived_intensity ~ cvalue*trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED.woemp, REML = FALSE)#  # second 1+ would need to be condition*trialxcondition or not?
anova(main.intensity.1)

# quick check with classical anova (! this is not reliable)
summary(aov(perceived_intensity ~ cvalue*trialxcondition + Error(id / (cvalue)), data = REWOD_HED.woemp))

# model comparison
anova(main.intensity.1, main.intensity.2, test = 'Chisq')
#sentence => HOWEVER here main.liking2 is NOT signifincatly better than main.liking1 (adding interaction DOESNT help the model predict better)



