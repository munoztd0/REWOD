## R code for FOR REWOD_HED
# last modified on Nov 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(influence.ME,lmerTest, lme4, MBESS, afex, car, ggplot2, dplyr, plyr, tidyr, reshape, Hmisc, Rmisc,  ggpubr, gridExtra, plotrix, lsmeans, BayesFactor)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}


#SETUP
task = 'HED'

# Set working directory
analysis_path <- file.path('~/REWOD/DERIVATIVES/BEHAV', task) 
setwd(analysis_path)

# open dataset (session two only)
REWOD_HED <- read.delim(file.path(analysis_path,'REWOD_HEDONIC_ses_second.txt'), header = T, sep ='') # read in dataset

# define factors
REWOD_HED$session          <- factor(REWOD_HED$session)
REWOD_HED$condition        <- factor(REWOD_HED$condition)

REWOD_HED$Condition[REWOD_HED$condition== 'chocolate']     <- 'Reward'
REWOD_HED$Condition[REWOD_HED$condition== 'empty']     <- 'Control'
REWOD_HED$Condition[REWOD_HED$condition== 'neutral']     <- 'Neutral'

## remove sub 1 & 8 
REWOD_HED <- filter(REWOD_HED,  id != "8")

# PLOTS


# get means by condition 
bt = ddply(REWOD_HED, .(trialxcondition), summarise,  perceived_liking = mean(perceived_liking, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE)) 
# get means by condition and trialxcondition
bct = ddply(REWOD_HED, .(condition, trialxcondition), summarise,  perceived_liking = mean(perceived_liking, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE)) 

# get means by participant 
bs = ddply(REWOD_HED, .(id, trialxcondition), summarise, perceived_liking = mean(perceived_liking, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))
bsLIK = ddply(REWOD_HED, .(id, Condition), summarise, perceived_liking = mean(perceived_liking, na.rm = TRUE), perceived_intensity = mean(perceived_intensity, na.rm = TRUE))



# functions ---------------------------------------------------------------


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Rˆ2* = ",signif(summary(fit)$adj.r.squared, 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}




# plots -------------------------------------------------------------------


# plot liking by time by condition with regression lign
ggplotRegression(lm(perceived_liking ~ trialxcondition*condition, data = bct)) + 
  facet_wrap(~condition)



#plot intensity to see the trajectory of learning (by condition) 
ggplot(bct, aes(x = trialxcondition, y = perceived_intensity, color = condition)) +
  geom_point() +
  geom_line(aes(group = condition), alpha = .3, size = 1) +
  scale_colour_manual("", 
                      values = c("chocolate"="green", "empty"="red", "neutral"="blue")) +
  theme_classic() +
  labs(
    title = "intensity By Time By condition",
    x = "trialxcondition",
    y = "perceived_intensity"
  )

# plot liking by time by condition with regression lign
ggplotRegression(lm(perceived_intensity ~ trialxcondition*condition, data = bct)) + 
  facet_wrap(~condition)




# get mean an SEM


dfLIK <- summarySE(REWOD_HED, measurevar="perceived_liking", groupvars=c("trialxcondition", "Condition"))
dfLIK$Condition = as.factor(dfLIK$Condition)
dfLIK$Condition = factor(dfLIK$Condition,levels(dfLIK$Condition)[c(3,2,1)])

ggplot(dfLIK, aes(x = trialxcondition, y = perceived_liking, color=Condition)) +
  geom_line(alpha = .7, size = 1, position =position_dodge(width = 0.5)) +
  geom_point(position =position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymax = perceived_liking +se, ymin = perceived_liking -se), width=0.5, alpha=0.7, size=0.4, position = position_dodge(width = 0.5))+
  scale_colour_manual(values = c("Reward"="blue", "Neutral"="red", "Control"="black")) +
  scale_y_continuous(expand = c(0, 0),  limits = c(40,80),  breaks=c(seq.int(40,80, by = 5))) +  #breaks = c(4.0, seq.int(5,16, by = 2.5)),
  scale_x_continuous(expand = c(0, 0), limits = c(0,19), breaks=c(0, seq.int(1,18, by = 2),19))+ 
  theme_classic() +
    theme(plot.margin = unit(c(1, 1, 1, 1), units = "cm"), axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(x = "Trials",y = "Pleasantness Ratings")

  # theme(text = element_text(size=rel(4)), plot.margin = unit(c(1, 1,0, 1), units = "cm"), 
  #       axis.title.x = element_text(size=16), axis.title.y = element_text(size=16)) +





# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
dfLIK2 <- summarySE(bsLIK, measurevar="perceived_liking", groupvars=c("Condition"))
dfLIK2$Condition <- as.factor(dfLIK2$Condition)
bsLIK$Condition <- as.factor(bsLIK$Condition)

dfLIK2$Condition = factor(dfLIK2$Condition,levels(dfLIK2$Condition)[c(3,2,1)])
bsLIK$Condition = factor(bsLIK$Condition,levels(bsLIK$Condition)[c(3,2,1)])  

ggplot(bsLIK, aes(x = Condition, y = perceived_liking, fill = Condition)) +
  geom_jitter(width = 0.05, color="black",alpha=0.5, size = 0.5) +
  geom_bar(data=dfLIK2, stat="identity", alpha=0.6, width=0.35, position = position_dodge(width = 0.01)) +
  scale_fill_manual("legend", values = c("Reward"="blue", "Neutral"="red", "Control"="black")) +
  geom_line(aes(x=Condition, y=perceived_liking, group=id), col="grey", alpha=0.4) +
  geom_errorbar(data=dfLIK2, aes(x = Condition, ymax = perceived_liking + se, ymin = perceived_liking - se), width=0.1, colour="black", alpha=1, size=0.4)+
  scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(0,100)) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 1, 1), units = "cm"),  axis.title.x = element_text(size=16), axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=16), legend.position = "none", axis.ticks.x = element_blank(), axis.line.x = element_line(color = "white")) +
  labs(
    x = "Odor Stimulus",
    y = "Plesantness Ratings"
  )

# 
# ggplot(dfPIT, aes(x = trialxcondition, y = n_grips, color=Condition)) +
#   geom_line(alpha = .7, size = 1) +
#   geom_point() +
#   geom_errorbar(aes(ymax = n_grips + 0.5*se, ymin = n_grips - 0.5*se), width=0.1, alpha=0.7, size=0.4)+
#   scale_colour_manual(values = c("CS+"="blue", "CS-"="red", "Baseline"="black")) +
#   scale_y_continuous(expand = c(0, 0),  limits = c(4.0,16)) +  #breaks = c(4.0, seq.int(5,16, by = 2.5)),
#   scale_x_continuous(expand = c(0, 0), limits = c(0,16), breaks=c(0, seq.int(1,15, by = 2),16))+ 
#   theme_classic() +
#   theme(plot.margin = unit(c(1, 1, 1, 1), units = "cm"), axis.title.x = element_text(size=16), 
#         axis.title.y = element_text(size=16), legend.position = c(0.9, 0.9), legend.title=element_blank()) +
#   labs(x = "Trials",y = "Number of Squeezes")


# ANALYSIS

REWOD_HED$id               <- factor(REWOD_HED$id)
REWOD_HED$condition       <- factor(REWOD_HED$condition)
REWOD_HED$trialxcondition           <- factor(REWOD_HED$trialxcondition)

#removing empty condition
REWOD_HED.woemp <- filter(REWOD_HED, condition != "empty")

#Assumptions:
my.model= lmer(perceived_liking ~ condition + (1|id) + (1|trialxcondition),  data = REWOD_HED) #1+cvalue\id or 1\id

#1)Linearity 
plot(my.model)
#2) Absence of collinearity
#3)Homoscedasticity AND #4)Normality of residuals
qqnorm(residuals(my.model))
#5) Absence of influential data points (less visible but need to check)
#alt.est.id <- influence(model=my.model, group="id")
#alt.est.trial <- influence(model=my.model, group="trialxcondition")




# LIKING ------------------------------------------------------------------


main.model.lik = lmer(perceived_liking ~ condition + trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)
summary(main.model.lik)

null.model.lik = lmer(perceived_liking ~ trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)

test = anova(main.model.lik, null.model.lik, test = 'Chisq')
test
#sentence => main.liking is 'signifincatly' better than the null model wihtout condition a fixe effect
# condition affected liking rating (χ2 (1)= 868.41, p<2.20×10ˆ-16), rising reward ratings by 17.63 points ± 0.57 (SEE) compared to neutral condition and,
# 17.63 ± 0.56 (SEE) compared to the control condition.

#Δ BIC = 847.92
delta_BIC = test$BIC[1] -test$BIC[2] 
delta_BIC

lsmeans(main.model.lik, pairwise ~ condition)

# planned contrast
REWOD_HED$cvalue[REWOD_HED$condition== 'chocolate']     <- 2
REWOD_HED$cvalue[REWOD_HED$condition== 'empty']     <- -1
REWOD_HED$cvalue[REWOD_HED$condition== 'neutral']     <- -1
REWOD_HED$cvalue       <- factor(REWOD_HED$cvalue)

#
main.cont.lik = lmer(perceived_liking ~ cvalue + trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)
summary(main.cont.lik)

null.cont.lik = lmer(perceived_liking ~ trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)

test2 = anova(main.cont.lik, null.cont.lik, test = 'Chisq')
test2
#sentence => main.liking is 'signifincatly' better than the null model wihtout condition a fixe effect
# condition affected liking rating (χ2 (1)= 866.73, p<2.20×10ˆ-16), rising reward ratings by 17.27 points ± 0.49 (SEE) compared to the other two conditions
#Δ BIC = 847.92
delta_BIC = test2$BIC[1] -test2$BIC[2] 
delta_BIC



#  contrast NEUTRAL - EMPTY "we play against ourselves by oding this contrast and being conservator"
REWOD_HED$cvalue1[REWOD_HED$condition== 'chocolate']     <- 0
REWOD_HED$cvalue1[REWOD_HED$condition== 'empty']     <- 1
REWOD_HED$cvalue1[REWOD_HED$condition== 'neutral']     <- -1
REWOD_HED$cvalue1       <- factor(REWOD_HED$cvalue1)

#
main.cont.lik1 = lmer(perceived_liking ~ cvalue1 + trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)
summary(main.cont.lik1)
#still not sign p = 0.19458 



# INTENSITY ---------------------------------------------------------------


main.model.int = lmer(perceived_intensity ~ condition + trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)
summary(main.model.int)

null.model.int = lmer(perceived_intensity ~ trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)

testint = anova(main.model.int, null.model.int, test = 'Chisq')
test
#sentence => main.intensity is 'signifincatly' better than the null model wihtout condition a fixe effect
# condition affected intensity rating (χ2 (1)= 868.41, p<2.20×10ˆ-16), rising reward ratings by 17.63 points ± 0.57 (SEE) compared to neutral condition and,
# 17.63 ± 0.56 (SEE) compared to the control condition.

#Δ BIC = XX
delta_BIC = test$BIC[1] -test$BIC[2] 
delta_BIC

lsmeans(main.model.int, pairwise ~ condition)

# planned contrast
REWOD_HED$cvalue[REWOD_HED$condition== 'chocolate']     <- 2
REWOD_HED$cvalue[REWOD_HED$condition== 'empty']     <- -1
REWOD_HED$cvalue[REWOD_HED$condition== 'neutral']     <- -1
REWOD_HED$cvalue       <- factor(REWOD_HED$cvalue)

#
main.cont.int = lmer(perceived_intensity ~ cvalue + trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)
summary(main.cont.int)

null.cont.int = lmer(perceived_intensity ~ trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)

testint2 = anova(main.cont.int, null.cont.int, test = 'Chisq')
testint2
#sentence => main.intensity is 'signifincatly' better than the null model without condition as fixed effect
# condition affected intensity rating (χ2 (1)= XX p<2.20×10ˆ-16), rising reward intensity ratings by XX points ± X.X (SEE) compared to the other two conditions
#Δ BIC = XX
delta_BIC = test2$BIC[1] -test2$BIC[2] 
delta_BIC


# 
# #  contrast NEUTRAL - EMPTY "we play against ourselves by oding this contrast and being conservator"
# REWOD_HED$cvalue1[REWOD_HED$condition== 'chocolate']     <- 0
# REWOD_HED$cvalue1[REWOD_HED$condition== 'empty']     <- 1
# REWOD_HED$cvalue1[REWOD_HED$condition== 'neutral']     <- -1
# REWOD_HED$cvalue1       <- factor(REWOD_HED$cvalue1)
# 
# #
# main.cont1 = lmer(perceived_intensity ~ cvalue1 + trialxcondition + (1|id), data = REWOD_HED, REML=FALSE)
# summary(main.cont1)

