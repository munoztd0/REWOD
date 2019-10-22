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



# #just anova BC HLM dont work really
HED.aov <- aov_car(perceived_liking ~ condition + Error(id/condition), data = REWOD_HED, anova_table = list(es = "pes"), fun_aggregate = mean)
HED.aov
HED.aov_sum <- summary(HED.aov)
HED.aov_sum


#else
mymodel = lmer(perceived_liking ~ condition + (1|id) + (1|trialxcondition), data = REWOD_HED)

lsmeans(mymodel, pairwise ~ condition)



## planned contrast
REWOD_HED$cont[REWOD_HED$condition== 'chocolate']     <- 'Reward'
REWOD_HED$cont[REWOD_HED$condition== 'empty']     <- 'No_Reward'
REWOD_HED$cont[REWOD_HED$condition== 'neutral']     <- 'No_Reward'
REWOD_HED$cont       <- factor(REWOD_HED$cont)

##

mymodel2 = lmer(perceived_liking ~ cont + (1|id) + (1|trialxcondition), data = REWOD_HED)

lsmeans(mymodel2, pairwise ~ cont)



# otherwise ---------------------------------------------------------------

##contrasts

REWOD_HED$cvalue[REWOD_HED$condition== 'chocolate']     <- 2
REWOD_HED$cvalue[REWOD_HED$condition== 'empty']     <- -1
REWOD_HED$cvalue[REWOD_HED$condition== 'neutral']     <- -1
REWOD_HED$cvalue       <- factor(REWOD_HED$cvalue)


#gamlss (for me this is the best way to check that the lmer do not depend on the distributions)
###summary(gamlss(ANT_DW_ins ~ Ccongr + re(random=~1+Ccongr|ID) + re(random=~1|trial), family = BEINF, data = na.omit(PIC.naomit)))



# 1. Liking: do participants prefer to the reward (chocolate) cond --------


# lmer analyis ~ cvalue 
main.liking = lmer(perceived_liking ~ cvalue + trialxcondition + (1|id), data = REWOD_HED, REML = FALSE)
summary(main.liking)
#sphericity ok\

plot(main.liking) #weird?

# quick check with classical anova (! this is not reliable)
#summary(aov(perceived_liking ~ cvalue, data = REWOD_HED))


# model comparison
main.liking.0 = lmer(perceived_liking ~ (1|id) + (1|), data = REWOD_HED, REML = FALSE)
test = anova(main.liking.0, main.liking, test = 'Chisq')
test
#sentence => main.liking is signifincatly better than the null model
# condition chocolate affected liking rating (χ2 (1)= 855.08, p<2.20×10-16), rising it by about 17.27  ± 0.49 (standard errors).

#Δ BIC = 847.92
delta_BIC = test$BIC[1] -test$BIC[2] 
delta_BIC


# 2. Intensity: do participants find the reward (chocolate) more intense --------


# lmer analyis ~ condition 
main.intensity = lmer(perceived_intensity ~ cvalue + (1|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
summary(main.intensity)

# quick check with classical anova (! this is not reliable)
#summary(aov(perceived_intensity ~ cvalue, data = REWOD_HED))

# model comparison
main.intensity.0 = lmer(perceived_intensity ~ (1|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
test2 = anova(main.intensity.0, main.intensity, test = 'Chisq')
test2
# condition chocolate affected intensity (χ2 (1)=340.74, p<2.20×10-16), rising it by about 20.41 ± 1.03 (standard errors).

#Δ BIC = 333.5726
delta_BIC = test2$BIC[1] -test2$BIC[2] # From BICs to Bayes factor
delta_BIC











# 
# # lmer analyis condition and trialxcondition 
# main.intensity.1 = lmer(perceived_intensity ~ cvalue + trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)
# anova(main.intensity.1)
# 
# # quick check with classical anova (! this is not reliable)
# summary(aov(perceived_intensity ~ cvalue + trialxcondition + Error(id / (cvalue)), data = REWOD_HED))
# 
# # model comparison
# anova(main.intensity, main.intensity.1, test = 'Chisq')
# #sentence => main.liking1 is signifincatly better than main.liking (adding trialxcondition makes the model predict better)
# 
# # lmer analyis (+interaction) # should I have used the condition*trialxcondition variable instead?
# main.intensity.2 = lmer(perceived_intensity ~ cvalue*trialxcondition + (1+cvalue|id) + (1|trialxcondition), data = REWOD_HED, REML = FALSE)#  # second 1+ would need to be cvalue*trialxcondition or not?
# anova(main.intensity.1)
# 
# # quick check with classical anova (! this is not reliable)
# summary(aov(perceived_intensity ~ cvalue*trialxcondition + Error(id / (cvalue)), data = REWOD_HED))
# 
# # model comparison
# anova(main.intensity.1, main.intensity.2, test = 'Chisq')
# #sentence => HOWEVER here main.liking2 is NOT signifincatly better than main.liking1 (adding interaction DOESNT help the model predict better)



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
main.liking.0 = lmer(perceived_liking ~ (1|id) + (1|trial), data = REWOD_HED.woemp, REML = FALSE)
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



