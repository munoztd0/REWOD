## R code for FOR REWOD_PIT
# last modified on Nov 2018 by David

# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(influence.ME,lmerTest, lme4, MBESS, afex, car, ggplot2, dplyr, plyr, tidyr, reshape, Hmisc, Rmisc,  ggpubr, gridExtra, plotrix, lsmeans, BayesFactor)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}


#SETUP

#SETUP
task = 'PIT'

# Set working directory
analysis_path <- file.path('~/REWOD/DERIVATIVES/BEHAV', task) 
setwd(analysis_path)

# open dataset
REWOD_PIT <- read.delim(file.path(analysis_path,'REWOD_PIT_ses_second.txt'), header = T, sep ='') # read in dataset

## subsetting into 3 differents tasks
REWOD_PIT.all <- REWOD_PIT
REWOD_RIM <- subset (REWOD_PIT.all,task == 'Reminder') 
REWOD_PE <- subset (REWOD_PIT.all,task == 'Partial_Extinction') 
REWOD_PIT <- subset (REWOD_PIT.all,task == 'PIT') 


# define factors
REWOD_RIM$id               <- factor(REWOD_RIM$id)
REWOD_RIM$trial            <- factor(REWOD_RIM$trial)
REWOD_RIM$task              <- factor(REWOD_RIM$task)
REWOD_RIM$session          <- factor(REWOD_RIM$session)
REWOD_RIM$reward        <- factor(REWOD_RIM$reward)

REWOD_PE$id               <- factor(REWOD_PE$id)
REWOD_PE$trial            <- factor(REWOD_PE$trial)
REWOD_PE$task              <- factor(REWOD_PE$task)
REWOD_PE$session          <- factor(REWOD_PE$session)
REWOD_PE$reward        <- factor(REWOD_PE$reward)

REWOD_PIT$id               <- factor(REWOD_PIT$id)
REWOD_PIT$trial            <- factor(REWOD_PIT$trial)
REWOD_PIT$task              <- factor(REWOD_PIT$task)
REWOD_PIT$session          <- factor(REWOD_PIT$session)


# PLOTS
REWOD_PIT$Condition[REWOD_PIT$condition== 'CSplus']     <- 'CS+'
REWOD_PIT$Condition[REWOD_PIT$condition== 'CSminus']     <- 'CS-'
REWOD_PIT$Condition[REWOD_PIT$condition== 'Baseline']     <- 'Baseline'

REWOD_PIT$Condition <- as.factor(REWOD_PIT$Condition)
# # FUNCTIONS -------------------------------------------------------------


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


## plot overall effect
# get means by trialxcondition
RIM.bt = ddply(REWOD_RIM, .(trial), summarise,  n_grips = mean(n_grips, na.rm = TRUE)) 
PE.bt = ddply(REWOD_PE, .(trial), summarise,  n_grips = mean(n_grips, na.rm = TRUE)) 
PIT.bt = ddply(REWOD_PIT, .(trialxcondition), summarise,  n_grips = mean(n_grips, na.rm = TRUE)) 

# get means by condition
PIT.bc = ddply(REWOD_PIT, .(condition), summarise,  n_grips = mean(n_grips, na.rm = TRUE)) 

# get means by trial & condition
PIT.bct = ddply(REWOD_PIT, .(trialxcondition, condition), summarise,  n_grips = mean(n_grips, na.rm = TRUE)) 

# get means by participant 
RIM.bs = ddply(REWOD_RIM, .(id, trial), summarise, n_grips = mean(n_grips, na.rm = TRUE)) #not condition
PE.bs = ddply(REWOD_PE, .(id, trial), summarise, n_grips = mean(n_grips, na.rm = TRUE)) #not condition
PIT.bs = ddply(REWOD_PIT, .(id, Condition), summarise, n_grips = mean(n_grips, na.rm = TRUE)) 



# PLOTS -------------------------------------------------------------------



##plot n_grips to see the trajectory of learning (overall average by trials) by conditions
dfPIT <- summarySE(REWOD_PIT, measurevar="n_grips", groupvars=c("trialxcondition", "Condition"))



ggplot(dfPIT, aes(x = trialxcondition, y = n_grips, color=Condition)) +
  geom_line(alpha = .7, size = 1) +
  geom_point() +
  geom_errorbar(aes(ymax = n_grips + 0.5*se, ymin = n_grips - 0.5*se), width=0.1, alpha=0.7, size=0.4)+
  scale_colour_manual(values = c("CS+"="blue", "CS-"="red", "Baseline"="black")) +
  scale_y_continuous(breaks = c(4.0, seq.int(5,16, by = 2.5)), limits = c(4.0,16)) +
  scale_x_continuous(breaks=c(seq.int(-1,15, by = 2))) + 
  theme_classic() +
  theme(axis.title = element_text(size=12), 
        legend.position = c(0.9, 0.9), legend.title=element_blank()) +
  labs(x = "Trials",y = "Number of Squeezes")


#Bar plot (360 trial x condition)

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
dfPIT2 <- summarySE(REWOD_PIT, measurevar="n_grips", groupvars=c("condition"))
Condition <- c("Baseline", "CS-", "CS+")
dfPIT2 <- data.frame(dfPIT2, Condition)
dfPIT2$Condition = factor(dfPIT2$Condition,levels(dfPIT2$Condition)[c(3,2,1)])
PIT.bs$Condition = factor(PIT.bs$Condition,levels(PIT.bs$Condition)[c(3,2,1)])  

ggplot(PIT.bs, aes(x = Condition, y = n_grips)) +
    geom_jitter(width = 0.05, color="dark grey") +
    geom_bar(data=dfPIT2, stat="identity", fill="black", alpha=0.6, width=0.35, position = position_dodge(width = 0.01)) +
    geom_line(aes(x=Condition, y=n_grips, group=id), col="grey", alpha=0.4) +
    geom_errorbar(data=dfPIT2, aes(x = Condition, ymax = n_grips + se, ymin = n_grips - se), width=0.1, colour="black", alpha=1, size=0.6)+
    #scale_x_discrete(limits=c("Baseline","CS-","CS+"))+
    scale_y_continuous(breaks=c(0,5, 10, 15, 20,25))+
    theme_classic() +
    theme(axis.title = element_text(size=12), 
        legend.position = c(0.9, 0.9), legend.title=element_blank()) +
    labs(
      x = "Pavlovian Stimulus",
      y = "Number of Squeezes"
    )
  
  
  # ggplot(PIT.bs, aes(x = Condition, y = n_grips)) +
  #   geom_jitter(width = 0.05, color="black") +
  #   geom_bar(data=dfPIT2, stat="identity", fill="black", alpha=0.6, width=0.35, position = position_dodge(width = 0.01)) +
  #   geom_line(aes(x=Condition, y=n_grips, group=id), col="grey", alpha=0.4) +
  #   geom_errorbar(data=dfPIT2, aes(x = Condition, ymax = n_grips + se, ymin = n_grips - se), width=0.1, colour="black", alpha=1, size=0.4)+
  #   geom_segment(aes(x = 1.5, y = 13.5, xend = 3, yend = 13.5), size =0.4)+
  #   geom_segment(aes(x = 1, y = 10, xend = 2, yend = 10), size =0.4)+
  #   geom_segment(aes(x = 1.5, y = 10, xend = 1.5, yend = 13.5), size =0.4)+  
  #   geom_segment(aes(x = 1, y = 9.5, xend = 1, yend = 10), size =0.4)+  
  #   geom_segment(aes(x = 2, y = 9.5, xend = 2, yend = 10), size =0.4)+ 
  #   geom_segment(aes(x = 3, y = 13, xend = 3, yend = 13.5), size =0.4)+
  #   annotate("text", x = 2.25, y = 14, label = "***") +
  #   scale_x_discrete(limits=c("Baseline","CS-","CS+"))+
  #   scale_y_continuous(breaks=c(0,5, 10, 15, 20,25))+
  #   theme_classic() +
  #   theme(plot.title = element_text(hjust = 0.5, size=20,  face="bold"), axis.title.y = element_text(size=14), axis.text=element_text(size=14), legend.position = c(0.9, 0.85)) +
  #   labs(
  #     x = NULL,
  #     y = "Number of Squeezes"
  #   )

  
# ANALYSIS


## 1. number of grips: are participants gripping more on the CSplus condition? 

#factorise trial and condition
REWOD_PIT$trialxcondition            <- factor(REWOD_PIT$trial)
REWOD_PIT$condition       <- factor(REWOD_PIT$condition)
#Assumptions:
my.model <- lmer(n_grips ~ condition + (1|id) , data = REWOD_PIT, REML = FALSE)  #1+cvalue\id or 1\id
#1)Linearity (not good?)
plot(my.model)
#2) Absence of collinearity
#3)Homoscedasticity AND #4)Normality of residuals
qqnorm(residuals(my.model))
#5) Absence of influential data points (ID =4 and ID = 22 ??)
alt.est.id <- influence(model=my.model, group="id")
plot(dfbetas(alt.est.id), PIT.bs$id)



#just anova BC HLM dont work really
PIT.aov <- aov_car(n_grips ~ Condition + Error(id/Condition), data = REWOD_PIT, anova_table = list(es = "pes"))
PIT.aov
PIT.aov_sum <- summary(PIT.aov)
PIT.aov_sum


#----EFFECT SIZE

# Confidence interval
PIT.CSCategory_lims      <- conf.limits.ncf(F.value = PIT.aov_sum$univariate.tests[2,5], conf.level = .90, df.1 <- PIT.aov_sum$univariate.tests[2,2], df.2 <- PIT.aov_sum$univariate.tests[2,4])
PIT.CSCategory_lower.lim <- PIT.CSCategory_lims$Lower.Limit/(PIT.CSCategory_lims$Lower.Limit + df.1 + df.2 + 1)
PIT.CSCategory_upper.lim <- PIT.CSCategory_lims$Upper.Limit/(PIT.CSCategory_lims$Upper.Limit + df.1 + df.2 + 1)

PIT.effectsizes <- matrix(c(PIT.aov$anova_table$pes[1], ifelse(is.na(PIT.CSCategory_lower.lim) == F, PIT.CSCategory_lower.lim, 0), ifelse(is.na(PIT.CSCategory_upper.lim) == F, PIT.CSCategory_upper.lim, .00059834237206)), #value computed with SPSS
                           ncol = 3, byrow = T)

colnames(PIT.effectsizes) <- c("Partial eta squared", "90% CI lower limit", "90% CI upper limit")
rownames(PIT.effectsizes) <- c("CSCategory")
PIT.effectsizes



lsmeans(PIT.aov, pairwise ~ Condition) #?

#else
mymodel = lmer(n_grips ~ Condition + (1|id), data = REWOD_PIT, REML = FALSE) 
lsmeans(mymodel, pairwise ~ Condition)

# manual planned contrasts
REWOD_PIT$cont[REWOD_PIT$condition== 'CSplus']     <- 'CSplus'
REWOD_PIT$cont[REWOD_PIT$condition== 'CSminus']     <- 'CSminus&Baseline'
REWOD_PIT$cont[REWOD_PIT$condition== 'Baseline']     <- 'CSminus&Baseline'
REWOD_PIT$cont       <- factor(REWOD_PIT$cont)

#
mymodel2 = lmer(n_grips ~ cont + (1+cont|id), data = REWOD_PIT, REML = FALSE) 

lsmeans(mymodel2, pairwise ~ cont)

#summary(aov(n_grips ~ cont + Error(id / (cont)), data = REWOD_PIT))

 

# otherwise ---------------------------------------------------------------
# manual planned contrasts
REWOD_PIT$cvalue[REWOD_PIT$condition== 'CSplus']       <- 2
REWOD_PIT$cvalue[REWOD_PIT$condition== 'CSminus']      <- -1
REWOD_PIT$cvalue[REWOD_PIT$condition== 'Baseline']     <- -1
REWOD_PIT$cvalue       <- factor(REWOD_PIT$cvalue)


# lmer analyis ~ condition 
main.n_grips = lmer(n_grips ~ cvalue + (1+cvalue|id) , data = REWOD_PIT, REML = FALSE) 
#main.n_grips = lmer(n_grips ~ cvalue + (1+cvalue|id) + (1|trial), data = REWOD_PIT, REML = FALSE) #1+cvalue\id or 1\id
#anova(main.n_grips)
summary(main.n_grips)
  
# quick check with classical anova (! this is not reliable)
#summary(aov(n_grips ~ cvalue + Error(id / (cvalue)), data = REWOD_PIT))

# model comparison
main.n_grips.0 = lmer(n_grips ~ (1+cvalue|id), data = REWOD_PIT, REML = FALSE)
test = anova(main.n_grips.0, main.n_grips, test = 'Chisq')
test
#sentence => main.n_grips is signifincatly better than the null model
# condition CS+ affected mobilized effort (χ2 (1)= 11.058, p=8.83 × 10^-4), rising it by about 4.50  ± 1.20 (standard errors).

# OR lsmeans(main.n_grips, pairwise ~ cvalue)

#Δ BIC = 4.074
delta_BIC = test$BIC[1] -test$BIC[2] 
delta_BIC


##############  Base cs minus ########## ??

# #contrasts
# REWOD_PIT$cvalue1[REWOD_PIT$condition== 'CSplus']     <- 0
# REWOD_PIT$cvalue1[REWOD_PIT$condition== 'CSminus']     <- -1
# REWOD_PIT$cvalue1[REWOD_PIT$condition== 'Baseline']     <- 1
# REWOD_PIT$cvalue1       <- factor(REWOD_PIT$cvalue1)
# 
# # lmer analyis ~ condition 
# main.n_grips = lmer(n_grips ~ cvalue1 + (1+cvalue1|id) , data = REWOD_PIT, REML = FALSE) 
# #main.n_grips = lmer(n_grips ~ cvalue1 + (1+cvalue1|id) + (1|trial), data = REWOD_PIT, REML = FALSE) #1+cvalue1\id or 1\id
# #anova(main.n_grips)
# summary(main.n_grips)
# 
# # quick check with classical anova (! this is not reliable)
# #summary(aov(n_grips ~ cvalue1 + Error(id / (cvalue1)), data = REWOD_PIT))
# 
# # model comparison
# main.n_grips.0 = lmer(n_grips ~ (1+cvalue1|id), data = REWOD_PIT, REML = FALSE)
# test = anova(main.n_grips.0, main.n_grips, test = 'Chisq')
# test
# #sentence => main.n_grips is signifincatly better than the null model
# # condition CS+ affected mobilized effort (χ2 (1)= 11.058, p=8.83 × 10^-4), rising it by about 4.50  ± 1.20 (standard errors).
# 
# #Δ BIC = 4.074
# delta_BIC = test$BIC[1] -test$BIC[2] 
# delta_BIC


