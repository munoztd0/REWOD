## R code for FOR REWOD_INST
# last modified on Nov 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------


# load libraries
pacman::p_load(MBESS, afex, car, ggplot2, dplyr, plyr, tidyr, reshape, Hmisc, Rmisc,  ggpubr, gridExtra, plotrix, lsmeans, BayesFactor)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

task = 'INSTRU'

# Set working directory
analysis_path <- file.path('~/REWOD/DERIVATIVES/BEHAV', task) 
setwd(analysis_path)

# open dataset
REWOD_INST <- read.delim(file.path(analysis_path,'REWOD_INSTRU_ses_first.txt'), header = T, sep ='') # read in dataset

# define factors
REWOD_INST$id                       <- factor(REWOD_INST$id)
REWOD_INST$session                  <- factor(REWOD_INST$session)
REWOD_INST$rewarded_response        <- factor(REWOD_INST$rewarded_response)

## remove sub 8 (we dont have scans)
REWOD_INST <- subset (REWOD_INST,!id == '8') 


# 
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

dfTRIAL <- data_summary(REWOD_INST, varname="n_grips", groupnames=c("trial"))

o = length(dfTRIAL$sd)
for(x in 1:o){
  dfTRIAL$sem[x] <- dfTRIAL$sd[x]/sqrt(length(dfTRIAL$sd))
  }



##plot n_grips to see the trajectory of learning (overall average by trials)

ggplot(dfTRIAL, aes(x = trial, y = n_grips)) +
  geom_point() + geom_line(group=1) +
  geom_errorbar(aes(ymin=n_grips-sem, ymax=n_grips+sem), color='grey', width=.2,
                position=position_dodge(0.05), linetype = "dashed") +
  theme_classic() +
  scale_y_continuous(breaks = c(9.50, seq.int(10,15, by = 1)), limits = c(9.50,14.5)) +
  scale_x_continuous(breaks=c(seq.int(1,24, by = 2), 24), limits = c(0,24)) + 
  labs(x = "Trial",
    y = "Number of Squeezes" )
  
  
#ANALYSIS

##1. number of grips: are participants gripping more over time?
REWOD_INST$trial            <- factor(REWOD_INST$trial)


#using afex
inst.aov <- aov_car(n_grips ~ trial + Error(id/trial), data = REWOD_INST, anova_table = list(es = "pes"))
inst.aov
inst.aov_sum <- summary(inst.aov)
inst.aov_sum

#----EFFECT SIZE
  
# Confidence interval

inst.trial_lims      <- conf.limits.ncf(F.value = inst.aov_sum$univariate.tests[2,5], conf.level = .90, df.1 <- inst.aov_sum$univariate.tests[2,2], df.2 <- inst.aov_sum$univariate.tests[2,4])
inst.trial_lower.lim <- inst.trial_lims$Lower.Limit/(inst.trial_lims$Lower.Limit + df.1 + df.2 + 1)
inst.trial_upper.lim <- inst.trial_lims$Upper.Limit/(inst.trial_lims$Upper.Limit + df.1 + df.2 + 1)

inst.effectsizes <- matrix(c(inst.aov$anova_table$pes[1], ifelse(is.na(inst.trial_lower.lim) == F, inst.trial_lower.lim, 0), ifelse(is.na(inst.trial_upper.lim) == F, inst.trial_upper.lim, .00059834237206)), #value computed with SPSS
                           ncol = 3, byrow = T)
colnames(inst.effectsizes) <- c("Partial eta squared", "90% CI lower limit", "90% CI upper limit")
rownames(inst.effectsizes) <- c("trial")
inst.effectsizes


#contrasts (should I include the first trial even its biased)

REWOD_INST$time <- rep(0, (length(REWOD_INST$trial)))

REWOD_INST$time[REWOD_INST$trial== '1']     <- 1
REWOD_INST$time[REWOD_INST$trial== '2']     <- 1
REWOD_INST$time[REWOD_INST$trial== '3']     <- 1
REWOD_INST$time[REWOD_INST$trial== '4']     <- 1
REWOD_INST$time[REWOD_INST$trial== '5']     <- 1

REWOD_INST$time[REWOD_INST$trial== '6']     <- -1
REWOD_INST$time[REWOD_INST$trial== '7']     <- -1
REWOD_INST$time[REWOD_INST$trial== '8']     <- -1
REWOD_INST$time[REWOD_INST$trial== '9']     <- -1
REWOD_INST$time[REWOD_INST$trial== '10']     <- -1

insttim.aov <- aov_car(n_grips ~ time + Error(id/time), data = REWOD_INST, anova_table = list(es = "pes"))
insttim.aov
insttim.aov_sum <- summary(insttim.aov)
insttim.aov_sum


  