## R code for FOR REWOD_INST
# last modified on Nov 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------


# load libraries
pacman::p_load(afex, car, ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot, ggpubr, gridExtra)

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
REWOD_INST$id               <- factor(REWOD_INST$id)
#REWOD_INST$trial            <- factor(REWOD_INST$trial)
REWOD_INST$session          <- factor(REWOD_INST$session)
REWOD_INST$rewarded_response        <- factor(REWOD_INST$rewarded_response)

## remove sub 8 (we dont have scans)
REWOD_INST <- subset (REWOD_INST,!id == '8') 



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

df2 <- data_summary(REWOD_INST, varname="n_grips", 
                    groupnames=c("trial"))
o = length(df2$sd)
for(x in 1:o){
  df2$sem[x] <- df2$sd[x]/sqrt(length(df2$sd))
  }
##plot n_grips to see the trajectory of learning (overall average by trials)


  
  ggplot(df2, aes(x = trial, y = n_grips)) +
    geom_point() + geom_line(group=1) +
    geom_errorbar(aes(ymin=n_grips-sem, ymax=n_grips+sem), color='grey', width=.2,
                  position=position_dodge(0.05), linetype = "dashed") +
    theme_classic() +
    #scale_x_continuous(limit = c(0, 24)) +
    #scale_y_continuous(limit = c(9, 14)) +
    labs(
      #title = "Average n-grips per trial",
      x = "Trial",
      y = "Number of Squeezes"
    )
  
  
  #ANALYSIS
  
  ##1. number of grips: are participants gripping more over time?
  REWOD_INST$trial            <- factor(REWOD_INST$trial)
  
  
  
  #using afex
  pav.aov <- aov_car(n_grips ~ trial + Error(id/trial), data = REWOD_INST, anova_table = list(es = "pes"))
  pav.aov
  pav.aov_sum <- summary(pav.aov)
  pav.aov_sum
  
  #----EFFECT SIZE
  
  # Confidence interval (see http://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html)
  
  pav.trial_lims      <- conf.limits.ncf(F.value = pav.aov_sum$univariate.tests[2,5], conf.level = .90, df.1 <- pav.aov_sum$univariate.tests[2,2], df.2 <- pav.aov_sum$univariate.tests[2,4])
  pav.trial_lower.lim <- pav.trial_lims$Lower.Limit/(pav.trial_lims$Lower.Limit + df.1 + df.2 + 1)
  pav.trial_upper.lim <- pav.trial_lims$Upper.Limit/(pav.trial_lims$Upper.Limit + df.1 + df.2 + 1)
  
  pav.effectsizes <- matrix(c(pav.aov$anova_table$pes[1], ifelse(is.na(pav.trial_lower.lim) == F, pav.trial_lower.lim, 0), ifelse(is.na(pav.trial_upper.lim) == F, pav.trial_upper.lim, .00059834237206)), #value computed with SPSS
                             ncol = 3, byrow = T)
  colnames(pav.effectsizes) <- c("Partial eta squared", "90% CI lower limit", "90% CI upper limit")
  rownames(pav.effectsizes) <- c("trial")
  pav.effectsizes
  
 
  
  
  #contrasts (should I include the first trial even its biased)
  
  REWOD_INST$time <- rep(0, (length(REWOD_INST$trial)))

  REWOD_INST$time[REWOD_INST$trial== '1']     <- 1
  REWOD_INST$time[REWOD_INST$trial== '2']     <- 1
  REWOD_INST$time[REWOD_INST$trial== '3']     <- 1
  REWOD_INST$time[REWOD_INST$trial== '4']     <- 1
  REWOD_INST$time[REWOD_INST$trial== '5']     <- 1


  
  x = lm(n_grips ~ time, data = REWOD_INST)
  
  summary(x)
  
  