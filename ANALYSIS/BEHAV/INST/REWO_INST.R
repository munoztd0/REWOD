## R code for FOR REWOD_INST
# last modified on Nov 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------


# load libraries
pacman::p_load(apaTables, MBESS, afex, car, ggplot2, dplyr, plyr, tidyr, reshape, Hmisc, Rmisc,  ggpubr, gridExtra, plotrix, lsmeans, BayesFactor)

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
REWOD_INST$trial        <- factor(REWOD_INST$trial)

## remove sub 8 (we dont have scans)
REWOD_INST <- subset (REWOD_INST,!id == '8') 

#REWOD_INST <- filter(REWOD_INST, rewarded_response == 2)




df <- summarySE(REWOD_INST, measurevar="n_grips", groupvars=c("id", "trial"))
dfTRIAL <- summarySEwithin(df,
                         measurevar = "n_grips",
                         withinvars = "trial", 
                         idvar = "id")


dfTRIAL$trial        <- as.numeric(dfTRIAL$trial)
##plot n_grips to see the trajectory of learning (overall average by trials)


ggplot(dfTRIAL, aes(x = trial, y = n_grips)) +
    geom_point() + geom_line(group=1) +
    geom_errorbar(aes(ymin=n_grips-se, ymax=n_grips+se), color='grey', width=.3,
                  position=position_dodge(0.05), linetype = "dashed") +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0), limits = c(10,14)) + #, breaks = c(9.50, seq.int(10,15, by = 1)), ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0,25), breaks=c(0, seq.int(1,25, by = 3))) + #,breaks=c(seq.int(1,24, by = 2), 24), limits = c(0,24)) + 
    labs(x = "Trial
          ",
      y = "Number of Squeezes",title= "   
       ") +
    theme(text = element_text(size=rel(4)), plot.margin = unit(c(1, 1,0, 1), units = "cm"), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

  
#ANALYSIS

# ANOVA trials ------------------------------------------------------------


##1. number of grips: are participants gripping more over time?
REWOD_INST$trial            <- factor(REWOD_INST$trial)


anova_model = ezANOVA(data = REWOD_INST,
                      dv = n_grips,
                      wid = id,
                      within = trial,
                      detailed = TRUE,
                      type = 3)



# effect sizes ------------------------------------------------------------

dfm = anova_model$ANOVA$DFn[2]
dfe = anova_model$ANOVA$DFd[2]
msm = anova_model$ANOVA$SSn[2] / anova_model$ANOVA$DFn[2]
mse = anova_model$ANOVA$SSd[2] / anova_model$ANOVA$DFd[2]
mss = anova_model$ANOVA$SSd[1] / anova_model$ANOVA$DFd[1]
ssm = anova_model$ANOVA$SSn[2]
sse = anova_model$ANOVA$SSd[2]
sss = anova_model$ANOVA$SSd[1]
a = .1

f = msm/mse

partial_omega = (f-1)/(f + (dfe +1)/dfm)
limits <- ci.R2(R2 = partial_omega, df.1 = dfm, df.2 = dfe, conf.level = (1-a))
partial_omega
limits$Lower.Conf.Limit.R2
limits$Upper.Conf.Limit.R2



#contrasts (should I include the first trial even its biased?)

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


  