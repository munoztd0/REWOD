## R code for FOR PICSTRESS
# last modified on October 2018


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(car, lme4, lmerTest, pbkrtest, ggplot2, dplyr, plyr, tidyr, multcomp, mvoutlier, HH, doBy, psych, pastecs, reshape, reshape2, 
               jtools, effects, compute.es, DescTools, MBESS, afex, ez, metafor, influence.ME)

require(lattice)


# Set working directory
analysis_path <- '/Users/evapool/switchdrive/PAVSTRESS/ANALYSIS/R/'#dirname(sys.frame(1)$ofile); # for this to work the script needs to be sourced

setwd(analysis_path)

# open dataset
PIC <- read.delim(file.path(analysis_path,'Database-PICSTRESS.txt'), header = T, sep ='') # read in dataset
PIC <- subset (PIC,!IC == 'NaN') # remove the missing trials

# define factors
PIC$ID               <- factor(PIC$ID)
PIC$trial            <- factor(PIC$trial)
PIC$run              <- factor(PIC$run)
PIC$session          <- factor(PIC$session)
PIC$order            <- factor(PIC$order)
PIC$IC_all           <- factor(PIC$IC_all)
PIC$IC               <- factor(PIC$IC)
PIC$pav_congr        <- factor(PIC$pav_congr)
PIC$condition        <- factor(PIC$condition)

# load library
library(sjstats)
library(gamlss)
library(lme4)
library(lmerTest)
library(ggplot2)
library(multcomp)
library(pbkrtest)
library(dplyr)
library(mvoutlier)
library(HH)
library(plyr)
library(doBy)
library(afex)


# get times in milliseconds
#PIC$IC_DW_pav   <- PIC$IC_DW_pav * 1000
#PIC$IC_DW_ins   <- PIC$IC_DW_ins * 1000
#PIC$ANT_DW_cue  <- PIC$ANT_DW_cue * 1000
#PIC$ANT_DW_pav  <- PIC$ANT_DW_pav * 1000
#PIC$ANT_DW_ins  <- PIC$ANT_DW_ins * 1000
#PIC$ANT_ACC     <- PIC$ANT_ACC * 1000
#PIC$US_RT       <- PIC$US_RT * 1000


## define contrasts of interest based on hypothesis

# simplest value contrast (value S)
PIC$Cvalue[PIC$IC== 'CSm']     <- -1
PIC$Cvalue[PIC$IC== 'congr']   <- .5
PIC$Cvalue[PIC$IC== 'incongr'] <- .5

# Congruency constrast
PIC$Ccongr[PIC$IC== 'CSm']     <- 0
PIC$Ccongr[PIC$IC== 'congr']   <- 1
PIC$Ccongr[PIC$IC== 'incongr'] <- -1

## remove sub 2
PIC <- subset (PIC,!ID == '2') 

# ----------------------------------------------------------------------------------------------------
#                                 PLOTS
# ----------------------------------------------------------------------------------------------------


## supercifical look at the distribution (non-averaged per participant)

# IC dwell time on ins and pav ROIs
boxplot(PIC$IC_DW_ins ~ PIC$IC, las = 1)
boxplot(PIC$IC_DW_pav ~ PIC$IC, las = 1)

# ANT dwell time on ins and pav ROIs
boxplot(PIC$ANT_DW_ins ~ PIC$IC, las = 1)
boxplot(PIC$ANT_DW_pav ~ PIC$IC, las = 1)

# US reaction time for congruent PAV
boxplot(PIC$US_RT ~ PIC$pav_congr, las = 1)

# IC ratings
boxplot(PIC$R.IC.ratings ~ PIC$IC, las = 1)

# pupil 
boxplot(PIC$IC_pupil ~ PIC$IC, las = 1)


## plot overall effects

# get means by group
bc = ddply(PIC, .(IC), summarise, ANT_DW_pav=mean(ANT_DW_pav, na.rm = TRUE), ANT_DW_ins=mean(ANT_DW_ins, na.rm = TRUE), IC_DW_pav = mean(IC_DW_pav, na.rm = TRUE), IC_DW_ins = mean(IC_DW_ins, na.rm = TRUE), R.IC.ratings = mean(R.IC.ratings, na.rm = TRUE))

# get means by participant 
bs = ddply(PIC, .(ID, IC), summarise, ANT_DW_pav=mean(ANT_DW_pav, na.rm = TRUE), ANT_DW_ins=mean(ANT_DW_ins, na.rm = TRUE), IC_DW_pav = mean(IC_DW_pav, na.rm = TRUE), IC_DW_ins = mean(IC_DW_ins, na.rm = TRUE), R.IC.ratings = mean(R.IC.ratings, na.rm = TRUE))

# remove CSm from the plots for now
bc <- subset(bc, IC == 'congr' | IC == 'incongr')
bs <- subset(bs, IC == 'congr' | IC == 'incongr')

# plot anticipatory dwell time on pav ROI
ggplot(bs, aes(x = IC, y = ANT_DW_pav, fill = I('royalblue1'), color = I('royalblue4'))) +
  geom_point() +
  geom_line(aes(group = ID), alpha = .3, size = 1) +
  geom_bar(data = bc, stat = "identity", alpha = .3) +
  guides(color = "none", fill = "none") +
  guides(color = "none", fill = "none") +
  theme_bw() +
  labs(
    title = "Overall effect",
    x = "Instrumental Cue",
    y = "Dwell time in the Pavlovian ROI"
  )

# plot anticipatory dwell time on ins ROI 
ggplot(bs, aes(x = IC, y = ANT_DW_ins, fill = I('darkorange4'), color = I('darkorange3'))) +
  geom_point() +
  geom_line(aes(group = ID), alpha = .3, size = 1) +
  geom_bar(data =bc, stat = "identity", alpha = .3) +
  guides(color = "none", fill = "none") +
  guides(color = "none", fill = "none") +
  theme_bw() +
  labs(
    title = "Overall effect",
    x = "Instrumental Cue",
    y = "Dwell time in the instrumental ROI"
  )


## plot stress induction effect
STRESS <- subset (PIC,!session == 'first') 

# get means by group
bc.stress = ddply(STRESS, .(IC, condition), summarise, ANT_DW_pav = mean(ANT_DW_pav, na.rm = TRUE), ANT_DW_ins = mean(ANT_DW_ins, na.rm = TRUE), IC_DW_pav = mean(IC_DW_pav, na.rm = TRUE), IC_DW_ins = mean(IC_DW_ins, na.rm = TRUE), R.IC.ratings = mean(R.IC.ratings, na.rm = TRUE))

# get means by participant 
bs.stress = ddply(STRESS, .(ID, IC, condition), summarise, ANT_DW_pav = mean(ANT_DW_pav, na.rm = TRUE), ANT_DW_ins = mean(ANT_DW_ins, na.rm = TRUE), IC_DW_pav = mean(IC_DW_pav, na.rm = TRUE), IC_DW_ins = mean(IC_DW_ins, na.rm = TRUE), R.IC.ratings = mean(R.IC.ratings, na.rm = TRUE))

# remove CSm from the plots for now
bc.stress <- subset(bc.stress, IC == 'congr' | IC == 'incongr')
bs.stress <- subset(bs.stress, IC == 'congr' | IC == 'incongr')

# plot anticipatory dwell time on pav ROI
ggplot(data = bs.stress, aes(x = IC, y = ANT_DW_pav, fill = condition, color = I("black"))) +
  geom_bar(data = bc.stress, stat = "identity", alpha = .3, position = position_dodge(width = 0.95)) +
  theme_classic() +
  scale_fill_manual(values = c("royalblue1", "royalblue4")) +
  labs(
    title = "Effect of stress induction",
    x = "Instrumental cue",
    y = "Dwell time in the Pavlovian ROI",
    fill = "Condition"
  )

# plot anticipatory dwell time on ins ROI
ggplot(data = bs.stress, aes(x = IC, y = ANT_DW_ins, fill = condition, color = I("black"))) +
  geom_bar(data = bc.stress, stat = "identity", alpha = .3, position = position_dodge(width = 0.95)) +
  theme_classic() +
  scale_fill_manual(values = c("darkorange1", "darkorange4")) +
  labs(
    title = "Effect of stress induction",
    x = "Instrumental cue",
    y = "Dwell time in the Instrumental ROI",
    fill = "Condition"
  )


## plot runs independently
PIC.run1 <- subset(STRESS, run == '1')
PIC.run2 <- subset(STRESS, run == '2')
PIC.run3 <- subset(STRESS, run == '3')



# ----------------------------------------------------------------------------------------------------
#                                 ANALYSIS
# ----------------------------------------------------------------------------------------------------

# remove variables containig nans for gmlss models
vars <- names(PIC) %in% c("ID","trial", "run","session","condition","Cvalue","Ccongr","ANT_DW_ins",'ANT_DW_pav')
PIC.naomit <- PIC[vars] # DATABASE WITHOUT NANS
PIC.naomit <- na.omit(PIC.naomit)

#-------------------------------------- Manipulation checks ------------------------------------------------


#########  1. Reaction time: are participants faster to detect expected reward compared with unexpected ones ######################

# lmer analyis
main.USRT = lmer(US_RT ~ pav_congr + (1+pav_congr|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.USRT)

# quick check with classical anova (! this is not reliable)
summary(aov(US_RT ~ pav_congr + Error(ID / (pav_congr)), data = PIC))

# model comparison
main.USRT.0 = lmer(US_RT ~ (1|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.USRT.0, main.USRT, test = 'Chisq')


#########  2. Liking ratings: do participants like more IC associated with reward? ######################

# lmer analyis
main.liking = lmer(R.IC.ratings ~ Cvalue + (1+Cvalue|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.liking)

# quick check with classical anova (! this is not reliable)
summary(aov(R.IC.ratings ~ Cvalue + Error(ID / (Cvalue)), data = PIC))

# model comparison
main.liking.0 = lmer(R.IC.ratings ~ (1|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.liking.0, main.liking, test = 'Chisq')


######### 3. Value effect: do participants look more where into the reinfored location? ######################

# lmer analysis
main.value = lmer(ANT_DW_ins ~ Cvalue + (1+Cvalue|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.value)

# quick check with classical anova (! this is not reliable)
summary(aov(ANT_DW_ins ~ Cvalue + Error(ID / (Cvalue)), data = PIC))

#gamlss (for me this is the best way to check that the lmer do not depend on the distributions)
summary(gamlss(ANT_DW_ins ~ Cvalue + re(random=~1+Ccongr|ID) + re(random=~1|trial), family = BEINF, data = na.omit(PIC.naomit)))

# model comparison
main.value.0 = lmer(ANT_DW_ins ~ (1|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.value.0, main.value, test = 'Chisq')


######### 4. Learning effect: do participants look more where into the Pavlovian ROI (congruent vs incongruent?) ######################

# lmer analysis
main.learning  = lmer(ANT_DW_pav ~ Ccongr + (1 + Ccongr|ID) + (1|trial), data = PIC, REML = FALSE)
anova (main.learning)

# quick check with classical anova (! this is not reliable)
summary(aov(ANT_DW_pav ~ Ccongr + Error(ID / (Ccongr)), data = PIC))

#gamlss (for me this is the best way to check that the lmer do not depend on the distributions)
summary(gamlss(ANT_DW_pav ~ Ccongr + re(random=~1+Ccongr|ID) + re(random=~1|trial), family = BEINF, data = na.omit(PIC.naomit)))

# model comparison
main.learning.0 = lmer(ANT_DW_pav ~ (1|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.learning.0, main.learning, test = 'Chisq')


######### 5. Interference effect: do participants look more into the Instrumental ROI (congrent vs incongruent?)######################

# lmer analysis
main.interference  = lmer(ANT_DW_ins ~ Ccongr + (1+Ccongr|ID) + (1|trial), data = PIC, REML = FALSE)
anova (main.interference)

# quick check with classical anova (! this is not reliable)
summary(aov(ANT_DW_ins ~ Ccongr + Error(ID / (Ccongr)), data = PIC))

#gamlss (for me this is the best way to check that the lmer do not depend on the distributions)
summary(gamlss(ANT_DW_ins ~ Ccongr + re(random=~1+Ccongr|ID) + re(random=~1|trial), family = BEINF, data = na.omit(PIC.naomit)))

# model comparison (interference effect)
main.interference.0 = lmer(ANT_DW_ins ~ (1|ID) + (1|trial), data = PIC, REML = FALSE)
anova(main.interference.0, main.interference, test = 'Chisq')

# model comparison (slope adjustement per trial)
main.interference.0trial = lmer(ANT_DW_ins ~ (1|ID), data = PIC, REML = FALSE)
anova(main.interference.0trial, main.interference, test = 'Chisq')


######### 6. Effort effect on the pupil? (ATTENTION DESIGN NOT OK FOR PUPIL ANALYSIS THIS IS JUST CURIOUSITY) ######################

# effort  
main.effort = lmer(IC_pupil ~ Ccongr + (1+Ccongr|ID) + (1|trial), data = PIC, REML=FALSE)
anova (main.effort)

# value
main.value  = lmer(IC_pupil ~ Cvalue + (1+Cvalue|ID) + (1|trial), data = PIC, REML=FALSE)
anova (main.value)


#-------------------------------------- NOW WE TEST THE EFFECTS OF STRESS ------------------------------------------------

# first we need to prepare the database and remove the first session
STRESS <- subset (PIC,!session == 'first') 

# remove variables containig nans for gmlss models
vars <- names(STRESS) %in% c("ID","trial", "run","session","condition","Cvalue","Ccongr","ANT_DW_ins",'ANT_DW_pav','bin')
STRESS.naomit <- STRESS[vars] # DATABASE WITHOUT NANS
STRESS.naomit <- na.omit(STRESS.naomit)

# then we obtain the first - and the first + second runs which are these that interest us.
STRESS.first <- subset (STRESS, run == '1')


## plot mean DW on ins ROI per bin and IC congruency and stress manip

# get mean by bin, IC, and condition
bs = ddply(STRESS, .(bin, IC, condition), summarise, M = mean(ANT_DW_ins, na.rm = TRUE), SE = sd(ANT_DW_ins, na.rm = TRUE) / sqrt(20))

# remove CSm means
bs <- subset(bs, IC != 'CSm')

# plot mean DW on ins ROI by bin and IC Congruency
ggplot(data = bs, aes(x = bin, y = M, group = IC)) +
  geom_point(aes(shape = IC), position = position_dodge(width = 0.3), size = 2) +
  geom_line(aes(linetype = IC), position = position_dodge(width = 0.3)) +
  facet_grid(~ condition) +
  #geom_errorbar(data = bs, aes(ymin = M - SE, ymax = M + SE), position = position_dodge(width = 0.3), width = 0.5) +
  theme_classic() +
  scale_shape_manual(values = c(16, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = 'DW on Instrumental ROI by IC congruengy, bin\nand condition',
    x = 'Ten trials bin',
    y = 'DW on Instrumental ROI',
    shape = 'IC Congruency',
    linetype = 'IC Congruency'
  )


## plot IC congruency index per bin and stress manip

# get index per bin and condition
bs = ddply(STRESS, .(bin, condition), summarise, IC_index = mean(ANT_DW_ins[IC == 'congr'], na.rm = TRUE) - mean(ANT_DW_ins[IC == 'incongr'], na.rm = TRUE))

# plot index by bin and condition
ggplot(data = bs, aes(x = bin, y = IC_index, group = condition)) +
  geom_point(aes(group = condition, colour = condition), size = 3) +
  geom_line(aes(colour = condition)) +
  scale_y_continuous(limits = c(-0.1,0.1)) +
  scale_colour_manual(values = c('black', 'red')) +
  theme_classic() +
  labs(
    title = 'IC Congruency Index per 10 trials and condition',
    x = 'Ten trials bin',
    y = 'IC Congruency Index',
    group = 'Condition',
    colour = 'Condition'
  )


### overall effects

mdl  = lmer(ANT_DW_ins ~ Ccongr*condition + (1+Ccongr*condition|ID) + (1|trial), data = STRESS, REML = FALSE)
anova(mdl)

mdl  = lmer(ANT_DW_pav ~ Ccongr*condition + (1+Ccongr*condition|ID) + (1|trial), data = STRESS, REML = FALSE)
anova(mdl)

mdl  = lmer(ANT_DW_ins ~ Cvalue*condition + (1+Cvalue*condition|ID) + (1|trial), data = STRESS, REML = FALSE)
anova(mdl)

mdl  = lmer(ANT_DW_pav ~ Cvalue*condition + (1+Cvalue*condition|ID) + (1|trial), data = STRESS, REML = FALSE)
anova(mdl)


### let's plot to see what is the direction of the effects

# get means by group
bc = ddply(PIC, .(IC, condition), summarise, ANT_DW_ins=mean(ANT_DW_ins, na.rm = TRUE), ANT_DW_ins=mean(ANT_DW_ins, na.rm = TRUE), IC_DW_pav = mean(IC_DW_pav, na.rm = TRUE), IC_DW_ins = mean(IC_DW_ins, na.rm = TRUE))

# get means by participant 
bs = ddply(PIC, .(ID, IC, condition), summarise, ANT_DW_ins=mean(ANT_DW_ins, na.rm = TRUE), ANT_DW_ins=mean(ANT_DW_ins, na.rm = TRUE), IC_DW_pav = mean(IC_DW_pav, na.rm = TRUE), IC_DW_ins = mean(IC_DW_ins, na.rm = TRUE))

# remove CSm from the plots for now
bc <- subset(bc, IC == 'congr' | IC == 'incongr')
bs <- subset(bs, IC == 'congr' | IC == 'incongr')


# plot anticipatory dwell time on ins ROI (effect of stress in congr and incongr)
ggplot(data = bs, aes(x = IC, y = ANT_DW_ins, fill = condition, color = I("black"))) +
  geom_bar(data = bc, stat = "identity", alpha = .3, position = position_dodge(width = 0.95)) +
  theme_classic() +
  scale_fill_manual(values = c("darkorange1", "darkorange4")) +
  labs(
    title = "",
    x = "Instrumental cue",
    y = "Dwell time in the Pavlovian ROI",
    fill = "Condition"
  )


# plot anticipatory dwell time on ins ROI (effect of congruency in stress-free and stress-full)
ggplot(bs, aes(x = IC, y = ANT_DW_ins, fill = IC, color = IC)) +
  geom_bar(data = bc, stat = "identity", alpha = .3) +
  facet_grid(~ condition) +
  theme_bw() +
  labs(
    title = '',
    x = 'condition', 
    y = "dwell instrumental"
  )
