## R code for FOR REWOD GENERAL
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot, ggpubr, gridExtra, mosaic)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

#SETUP
taskHED = 'hedonic'
taskPIT = 'PIT'
con_name = 'AMY'
con1 = 'CSp-CSm'
con2 = 'reward-neutral'
mod1 = 'eff'
mod2 = 'int'



## R code for FOR REWOD_HED
# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS') 
setwd(analysis_path)

# open dataset 
BETAS_R_N <- read.delim(file.path(analysis_path, taskHED, 'ROI', paste('extracted_betas_',con_name,'.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_R_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset



R_N_df = merge(BETAS_R_N, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
R_N_df$ID <- factor(R_N_df$ID)



# open dataset 
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, taskPIT, 'ROI', paste('extracted_betas_',con_name,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, taskPIT, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
CSp_CSm_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)

# zscore
CSp_CSm_df$eff = zscore(CSp_CSm_df$eff)

R_N_df$int = zscore(R_N_df$int )


# PLOT FUNCTIONS --------------------------------------------------------------------

A1 <- ggplot(R_N_df, aes(int, AMY_AAA_betas)) + #A2
  geom_point( size = 1) +
  geom_smooth(method = "lm", col = "green", fullrange = T) +
  #labs(subtitle = paste("Adj R2 = ",signif(summary(lm(R_N_df$AMY_BLA_LEFT_betas~R_N_df$lik))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(R_N_df$AMY_BLA_LEFT_betas~R_N_df$lik))$coef[2,4], 3)))+
  #scale_x_continuous(name="Perceived Intensity", limits=c(-2.02, 2.02)) +
  #scale_y_continuous(name="Beta Reward > neutral", limits=c(-0.6, 0.6)) +
  theme(plot.subtitle = element_text(size = 10, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)

A2 <- ggplot(CSp_CSm_df, aes(eff, AMY_AA_betas)) + #A2
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green", fullrange = T) +
  #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_RN$AMY_BLA_LEFT_betas~CSp_CSm_RN$eff))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(CSp_CSm_RN$AMY_BLA_LEFT_betas~CSp_CSm_RN$eff))$coef[2,4], 3)))+
  scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 2.02)) +
  scale_y_continuous(name="Beta CSp > CSm", limits=c(-0.6, 0.6)) +
  theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)


figure1 <- ggarrange(A1, A2,
                     #labels = c("B: Coeficients of determination"),
                     ncol = 2, nrow = 1) 

