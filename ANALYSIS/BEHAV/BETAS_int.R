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
con_name2 = 'Od_NoOd'
con1 = 'CSp-CSm'
con2 = 'Odor-NoOdor'
mod1 = 'eff'
mod2 = 'int'
mod3 = 'lik'



## R code for FOR REWOD_HED
# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS') 
setwd(analysis_path)

# open dataset 
BETAS_O_N <- read.delim(file.path(analysis_path, taskHED, 'ROI', paste('extracted_betas_',con_name2,'.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_O_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset

LIK_O_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(con2,'_', mod3, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset



O_N_df = merge(BETAS_O_N, INT_O_N, by.x = "ID", by.y = "subj", all.x = TRUE)
O_N_df = merge(O_N_df, LIK_O_N, by.x = "ID", by.y = "subj", all.x = TRUE)

# define factors
O_N_df$ID <- factor(O_N_df$ID)



# open dataset 
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, taskPIT, 'ROI', paste('extracted_betas_',con_name,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, taskPIT, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
CSp_CSm_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)

# zscore
CSp_CSm_df$eff = zscore(CSp_CSm_df$eff)

O_N_df$int = zscore(O_N_df$int )
O_N_df$lik = zscore(O_N_df$lik )


# PLOT FUNCTIONS --------------------------------------------------------------------

A1 <- ggplot(O_N_df, aes(int, AMY_AAA_betas)) + #A2
  geom_point( size = 1) +
  geom_smooth(method = "lm", col = "orange", fullrange = T) +
  #labs(subtitle = paste("Adj R2 = ",signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$int))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(O_N_df$AMY_BLA_LEFT_betas~O_N_df$lik))$coef[2,4], 3)))+
  scale_x_continuous(name="Perceived intensity", limits=c(-2.02, 3.02)) +
  scale_y_continuous(name="Beta Odor > No Odor", limits=c(-0.5, 2)) +
  theme(plot.subtitle = element_text(size = 10, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)

A2 <- ggplot(O_N_df, aes(lik, AMY_AAA_betas)) + 
  geom_point( size = 1) +
  geom_smooth(method = "lm", col = "orange", fullrange = T) +
  #labs(subtitle = paste("Adj R2 = ",signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$lik))$adj.r.squared, 3),
  #"likercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(O_N_df$AMY_BLA_LEFT_betas~O_N_df$lik))$coef[2,4], 3)))+
  scale_x_continuous(name="Hedonic Pleasure", limits=c(-2.02, 3.02)) +
  scale_y_continuous(name="Beta Odor > No Odor", limits=c(-0.5, 2)) +
  theme(plot.subtitle = element_text(size = 10, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)

A3 <- ggplot(CSp_CSm_df, aes(eff, AMY_AAA_betas)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange", fullrange = T) +
  #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_RN$AMY_AAA_betas~CSp_CSm_RN$eff))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(CSp_CSm_df$AMY_AAA_betas~CSp_CSm_df$eff))$coef[2,4], 3)))+
  scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 3.02)) +
  scale_y_continuous(name="Beta CSp > CSm", limits=c(-0.5, 2)) +
  theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)


figure1 <- ggarrange(A1, A2, A3,
                     #labels = c("B: Coeficients of determination"),
                     ncol = 3, nrow = 1) 

AMY_AAA = image_read_pdf('~/REWOD/DERIVATIVES/BEHAV/FIGURES/AMY_int_AAA.pdf')

A4 <-  rasterGrob(AMY_AAA, interpolate=TRUE)



figure2 <- ggarrange(A4, figure1,
                     labels = c("A","B"),
                     vjust=1.5, hjust = -1,
                     ncol = 1, nrow = 2) 




# p-val -------------------------------------------------------------------



P_eff =signif(summary(lm(CSp_CSm_df$AMY_AAA_betas~CSp_CSm_df$eff))$coef[2,4], 3)
P_lik =signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$lik))$coef[2,4], 3)
P_int =signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$int))$coef[2,4], 3)
P_eff 
P_lik 
P_int

Radj_eff =signif(summary(lm(CSp_CSm_df$AMY_AAA_betas~CSp_CSm_df$eff))$adj.r.squared, 3)
Radj_lik =signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$lik))$adj.r.squared, 3)
Radj_int =signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$int))$adj.r.squared, 3)
Radj_eff 
Radj_lik 
Radj_int
