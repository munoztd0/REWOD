## R code for FOR REWOD_PIT
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

#SETUP
task = 'PIT'
con_name1 = 'CSp_CSm'

con1 = 'CSp-CSm'

mod1 = 'eff'


# Set working directory
analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
setwd(analysis_path)

# open dataset 
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset



# merge
eff_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)



# define factors
eff_df$ID <- factor(eff_df$ID)




# plot perceived_likings by time with regression lign
ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
# Plot CSp_CSm liking

eff = eff_df$eff
ggplotRegression(lm(eff_df$Nacc_Left~eff))
ggplotRegression(lm(eff_df$Nacc_Right~eff))
ggplotRegression(lm(eff_df$vmPFC_Left~eff))
ggplotRegression(lm(eff_df$vmPFC_Right~eff))




# Corr
corr_CSp_CSm.rcorr = rcorr(as.matrix(eff_df))
corr_CSp_CSm.coeff = corr_CSp_CSm.rcorr$r[2:5,6]
corr_CSp_CSm.p = corr_CSp_CSm.rcorr$P[2:5,6]


corrplot(as.matrix(corr_CSp_CSm.coeff), method = "circle",
         tl.col = "black", tl.srt = 45)



# Get the R^2 and adj R* FOR EFF
CSp_CSp_R_squared_eff <- data_frame()
CSp_CSp_R_adj_eff<- data_frame()
namesEff = c("Nacc_Left","Nacc_Right", "vmPFC_Left", "vmPFC_Right")

CSp_CSp_R_squared_eff[1,1] <- summary(lm(eff_df$Nacc_Left~eff))$r.squared
CSp_CSp_R_adj_eff[1,1] <- summary(lm(eff_df$Nacc_Left~eff))$adj.r.squared

CSp_CSp_R_squared_eff[2,1] <- summary(lm(eff_df$Nacc_Right~eff))$r.squared
CSp_CSp_R_adj_eff[2,1] <- summary(lm(eff_df$Nacc_Right~eff))$adj.r.squared

CSp_CSp_R_squared_eff[3,1] <- summary(lm(eff_df$vmPFC_Left~eff))$r.squared
CSp_CSp_R_adj_eff[3,1] <- summary(lm(eff_df$vmPFC_Left~eff))$adj.r.squared

CSp_CSp_R_squared_eff[4,1] <- summary(lm(eff_df$vmPFC_Right~eff))$r.squared
CSp_CSp_R_adj_eff[4,1] <- summary(lm(eff_df$vmPFC_Right~eff))$adj.r.squared

CSp_CSp_R_squared_eff[,2] <- namesEff
CSp_CSp_R_adj_eff[,2] <- namesEff


## SUMMARY ##
# so basically contrast 
# CSp-CSm -> eff&Nacc R+L + vmPFC right

