## R code for FOR REWOD_HED
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot, ggpubr, gridExtra)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}


#SETUP
taskPIT = 'PIT'
taskHED = 'hedonic'


con_name2 = 'CSp_CSm'

con2 = 'reward-neutral'

mod1 = 'eff'

mod1 = 'lik'
mod2 = 'int'


# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', taskPIT) 
analysis_path_mod <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', taskHED) 
setwd(analysis_path)

# open dataset 
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_inversed_',con_name2,'.txt',sep="")), header = T, sep ='\t') # read in dataset

LIK_CSp_CSm <- read.delim(file.path(analysis_path_mod, 'GLM-04', 'group_covariates', paste(con2,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_CSp_CSm <- read.delim(file.path(analysis_path_mod, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset

# merge

CSp_CSm_df = merge(BETAS_CSp_CSm, LIK_CSp_CSm, by.x = "ID", by.y = "subj", all.x = TRUE)
CSp_CSm_df = merge(CSp_CSm_df, INT_CSp_CSm, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)



# PLOT FUNCTIONS --------------------------------------------------------------------


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       #"Intercept =",signif(fit$coef[[1]],5 ),
                       #" Slope =",signif(fit$coef[[2]], 5),
                       "  &  P =",signif(summary(fit)$coef[2,4], 5))) +
    theme(plot.title = element_text(size = 10))
  
}




#  Plot for CSp_CSm  ----------------------------------------------------------

# For liking

lik = CSp_CSm_df$lik
A <- ggplotRegression(lm(CSp_CSm_df[[2]]~lik)) + rremove("x.title")
B <- ggplotRegression(lm(CSp_CSm_df[[3]]~lik)) + rremove("x.title")
C <- ggplotRegression(lm(CSp_CSm_df[[4]]~lik)) + rremove("x.title")
D <- ggplotRegression(lm(CSp_CSm_df[[5]]~lik)) + rremove("x.title")
A1 <- ggplotRegression(lm(CSp_CSm_df[[6]]~lik)) + rremove("x.title")
B1 <- ggplotRegression(lm(CSp_CSm_df[[7]]~lik))+ rremove("x.title")


figure3 <- ggarrange(A,B,C,D,A1,B1,
                     labels = c("A: AMY_BLA_L", "B: AMY_full_L", "C: CAUD_VENTR_L", "D: PUT_L", "E: PUT_R", "F: caud_PAULI", "G: nacc_PAULI"),
                     ncol = 2, nrow = 3,
                     vjust=3, hjust=-1) 

figure3 <- annotate_figure(figure3,
                           top = text_grob("Coeficient of determination: REWARD-NEUTRAL for LIKING", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_lik_coeff.pdf')
plot(figure3)
dev.off()


# For intensity

int = CSp_CSm_df$int
A2 <- ggplotRegression(lm(CSp_CSm_df[[2]]~int)) + rremove("x.title")
B2 <- ggplotRegression(lm(CSp_CSm_df[[3]]~int)) + rremove("x.title")
C2 <- ggplotRegression(lm(CSp_CSm_df[[4]]~int)) + rremove("x.title")
D2 <- ggplotRegression(lm(CSp_CSm_df[[5]]~int)) + rremove("x.title")
A3 <- ggplotRegression(lm(CSp_CSm_df[[6]]~int)) + rremove("x.title")
B3 <- ggplotRegression(lm(CSp_CSm_df[[7]]~int))+ rremove("x.title")

figure4 <- ggarrange(A2,B2,C2,D2,A3,B3,
                     labels = c("A: AMY_BLA_L", "B: AMY_full_L", "C: CAUD_VENTR_L", "D: PUT_L", "E: PUT_R", "F: caud_PAULI", "G: nacc_PAULI"),
                     ncol = 2, nrow = 3)

figure4 <- annotate_figure(figure4,
                           top = text_grob("Coeficient of determination: REWARD-NEUTRAL for INTENSITY", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_int_coeff.pdf')
plot(figure4)
dev.off()




# CORRELATIONS CSp_CSm ------------------------------------------------------

corr_CSp_CSm.rcorr = rcorr(as.matrix(CSp_CSm_df))
corr_CSp_CSm.coeff = corr_CSp_CSm.rcorr$r[2:7,8:9]
corr_CSp_CSm.p = corr_CSp_CSm.rcorr$P[2:7,8:9]

col3 <- colorRampPalette(c("blue", "white", "red")) 
# PLOT CORR

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_corrplot.pdf')
corrplot(corr_CSp_CSm.coeff , method = "circle",tl.col = "black", tl.srt = 45, col = col3(20))
dev.off()


# Get R.adj & R.squared for R_C ------------------------------------------


# # FOR LIKING
# R_C_R_squared_lik <- data_frame()
# R_C_R_adj_lik <- data_frame()
# namesRC = c("AMY_Left","AMY_Right", "Piri_Left", "Piri_Right", "subcal_Left", "subcal_Right")
# 
# #
# R_C_R_squared_lik[1,1] <- summary(lm(R_C_df$AMY_Left~lik))$r.squared
# R_C_R_adj_lik[1,1] <- summary(lm(R_C_df$AMY_Left~lik))$adj.r.squared
# 
# R_C_R_squared_lik[2,1] <- summary(lm(R_C_df$AMY_Right~int))$r.squared
# R_C_R_adj_lik[2,1] <- summary(lm(R_C_df$AMY_Right~lik))$adj.r.squared
# 
# R_C_R_squared_lik[3,1] <- summary(lm(R_C_df$Piri_Left~lik))$r.squared
# R_C_R_adj_lik[3,1] <- summary(lm(R_C_df$Piri_Left~lik))$adj.r.squared
# 
# R_C_R_squared_lik[4,1] <- summary(lm(R_C_df$Piri_Right~lik))$r.squared
# R_C_R_adj_lik[4,1] <- summary(lm(R_C_df$Piri_Right~lik))$adj.r.squared
# 
# R_C_R_squared_lik[5,1] <- summary(lm(R_C_df$subcal_Left~lik))$r.squared
# R_C_R_adj_lik[5,1] <- summary(lm(R_C_df$subcal_Left~lik))$adj.r.squared
# 
# R_C_R_squared_lik[6,1] <- summary(lm(R_C_df$subcal_Right~lik))$r.squared
# R_C_R_adj_lik[6,1] <- summary(lm(R_C_df$subcal_Right~lik))$adj.r.squared
# 
# R_C_R_adj_lik[,2] <- namesRC 
# R_C_R_squared_lik[,2] <- namesRC 
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_lik_R_adj.pdf')
# grid.table(R_C_R_adj_lik)
# dev.off()
# 
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_lik_R_squa.pdf')
# grid.table(R_C_R_squared_lik)
# dev.off()
# 
# # FOR INTENSITY
# 
# R_C_R_squared_int <- data_frame()
# R_C_R_adj_int <- data_frame()
# 
# #
# R_C_R_squared_int[1,1] <- summary(lm(R_C_df$AMY_Left~int))$r.squared
# R_C_R_adj_int[1,1] <- summary(lm(R_C_df$AMY_Left~int))$adj.r.squared
# 
# R_C_R_squared_int[2,1] <- summary(lm(R_C_df$AMY_Right~int))$r.squared
# R_C_R_adj_int[2,1] <- summary(lm(R_C_df$AMY_Right~int))$adj.r.squared
# 
# R_C_R_squared_int[3,1] <- summary(lm(R_C_df$Piri_Left~int))$r.squared
# R_C_R_adj_int[3,1] <- summary(lm(R_C_df$Piri_Left~int))$adj.r.squared
# 
# R_C_R_squared_int[4,1] <- summary(lm(R_C_df$Piri_Right~int))$r.squared
# R_C_R_adj_int[4,1] <- summary(lm(R_C_df$Piri_Right~int))$adj.r.squared
# 
# R_C_R_squared_int[5,1] <- summary(lm(R_C_df$subcal_Left~int))$r.squared
# R_C_R_adj_int[5,1] <- summary(lm(R_C_df$subcal_Left~int))$adj.r.squared
# 
# R_C_R_squared_int[6,1] <- summary(lm(R_C_df$subcal_Right~int))$r.squared
# R_C_R_adj_int[6,1] <- summary(lm(R_C_df$subcal_Right~int))$adj.r.squared
# 
# R_C_R_adj_int[,2] <- namesRC 
# R_C_R_squared_int[,2] <- namesRC 
# 
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_int_R_adj.pdf')
# grid.table(R_C_R_adj_int)
# dev.off()
# 
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_int_R_squa.pdf')
# grid.table(R_C_R_squared_int)
# dev.off()
# 
# 
# #  Get R.adj & R.squared for R_N ------------------------------------------
# 
# # FOR LIKING
# R_N_R_squared_lik <- data_frame()
# R_N_R_adj_lik <- data_frame()
# namesRN = c("Nacc_Left","Nacc_Right", "subcal_Left", "subcal_Right")
# 
# #
# R_N_R_squared_lik[1,1] <- summary(lm(R_N_df$Nacc_Left~lik))$r.squared
# R_N_R_adj_lik[1,1] <- summary(lm(R_N_df$Nacc_Left~lik))$adj.r.squared
# 
# R_N_R_squared_lik[2,1] <- summary(lm(R_N_df$Nacc_Right~lik))$r.squared
# R_N_R_adj_lik[2,1] <- summary(lm(R_N_df$Nacc_Right~lik))$adj.r.squared
# 
# R_N_R_squared_lik[3,1] <- summary(lm(R_N_df$subcal_Left~lik))$r.squared
# R_N_R_adj_lik[3,1] <- summary(lm(R_N_df$subcal_Left~lik))$adj.r.squared
# 
# R_N_R_squared_lik[4,1] <- summary(lm(R_N_df$subcal_Right~lik))$r.squared
# R_N_R_adj_lik[4,1] <- summary(lm(R_N_df$subcal_Right~lik))$adj.r.squared
# 
# R_N_R_adj_lik[,2] <- namesRN 
# R_N_R_squared_lik[,2] <- namesRN 
# 
# #
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_lik_R_adj.pdf')
# grid.table(R_N_R_adj_lik)
# dev.off()
# 
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_lik_R_squa.pdf')
# grid.table(R_N_R_squared_lik)
# dev.off()
# 
# 
# # FOR INTENSITY
# R_N_R_squared_int <- data_frame()
# R_N_R_adj_int<- data_frame()
# 
# #
# R_N_R_squared_int[1,1] <- summary(lm(R_N_df$Nacc_Left~int))$r.squared
# R_N_R_adj_int[1,1] <- summary(lm(R_N_df$Nacc_Left~int))$adj.r.squared
# 
# R_N_R_squared_int[2,1] <- summary(lm(R_N_df$Nacc_Right~int))$r.squared
# R_N_R_adj_int[2,1] <- summary(lm(R_N_df$Nacc_Right~int))$adj.r.squared
# 
# R_N_R_squared_int[3,1] <- summary(lm(R_N_df$subcal_Left~int))$r.squared
# R_N_R_adj_int[3,1] <- summary(lm(R_N_df$subcal_Left~int))$adj.r.squared
# 
# R_N_R_squared_int[4,1] <- summary(lm(R_N_df$subcal_Right~int))$r.squared
# R_N_R_adj_int[4,1] <- summary(lm(R_N_df$subcal_Right~int))$adj.r.squared
# 
# R_N_R_adj_int[,2] <- namesRN 
# R_N_R_squared_int[,2] <- namesRN 
# 
# 
# #
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_int_R_adj.pdf')
# grid.table(R_N_R_adj_int)
# dev.off()
# 
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_int_R_squa.pdf')
# grid.table(R_N_R_squared_int)
# dev.off()

## SUMMARY ##
# so basically contrast 
# reward_control -> lik&Amy + Piri + subcal Left & int&  AMY L+R + Piri right
# reward-neutral -> lik&Nacc left & int&Nacc right and subcal R+L
