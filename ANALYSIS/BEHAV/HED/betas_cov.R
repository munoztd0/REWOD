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
task = 'hedonic'
#con_name1 = 'R_NoR'
con_name2 = 'R_N'
#con1 = 'Reward_NoReward'
con2 = 'reward-neutral'
mod1 = 'lik'
mod2 = 'int'


# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
setwd(analysis_path)

# open dataset 
#BETAS_R_NoR <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'_lik.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_R_N <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name2,'_int.txt',sep="")), header = T, sep ='\t') # read in dataset

#LIK_R_NoR <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
#LIK_R_NoR <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

#INT_R_NoR <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
INT_R_N <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset

# merge
#R_NoR_df = merge(BETAS_R_NoR, LIK_R_NoR, by.x = "ID", by.y = "subj", all.x = TRUE)
#R_NoR_df = merge(R_NoR_df, INT_R_NoR, by.x = "ID", by.y = "subj", all.x = TRUE)

R_N_int_df = merge(BETAS_R_N, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
#R_N_int_df = merge(R_N_df, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
#R_NoR_df$ID <- factor(R_NoR_df$ID)
R_N_int_df$ID <- factor(R_NoR_df$ID)



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


# PLOT FOR R_N_int ------------------------------------------------------------

# # For liking
# 
# lik = R_C_df$lik
# A <- ggplotRegression(lm(R_C_df[[2]]~lik)) + rremove("x.title")
# B <- ggplotRegression(lm(R_C_df[[3]]~lik)) + rremove("x.title")
# C <- ggplotRegression(lm(R_C_df[[4]]~lik)) + rremove("x.title")
# D <- ggplotRegression(lm(R_C_df[[5]]~lik)) + rremove("x.title")
# A1 <- ggplotRegression(lm(R_C_df[[6]]~lik)) + rremove("x.title")
# B1 <- ggplotRegression(lm(R_C_df[[7]]~lik))+ rremove("x.title")
# C1 <- ggplotRegression(lm(R_C_df[[8]]~lik)) + rremove("x.title")
# D1 <- ggplotRegression(lm(R_C_df[[9]]~lik)) + rremove("x.title")
# 
# 
# 
# figure1 <- ggarrange(A,B,C,D,A1,B1,C1,D1, 
#                      labels = c("A: AMY_BLA_L", "B: AMY_BM_L", "C: CAUD_VENTR_L", "D", "E", "F"),
#                      ncol = 2, nrow = 3) 
# 
# figure1 <- annotate_figure(figure1,
#                 top = text_grob("Coefficient of determination: REWARD-CONTROL for LIKING", color = "black", face = "bold", size = 14),
#                 bottom = "Figure 1", fig.lab.face = "bold")
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_lik_coeff.pdf')
# plot(figure1)
# dev.off()
# 
# 
# For intensity

int = R_N_int_df$int
A <- ggplotRegression(lm(R_N_int_df[[2]]~int)) + rremove("x.title")
B <- ggplotRegression(lm(R_N_int_df[[3]]~int)) + rremove("x.title")
C <- ggplotRegression(lm(R_N_int_df[[4]]~int)) + rremove("x.title")
D <- ggplotRegression(lm(R_N_int_df[[5]]~int)) + rremove("x.title")
E <- ggplotRegression(lm(R_N_int_df[[6]]~int)) + rremove("x.title")


figure2 <- ggarrange(A,B,C,D,E,
                     labels = c("G AMY_ant_L", "H AMY_ant_R", "I AMY_post_R", "J midbrain", "K vmPFC_R", "L"),
                     ncol = 2, nrow = 3)

figure2 <- annotate_figure(figure2,
                top = text_grob("Coeficient of determination: REWARD-NEUTRAL int for INTENSITY", color = "black", face = "bold", size = 14),
                bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_INT_int_coeff.pdf')
plot(figure2)
dev.off()


#  Plot for R_NoR  ----------------------------------------------------------

# For liking

# lik = R_NoR_df$lik
# A <- ggplotRegression(lm(R_NoR_df[[2]]~lik)) + rremove("x.title")
# B <- ggplotRegression(lm(R_NoR_df[[3]]~lik)) + rremove("x.title")
# C <- ggplotRegression(lm(R_NoR_df[[4]]~lik)) + rremove("x.title")
# D <- ggplotRegression(lm(R_NoR_df[[5]]~lik)) + rremove("x.title")
# E <- ggplotRegression(lm(R_NoR_df[[6]]~lik)) + rremove("x.title")
# A1 <- ggplotRegression(lm(R_NoR_df[[7]]~lik)) + rremove("x.title")
# B1 <- ggplotRegression(lm(R_NoR_df[[8]]~lik)) + rremove("x.title")
# C1 <- ggplotRegression(lm(R_NoR_df[[9]]~lik)) + rremove("x.title")
# D1 <- ggplotRegression(lm(R_NoR_df[[10]]~lik)) + rremove("x.title")
# E1 <- ggplotRegression(lm(R_NoR_df[[11]]~lik)) + rremove("x.title")
# 
# 
# figure3 <- ggarrange(A,B,C,D,E,A1,B1,C,D1,E1,
#                      labels = c("A: CAUD_L", "B: FRONTAL_R", "C: IFG_L", "D: IFG_R", "E: OPER_L", "F: PUT_L", "G: SUBCAL_R", "G: SUBCAL_R", "G: midebrain", "H: vmPFC_L", "I: vlPFC_R"),
#                      ncol = 2, nrow = 5,
#                      vjust=3, hjust=-1) 
# 
# figure3 <- annotate_figure(figure3,
#                            top = text_grob("Coeficient of determination: REWARD-NEUTRAL for LIKING", color = "black", face = "bold", size = 14),
#                            bottom = "Figure 1", fig.lab.face = "bold")
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_NoR_lik_coeff.pdf')
# plot(figure3)
# dev.off()


# For intensity

# int = R_NoR_df$int
# A2 <- ggplotRegression(lm(R_NoR_df[[2]]~int)) + rremove("x.title")
# B2 <- ggplotRegression(lm(R_NoR_df[[3]]~int)) + rremove("x.title")
# C2 <- ggplotRegression(lm(R_NoR_df[[4]]~int)) + rremove("x.title")
# D2 <- ggplotRegression(lm(R_NoR_df[[5]]~int)) + rremove("x.title")
# A3 <- ggplotRegression(lm(R_NoR_df[[6]]~int)) + rremove("x.title")
# B3 <- ggplotRegression(lm(R_NoR_df[[7]]~int))+ rremove("x.title")
# 
# figure4 <- ggarrange(A2,B2,C2,D2,A3,B3,
#                      labels = c("A: AMY_BLA_L", "B: AMY_full_L", "C: CAUD_VENTR_L", "D: PUT_L", "E: PUT_R", "F: caud_PAULI", "G: nacc_PAULI"),
#                      ncol = 2, nrow = 3)
# 
# figure4 <- annotate_figure(figure4,
#                            top = text_grob("Coeficient of determination: REWARD-NEUTRAL for INTENSITY", color = "black", face = "bold", size = 14),
#                            bottom = "Figure 1", fig.lab.face = "bold")
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_NoR_int_coeff.pdf')
# plot(figure4)
# dev.off()

# # CORRELATIONS R_C------------------------------------------------------------
# 
# corr_R_C.rcorr = rcorr(as.matrix(R_C_df))
# corr_R_C.coeff = corr_R_C.rcorr$r[2:7,8:9]
# corr_R_C.p = corr_R_C.rcorr$P[2:7,8:9]
# 
# # PLOT CORR
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_corrplot.pdf')
# corrplot(corr_R_C.coeff , method = "circle",tl.col = "black", tl.srt = 45)
# dev.off()


# CORRELATIONS R_NoR ------------------------------------------------------

# corr_R_NoR.rcorr = rcorr(as.matrix(R_NoR_df))
# corr_R_NoR.coeff = corr_R_NoR.rcorr$r[2:11,12]
# corr_R_NoR.p = corr_R_NoR.rcorr$P[2:11,12]
# 
# col3 <- colorRampPalette(c("blue", "white", "red")) 
# # PLOT CORR
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_NoR_corrplot.pdf')
# corrplot(corr_R_NoR.coeff , method = "circle",tl.col = "black", tl.srt = 45, col = col3(20))
# dev.off()


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
# #  Get R.adj & R.squared for R_NoR ------------------------------------------
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
