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
#con_name1 = 'R_C'
con_name2 = 'R_N'
#con1 = 'reward-control'
con2 = 'reward-neutral'
mod1 = 'lik'
mod2 = 'int'

conEFF = 'CSp_CSm'

# #for eff
# taskEFF = 'PIT'
# 
# 
# 
# modEFF = 'eff'


# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
setwd(analysis_path)

# open dataset 
#BETAS_R_C <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_R_N <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name2,'.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_R_N_CSp <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name2, '_via_', conEFF, '.txt',sep="")), header = T, sep ='\t') # read in dataset


#LIK_R_C <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
LIK_R_N <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

#INT_R_C <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
INT_R_N <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset

#EFF     <- read.delim(file.path(analysis_pathEFF, 'GLM-04', 'group_covariates', paste(conEFF,'_', modEFF, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset

# merge
R_N_CSp = merge(BETAS_R_N_CSp, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
R_N_CSp = merge(R_N_CSp, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
#R_N_CSp = merge(R_N_CSp, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)

R_N_df = merge(BETAS_R_N, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
R_N_df = merge(R_N_df, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
#R_N_df = merge(R_N_df, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
#R_C_df$ID <- factor(R_C_df$ID)
R_N_df$ID <- factor(R_N_df$ID)
R_N_CSp$ID <- factor(R_N_CSp$ID)



# PLOT FUNCTIONS --------------------------------------------------------------------


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       #"Intercept =",signif(fit$coef[[1]],5 ),
                       #" Slope =",signif(fit$coef[[2]], 5),
                       "  &  P =",signif(summary(fit)$coef[2,4], 5)))+
    theme(plot.title = element_text(size = 10, hjust =1))
           
}


# PLOT FOR R_C ------------------------------------------------------------

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
# A2 <- ggplotRegression(lm(R_C_df[[10]]~lik)) + rremove("x.title")
# B2 <- ggplotRegression(lm(R_C_df[[11]]~lik))+ rremove("x.title")
# C2 <- ggplotRegression(lm(R_C_df[[12]]~lik)) + rremove("x.title")
# D2 <- ggplotRegression(lm(R_C_df[[13]]~lik)) + rremove("x.title")
# A3 <- ggplotRegression(lm(R_C_df[[14]]~lik)) + rremove("x.title")
# B3 <- ggplotRegression(lm(R_C_df[[15]]~lik))+ rremove("x.title")
# 
# 
# 
# figure1 <- ggarrange(A,B,C,D,A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,
#                      labels = c("A: AMY_BLA_L", "B: AMY_BLA_R", "C: AMY_PIRI_L", "D: AMY_PIRI_R", "E:  AMY_full_L", "F: caud_ant_L", "D: caud_ant_R", 
#                     "G: thala_L", "H: thala_R", "I: nacct_L", "j: nacc_R", "k: Put_R", "l: caud_pauli", "m: nacc_pauli"),
#                     ncol = 2, nrow = 7,
#                     vjust=3, hjust=0)
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
# # For intensity
# 
# int = R_C_df$int
# A <- ggplotRegression(lm(R_C_df[[2]]~int)) + rremove("x.title")
# B <- ggplotRegression(lm(R_C_df[[3]]~int)) + rremove("x.title")
# C <- ggplotRegression(lm(R_C_df[[4]]~int)) + rremove("x.title")
# D <- ggplotRegression(lm(R_C_df[[5]]~int)) + rremove("x.title")
# A1 <- ggplotRegression(lm(R_C_df[[6]]~int)) + rremove("x.title")
# B1 <- ggplotRegression(lm(R_C_df[[7]]~int))+ rremove("x.title")
# C1 <- ggplotRegression(lm(R_C_df[[8]]~int)) + rremove("x.title")
# D1 <- ggplotRegression(lm(R_C_df[[9]]~int)) + rremove("x.title")
# A2 <- ggplotRegression(lm(R_C_df[[10]]~int)) + rremove("x.title")
# B2 <- ggplotRegression(lm(R_C_df[[11]]~int))+ rremove("x.title")
# C2 <- ggplotRegression(lm(R_C_df[[12]]~int)) + rremove("x.title")
# D2 <- ggplotRegression(lm(R_C_df[[13]]~int)) + rremove("x.title")
# A3 <- ggplotRegression(lm(R_C_df[[14]]~int)) + rremove("x.title")
# B3 <- ggplotRegression(lm(R_C_df[[15]]~int))+ rremove("x.title")
# 
# 
# 
# figure2 <- ggarrange(A,B,C,D,A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,
#                      labels = c("A: AMY_BLA_L", "B: AMY_BLA_R", "C: AMY_PIRI_L", "D: AMY_PIRI_R", "E:  AMY_full_L", "F: caud_ant_L", "D: caud_ant_R", 
#                                 "G: thala_L", "H: thalam_R", "I: nacct_L", "j: nacc_R", "k: Put_R", "l: caud_pauli", "m: nacc_pauli"),
#                      ncol = 2, nrow = 7,
#                      vjust=3, hjust=0)
# 
# figure2 <- annotate_figure(figure2,
#                 top = text_grob("Coeficient of determination: REWARD-CONTROL for INTENSITY", color = "black", face = "bold", size = 14),
#                 bottom = "Figure 1", fig.lab.face = "bold")
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_int_coeff.pdf')
# plot(figure2)
# dev.off()


#  Plot for R_N  ----------------------------------------------------------

# For liking

lik = R_N_df$lik
A1 <- ggplotRegression(lm(R_N_df[[2]]~lik)) + rremove("x.title")
B1 <- ggplotRegression(lm(R_N_df[[3]]~lik)) + rremove("x.title")
C1 <- ggplotRegression(lm(R_N_df[[4]]~lik)) + rremove("x.title")
D1 <- ggplotRegression(lm(R_N_df[[5]]~lik)) + rremove("x.title")


figure1 <- ggarrange(A1,B1,C1,D1,
                     labels = c("A: AMY_BLA_L", "B: AMY_full_L", "C: CAUD_VENTR_L", "D: PUT_L"),
                     ncol = 2, nrow = 2,
                     vjust=3, hjust=0) 

figure1 <- annotate_figure(figure1,
                top = text_grob("Coeficient of determination: REWARD-NEUTRAL for LIKING", color = "black", face = "bold", size = 14),
                bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_lik_coeff.pdf')
plot(figure1)
dev.off()



# For intensity

int = R_N_df$int
A2 <- ggplotRegression(lm(R_N_df[[2]]~int)) + rremove("x.title")
B2 <- ggplotRegression(lm(R_N_df[[3]]~int)) + rremove("x.title")
C2 <- ggplotRegression(lm(R_N_df[[4]]~int)) + rremove("x.title")
D2 <- ggplotRegression(lm(R_N_df[[5]]~int)) + rremove("x.title")


figure2 <- ggarrange(A2,B2,C2,D2,
                     labels = c("A: AMY_BLA_L", "B: AMY_full_L", "C: CAUD_VENTR_L", "D: PUT_L"),
                     vjust=3, hjust=0,
                     ncol = 2, nrow = 2)

figure2 <- annotate_figure(figure2,
                top = text_grob("Coeficient of determination: REWARD-NEUTRAL for INTENSITY", color = "black", face = "bold", size = 14),
                bottom = "Figure 2", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_int_coeff.pdf')
plot(figure2)
dev.off()


#  Plot for R_N_CSp  ----------------------------------------------------------

# For liking

lik = R_N_CSp$lik
A3 <- ggplotRegression(lm(R_N_CSp[[2]]~lik)) + rremove("x.title")
B3 <- ggplotRegression(lm(R_N_CSp[[3]]~lik)) + rremove("x.title")
C3 <- ggplotRegression(lm(R_N_CSp[[4]]~lik)) + rremove("x.title")
D3 <- ggplotRegression(lm(R_N_CSp[[5]]~lik)) + rremove("x.title")
E3 <- ggplotRegression(lm(R_N_CSp[[6]]~lik)) + rremove("x.title")
F3 <- ggplotRegression(lm(R_N_CSp[[8]]~lik)) + rremove("x.title")


figure3 <- ggarrange(A3,B3,C3,D3,E3,F3,
                     labels = c("A: AMY_BM_L", "B: AMY_full_L","C: CAUD_ANT_R", "D: CAUD_VENTR_L", "E: CAUD_VENTR_R", "F: NACC_R"),
                     ncol = 2, nrow = 3,
                     vjust=3, hjust=0) 

figure3 <- annotate_figure(figure3,
                           top = text_grob("Coeficient of determination: REWARD-NEUTRAL from CSp-CSm ROIs for LIKING", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_CSpROIs_lik_coeff.pdf')
plot(figure3)
dev.off()



# For intensity

int = R_N_CSp$int
A4 <- ggplotRegression(lm(R_N_CSp[[2]]~int)) + rremove("x.title")
B4 <- ggplotRegression(lm(R_N_CSp[[3]]~int)) + rremove("x.title")
C4 <- ggplotRegression(lm(R_N_CSp[[4]]~int)) + rremove("x.title")
D4 <- ggplotRegression(lm(R_N_CSp[[5]]~int)) + rremove("x.title")
E4 <- ggplotRegression(lm(R_N_CSp[[6]]~int)) + rremove("x.title")
F4 <- ggplotRegression(lm(R_N_CSp[[8]]~int)) + rremove("x.title")


figure4 <- ggarrange(A4,B4,C4,D4,E4,F4,
                     labels = c("A: AMY_BM_L", "B: AMY_full_L","C: CAUD_ANT_R", "D: CAUD_VENTR_L", "E: CAUD_VENTR_R", "F: NACC_R"),
                     ncol = 2, nrow = 3,
                     vjust=3, hjust=0) 

figure4 <- annotate_figure(figure4,
                           top = text_grob("Coeficient of determination: REWARD-NEUTRAL from CSp-CSm ROIs for INTENSITY", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_CSpROIs_int_coeff.pdf')
plot(figure4)
dev.off()


#just AMY_BLA
figure4 <- ggarrange(A1,A2,A3,
                     labels = c("  A: Liking", 
                                "  B: Intensity", 
                                "  C: Effort"),
                     ncol = 1, nrow = 3,
                     vjust=1, hjust=0) 

figure4 <- annotate_figure(figure4,
                           top = text_grob("Coeficient of determination: REWARD-NEUTRAL for AMY_BLA", color = "black", face = "bold", size = 14),
                           bottom = "Figure 4", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_BLA_coeff.pdf')
plot(figure4)
dev.off()


# CORRELATIONS R_C ------------------------------------------------------

# corr_R_C.rcorr = rcorr(as.matrix(R_C_df))
# corr_R_C.coeff = corr_R_C.rcorr$r[2:15,16:17]
# corr_R_C.p = corr_R_C.rcorr$P[2:15,16:17]
# 
# col3 <- colorRampPalette(c("blue", "white", "red")) 
# # PLOT CORR
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_corrplot.pdf')
# corrplot(corr_R_C.coeff , method = "circle",tl.col = "black", tl.srt = 45, col = col3(20))
# dev.off()



# CORRELATIONS R_N ------------------------------------------------------

corr_R_N.rcorr = rcorr(as.matrix(R_N_df))
corr_R_N.coeff = corr_R_N.rcorr$r[2:5,8:10]
corr_R_N.p = corr_R_N.rcorr$P[2:5,8:10]

col3 <- colorRampPalette(c("blue", "white", "red")) 
# PLOT CORR

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_corrplot.pdf')
corrplot(corr_R_N.coeff , method = "circle",tl.col = "black", tl.srt = 45, col = col3(20))
dev.off()

# CORRELATIONS R_N ------------------------------------------------------

corr_R_N.rcorr = rcorr(as.matrix(R_N_CSp))
corr_R_N.coeff = corr_R_N.rcorr$r[2:5,8:10]
corr_R_N.p = corr_R_N.rcorr$P[2:5,8:10]

col3 <- colorRampPalette(c("blue", "white", "red")) 
# PLOT CORR

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_corrplot.pdf')
corrplot(corr_R_N.coeff , method = "circle",tl.col = "black", tl.srt = 45, col = col3(20))
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
