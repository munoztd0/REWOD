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
con_name1 = 'R_C'
con_name2 = 'R_N'
con1 = 'reward-control'
con2 = 'reward-neutral'
mod1 = 'lik'
mod2 = 'int'


# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
setwd(analysis_path)

# open dataset 
BETAS_R_C <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_R_N <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name2,'.txt',sep="")), header = T, sep ='\t') # read in dataset

LIK_R_C <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
LIK_R_N <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_R_C <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
INT_R_N <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset

# merge
R_C_df = merge(BETAS_R_C, LIK_R_C, by.x = "ID", by.y = "subj", all.x = TRUE)
R_C_df = merge(R_C_df, INT_R_C, by.x = "ID", by.y = "subj", all.x = TRUE)

R_N_df = merge(BETAS_R_N, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
R_N_df = merge(R_N_df, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
R_C_df$ID <- factor(R_C_df$ID)
R_N_df$ID <- factor(R_N_df$ID)



# PLOT FUNCTIONS --------------------------------------------------------------------


# plot perceived_likings by time with regression lign
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


# PLOT FOR R_C ------------------------------------------------------------

# For liking

lik = R_C_df$lik
A <- ggplotRegression(lm(R_C_df$AMY_Left~lik)) + rremove("x.title")
B <- ggplotRegression(lm(R_C_df$AMY_Right~lik)) + rremove("x.title")
C <- ggplotRegression(lm(R_C_df$Piri_Left~lik)) + rremove("x.title")
D <- ggplotRegression(lm(R_C_df$Piri_Right~lik)) + rremove("x.title")
E <- ggplotRegression(lm(R_C_df$subcal_Left~lik)) + rremove("x.title")
G <- ggplotRegression(lm(R_C_df$subcal_Right~lik))+ rremove("x.title")

figure1 <- ggarrange(A,B,C,D,E,G, 
                     labels = c("A", "B", "C", "D", "E", "F"),
                     ncol = 2, nrow = 3) 

figure1 <- annotate_figure(figure1,
                top = text_grob("Coefficient of determination: REWARD-CONTROL for LIKING", color = "black", face = "bold", size = 14),
                bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_lik_coeff.pdf')
plot(figure1)
dev.off()


# For intensity

int = R_C_df$int
H <- ggplotRegression(lm(R_C_df$AMY_Left~int)) + rremove("x.title")
I <- ggplotRegression(lm(R_C_df$AMY_Right~int)) + rremove("x.title")
J <- ggplotRegression(lm(R_C_df$Piri_Left~int)) + rremove("x.title")
K <- ggplotRegression(lm(R_C_df$Piri_Right~int)) + rremove("x.title")
L <- ggplotRegression(lm(R_C_df$subcal_Left~int)) + rremove("x.title")
M <- ggplotRegression(lm(R_C_df$subcal_Right~int)) + rremove("x.title")


figure2 <- ggarrange(H,I,J,K,L,M, 
                     labels = c("G", "H", "I", "J", "K", "L"),
                     ncol = 2, nrow = 3) 

figure2 <- annotate_figure(figure2,
                top = text_grob("Coeficient of determination: REWARD-CONTROL for INTENSITY", color = "black", face = "bold", size = 14),
                bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_int_coeff.pdf')
plot(figure2)
dev.off()


#  Plot for R_N  ----------------------------------------------------------

# For liking

lik = R_N_df$lik
N <- ggplotRegression(lm(R_N_df$Nacc_Left~lik)) + rremove("x.title")
O <- ggplotRegression(lm(R_N_df$Nacc_Right~lik)) + rremove("x.title")
P <- ggplotRegression(lm(R_N_df$subcal_Left~lik)) + rremove("x.title")
Q <- ggplotRegression(lm(R_N_df$subcal_Right~lik)) + rremove("x.title")


figure3 <- ggarrange(N,O,P,Q, 
                     labels = c("M", "N", "O", "P"),
                     ncol = 2, nrow = 3) 

figure3 <- annotate_figure(figure3,
                top = text_grob("Coeficient of determination: REWARD-NEUTRAL for LIKING", color = "black", face = "bold", size = 14),
                bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_lik_coeff.pdf')
plot(figure3)
dev.off()


# For intensity

int = R_N_df$int
R <- ggplotRegression(lm(R_N_df$Nacc_Left~int)) + rremove("x.title")
S <- ggplotRegression(lm(R_N_df$Nacc_Right~int)) + rremove("x.title")
U <- ggplotRegression(lm(R_N_df$subcal_Left~int)) + rremove("x.title")
V <-  ggplotRegression(lm(R_N_df$subcal_Right~int)) + rremove("x.title")

figure4 <- ggarrange(R,S,U,V, 
                     labels = c("Q", "R", "S", "T"),
                     ncol = 2, nrow = 3) 

figure4 <- annotate_figure(figure4,
                top = text_grob("Coeficient of determination: REWARD-NEUTRAL for INTENSITY", color = "black", face = "bold", size = 14),
                bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_int_coeff.pdf')
plot(figure4)
dev.off()

# CORRELATIONS R_C------------------------------------------------------------

corr_R_C.rcorr = rcorr(as.matrix(R_C_df))
corr_R_C.coeff = corr_R_C.rcorr$r[2:7,8:9]
corr_R_C.p = corr_R_C.rcorr$P[2:7,8:9]

# PLOT CORR

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_corrplot.pdf')
corrplot(corr_R_C.coeff , method = "circle",tl.col = "black", tl.srt = 45)
dev.off()


# CORRELATIONS R_N ------------------------------------------------------

corr_R_N.rcorr = rcorr(as.matrix(R_N_df))
corr_R_N.coeff = corr_R_N.rcorr$r[2:5,6:7]
corr_R_N.p = corr_R_N.rcorr$P[2:5,6:7]

# PLOT CORR

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_corrplot.pdf')
corrplot(corr_R_N.coeff , method = "circle",tl.col = "black", tl.srt = 45)
dev.off()


#  Get R.adj & R.squared for R_C ------------------------------------------


# FOR LIKING
R_C_R_squared_lik <- data_frame()
R_C_R_adj_lik <- data_frame()
namesRC = c("AMY_Left","AMY_Right", "Piri_Left", "Piri_Right", "subcal_Left", "subcal_Right")

#
R_C_R_squared_lik[1,1] <- summary(lm(R_C_df$AMY_Left~lik))$r.squared
R_C_R_adj_lik[1,1] <- summary(lm(R_C_df$AMY_Left~lik))$adj.r.squared

R_C_R_squared_lik[2,1] <- summary(lm(R_C_df$AMY_Right~int))$r.squared
R_C_R_adj_lik[2,1] <- summary(lm(R_C_df$AMY_Right~lik))$adj.r.squared

R_C_R_squared_lik[3,1] <- summary(lm(R_C_df$Piri_Left~lik))$r.squared
R_C_R_adj_lik[3,1] <- summary(lm(R_C_df$Piri_Left~lik))$adj.r.squared

R_C_R_squared_lik[4,1] <- summary(lm(R_C_df$Piri_Right~lik))$r.squared
R_C_R_adj_lik[4,1] <- summary(lm(R_C_df$Piri_Right~lik))$adj.r.squared

R_C_R_squared_lik[5,1] <- summary(lm(R_C_df$subcal_Left~lik))$r.squared
R_C_R_adj_lik[5,1] <- summary(lm(R_C_df$subcal_Left~lik))$adj.r.squared

R_C_R_squared_lik[6,1] <- summary(lm(R_C_df$subcal_Right~lik))$r.squared
R_C_R_adj_lik[6,1] <- summary(lm(R_C_df$subcal_Right~lik))$adj.r.squared

R_C_R_adj_lik[,2] <- namesRC 
R_C_R_squared_lik[,2] <- namesRC 

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_lik_R_adj.pdf')
grid.table(R_C_R_adj_lik)
dev.off()


pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_lik_R_squa.pdf')
grid.table(R_C_R_squared_lik)
dev.off()

# FOR INTENSITY

R_C_R_squared_int <- data_frame()
R_C_R_adj_int <- data_frame()

#
R_C_R_squared_int[1,1] <- summary(lm(R_C_df$AMY_Left~int))$r.squared
R_C_R_adj_int[1,1] <- summary(lm(R_C_df$AMY_Left~int))$adj.r.squared

R_C_R_squared_int[2,1] <- summary(lm(R_C_df$AMY_Right~int))$r.squared
R_C_R_adj_int[2,1] <- summary(lm(R_C_df$AMY_Right~int))$adj.r.squared

R_C_R_squared_int[3,1] <- summary(lm(R_C_df$Piri_Left~int))$r.squared
R_C_R_adj_int[3,1] <- summary(lm(R_C_df$Piri_Left~int))$adj.r.squared

R_C_R_squared_int[4,1] <- summary(lm(R_C_df$Piri_Right~int))$r.squared
R_C_R_adj_int[4,1] <- summary(lm(R_C_df$Piri_Right~int))$adj.r.squared

R_C_R_squared_int[5,1] <- summary(lm(R_C_df$subcal_Left~int))$r.squared
R_C_R_adj_int[5,1] <- summary(lm(R_C_df$subcal_Left~int))$adj.r.squared

R_C_R_squared_int[6,1] <- summary(lm(R_C_df$subcal_Right~int))$r.squared
R_C_R_adj_int[6,1] <- summary(lm(R_C_df$subcal_Right~int))$adj.r.squared

R_C_R_adj_int[,2] <- namesRC 
R_C_R_squared_int[,2] <- namesRC 


pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_int_R_adj.pdf')
grid.table(R_C_R_adj_int)
dev.off()


pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_C_int_R_squa.pdf')
grid.table(R_C_R_squared_int)
dev.off()


#  Get R.adj & R.squared for R_N ------------------------------------------

# FOR LIKING
R_N_R_squared_lik <- data_frame()
R_N_R_adj_lik <- data_frame()
namesRN = c("Nacc_Left","Nacc_Right", "subcal_Left", "subcal_Right")

#
R_N_R_squared_lik[1,1] <- summary(lm(R_N_df$Nacc_Left~lik))$r.squared
R_N_R_adj_lik[1,1] <- summary(lm(R_N_df$Nacc_Left~lik))$adj.r.squared

R_N_R_squared_lik[2,1] <- summary(lm(R_N_df$Nacc_Right~lik))$r.squared
R_N_R_adj_lik[2,1] <- summary(lm(R_N_df$Nacc_Right~lik))$adj.r.squared

R_N_R_squared_lik[3,1] <- summary(lm(R_N_df$subcal_Left~lik))$r.squared
R_N_R_adj_lik[3,1] <- summary(lm(R_N_df$subcal_Left~lik))$adj.r.squared

R_N_R_squared_lik[4,1] <- summary(lm(R_N_df$subcal_Right~lik))$r.squared
R_N_R_adj_lik[4,1] <- summary(lm(R_N_df$subcal_Right~lik))$adj.r.squared

R_N_R_adj_lik[,2] <- namesRN 
R_N_R_squared_lik[,2] <- namesRN 

#
pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_lik_R_adj.pdf')
grid.table(R_N_R_adj_lik)
dev.off()


pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_lik_R_squa.pdf')
grid.table(R_N_R_squared_lik)
dev.off()


# FOR INTENSITY
R_N_R_squared_int <- data_frame()
R_N_R_adj_int<- data_frame()

#
R_N_R_squared_int[1,1] <- summary(lm(R_N_df$Nacc_Left~int))$r.squared
R_N_R_adj_int[1,1] <- summary(lm(R_N_df$Nacc_Left~int))$adj.r.squared

R_N_R_squared_int[2,1] <- summary(lm(R_N_df$Nacc_Right~int))$r.squared
R_N_R_adj_int[2,1] <- summary(lm(R_N_df$Nacc_Right~int))$adj.r.squared

R_N_R_squared_int[3,1] <- summary(lm(R_N_df$subcal_Left~int))$r.squared
R_N_R_adj_int[3,1] <- summary(lm(R_N_df$subcal_Left~int))$adj.r.squared

R_N_R_squared_int[4,1] <- summary(lm(R_N_df$subcal_Right~int))$r.squared
R_N_R_adj_int[4,1] <- summary(lm(R_N_df$subcal_Right~int))$adj.r.squared

R_N_R_adj_int[,2] <- namesRN 
R_N_R_squared_int[,2] <- namesRN 


#
pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_int_R_adj.pdf')
grid.table(R_N_R_adj_int)
dev.off()


pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_int_R_squa.pdf')
grid.table(R_N_R_squared_int)
dev.off()

## SUMMARY ##
# so basically contrast 
# reward_control -> lik&Amy + Piri + subcal Left & int&  AMY L+R + Piri right
# reward-neutral -> lik&Nacc left & int&Nacc right and subcal R+L
