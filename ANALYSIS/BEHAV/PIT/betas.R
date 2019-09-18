## R code for FOR REWOD_PIT
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot, ggpubr, gridExtra)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}


#SETUP
task = 'PIT'
con_name1 = 'CSp_CSm'

con1 = 'CSp-CSm'

mod1 = 'eff'



# Set working directory ---------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
setwd(analysis_path)

# open dataset 
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
eff_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)



# define factors
eff_df$ID <- factor(eff_df$ID)





# PLOT FUNCTIONS ----------------------------------------------------------


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}



# Plot CSp_CSm  -----------------------------------------------------------

# For effort

eff = eff_df$eff
A  <- ggplotRegression(lm(eff_df$Nacc_Left~eff)) + rremove("x.title")
B  <- ggplotRegression(lm(eff_df$Nacc_Right~eff)) + rremove("x.title")
C  <- ggplotRegression(lm(eff_df$vmPFC_Left~eff)) + rremove("x.title")
D  <- ggplotRegression(lm(eff_df$vmPFC_Right~eff)) + rremove("x.title")

figure1 <- ggarrange(A,B,C,D, 
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2) 
figure1 <- annotate_figure(figure1,
                           top = text_grob("Coeficient of determination: CSp - CSm for EFFORT", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_eff_coeff.pdf')
plot(figure4)
dev.off()


# CORRELATIONS ------------------------------------------------------------


corr_CSp_CSm.rcorr = rcorr(as.matrix(eff_df))
corr_CSp_CSm.coeff = corr_CSp_CSm.rcorr$r[2:5,6]
corr_CSp_CSm.p = corr_CSp_CSm.rcorr$P[2:5,6]

# PLOT CORR
pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp-CSm_corrplot.pdf')
corrplot(as.matrix(corr_CSp_CSm.coeff), method = "circle", tl.col = "black", tl.srt = 45)
dev.off()



# Get R.adj & R.squared for CSp-CSm ---------------------------------------


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


#
pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp-CSm_eff_R_adj.pdf')
grid.table(CSp_CSp_R_adj_eff)
dev.off()


pdf('~/REWOD/DERIVATIVES/BEHAVPIT/CSp-CSm_eff_R_squa.pdf')
grid.table(CSp_CSp_R_squared_eff)
dev.off()

## SUMMARY ##
# so basically
# CSp-CSm -> eff&Nacc R+L + vmPFC right

