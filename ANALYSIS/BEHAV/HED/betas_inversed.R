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
taskHED = 'hedonic'
taskPIT = 'PIT'

con_name1 = 'R_N'

con1 = 'CSp-CSm'

mod1 = 'eff'



# Set working directory ---------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', taskHED) 
analysis_path_EFF <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', taskPIT) 
setwd(analysis_path)

# open dataset 
BETAS_R_N <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_inversed_', con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path_EFF, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
eff_df = merge(BETAS_R_N, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)



# define factors
eff_df$ID <- factor(eff_df$ID)





# PLOT FUNCTIONS ----------------------------------------------------------

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



# Plot R_N  -----------------------------------------------------------

# For effort

eff = eff_df$eff
A  <- ggplotRegression(lm(eff_df[[2]]~eff)) + rremove("x.title")
B  <- ggplotRegression(lm(eff_df[[3]]~eff)) + rremove("x.title")
C  <- ggplotRegression(lm(eff_df[[4]]~eff)) + rremove("x.title")
D  <- ggplotRegression(lm(eff_df[[5]]~eff)) + rremove("x.title")
A1  <- ggplotRegression(lm(eff_df[[6]]~eff)) + rremove("x.title")
B1  <- ggplotRegression(lm(eff_df[[7]]~eff)) + rremove("x.title")
C1  <- ggplotRegression(lm(eff_df[[8]]~eff)) + rremove("x.title")
D1  <- ggplotRegression(lm(eff_df[[9]]~eff)) + rremove("x.title")
E  <- ggplotRegression(lm(eff_df[[10]]~eff)) + rremove("x.title")
E1  <- ggplotRegression(lm(eff_df[[11]]~eff)) + rremove("x.title")

figure1 <- ggarrange(A,B,C,D,A1,B1,C1,D1, E, E1,
                     labels = c("A: AMY_BM_L", "H: AMY_full_L","B: CAUD_ANT_R", "C: CAUD_VENTR_L", "D: CAUD_VENTR_R","E: NACC_L", "F: NACC_R", "G: NACC_cluster_R", "I: PUT_R", "J: NAcc_pauli"),
                     ncol = 2, nrow = 5,
                     vjust=3, hjust=-1) 

figure1 <- annotate_figure(figure1,
                           top = text_grob("Coeficient of determination: CSp - CSm for EFFORT", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_eff_coeff.pdf')
plot(figure1)
dev.off()


# CORRELATIONS ------------------------------------------------------------


corr_R_N.rcorr = rcorr(as.matrix(eff_df))
corr_R_N.coeff = corr_R_N.rcorr$r[2:11,12]
corr_R_N.p = corr_R_N.rcorr$P[2:11,12]

col3 <- colorRampPalette(c("blue", "white", "red")) 

# PLOT CORR
pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_eff_corrplot.pdf')
corrplot(as.matrix(corr_R_N.coeff), method = "circle", tl.col = "black", tl.srt = 45, col = col3(20))
dev.off()



# Get R.adj & R.squared for CSp-CSm ---------------------------------------


R_N_R_squared_eff <- data_frame()
R_N_R_adj_eff<- data_frame()
#namesEff = c("Nacc_Left","Nacc_Right", "vmPFC_Left", "vmPFC_Right")

#CSp_CSm_R_squared_eff[1,1] <- summary(lm(eff_df$Nacc_Left~eff))$r.squared
#CSp_CSm_R_adj_eff[1,1] <- summary(lm(eff_df$Nacc_Left~eff))$adj.r.squared

#CSp_CSm_R_squared_eff[2,1] <- summary(lm(eff_df$Nacc_Right~eff))$r.squared
#CSp_CSm_R_adj_eff[2,1] <- summary(lm(eff_df$Nacc_Right~eff))$adj.r.squared

#CSp_CSm_R_squared_eff[3,1] <- summary(lm(eff_df$vmPFC_Left~eff))$r.squared
#CSp_CSm_R_adj_eff[3,1] <- summary(lm(eff_df$vmPFC_Left~eff))$adj.r.squared

#CSp_CSm_R_squared_eff[4,1] <- summary(lm(eff_df$vmPFC_Right~eff))$r.squared
#CSp_CSm_R_adj_eff[4,1] <- summary(lm(eff_df$vmPFC_Right~eff))$adj.r.squared

#CSp_CSm_R_squared_eff[,2] <- namesEff
#CSp_CSm_R_adj_eff[,2] <- namesEff


#
#pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp-CSm_eff_R_adj.pdf')
#grid.table(CSp_CSm_R_adj_eff)
#dev.off()


#pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp-CSm_eff_R_squa.pdf')
#grid.table(CSp_CSm_R_squared_eff)
#dev.off()

## SUMMARY ##
# so basically
# CSp-CSm -> eff&Nacc R+L + vmPFC right

