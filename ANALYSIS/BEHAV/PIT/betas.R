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

#for lik
taskHED = 'hedonic'

conHED = 'reward-neutral'

modLIK = 'lik'

modINT = 'int'




# Set working directory ---------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
analysis_pathHED <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', taskHED) 
setwd(analysis_path)

# open dataset 
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset

LIK_R_N <- read.delim(file.path(analysis_pathHED, 'GLM-04', 'group_covariates', paste(conHED,'_', modLIK, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_R_N <- read.delim(file.path(analysis_pathHED, 'GLM-04', 'group_covariates', paste(conHED,'_', modINT, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset


# merge
CSp_CSm_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)
CSp_CSm_df = merge(CSp_CSm_df, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
CSp_CSm_df = merge(CSp_CSm_df, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)





# PLOT FUNCTIONS ----------------------------------------------------------

ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       #"Intercept =",signif(fit$coef[[1]],5 ),
                       #" Slope =",signif(fit$coef[[2]], 5),
                       "  &  P =",signif(summary(fit)$coef[2,4], 5))) +
    theme(plot.title = element_text(size = 10, hjust =1))
  
}



# Plot CSp_CSm  -----------------------------------------------------------

# For effort

eff = CSp_CSm_df$eff
A1  <- ggplotRegression(lm(CSp_CSm_df[[2]]~eff)) + rremove("x.title")
B1  <- ggplotRegression(lm(CSp_CSm_df[[3]]~eff)) + rremove("x.title")
C1  <- ggplotRegression(lm(CSp_CSm_df[[4]]~eff)) + rremove("x.title")
D1  <- ggplotRegression(lm(CSp_CSm_df[[5]]~eff)) + rremove("x.title")
E1  <- ggplotRegression(lm(CSp_CSm_df[[6]]~eff)) + rremove("x.title")
F1  <- ggplotRegression(lm(CSp_CSm_df[[8]]~eff)) + rremove("x.title")


figure1 <- ggarrange(A1,B1,C1,D1,E1,F1,
                     labels = c("A: AMY_BM_L", "B: AMY_full_L","C: CAUD_ANT_R", "D: CAUD_VENTR_L", "E: CAUD_VENTR_R", "F: NACC_R"),
                     ncol = 2, nrow = 3,
                     vjust=3, hjust=0) 

figure1 <- annotate_figure(figure1,
                           top = text_grob("Coeficient of determination: CSp - CSm for EFFORT", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_eff_coeff.pdf')
plot(figure1)
dev.off()

# For liking

lik = CSp_CSm_df$lik
A2  <- ggplotRegression(lm(CSp_CSm_df[[2]]~lik)) + rremove("x.title")
B2  <- ggplotRegression(lm(CSp_CSm_df[[3]]~lik)) + rremove("x.title")
C2  <- ggplotRegression(lm(CSp_CSm_df[[4]]~lik)) + rremove("x.title")
D2  <- ggplotRegression(lm(CSp_CSm_df[[5]]~lik)) + rremove("x.title")
E2  <- ggplotRegression(lm(CSp_CSm_df[[6]]~lik)) + rremove("x.title")
F2  <- ggplotRegression(lm(CSp_CSm_df[[8]]~lik)) + rremove("x.title")


figure2 <- ggarrange(A2,B2,C2,D2,E2,F2,
                     labels = c("A: AMY_BM_L", "B: AMY_full_L","C: CAUD_ANT_R", "D: CAUD_VENTR_L", "E: CAUD_VENTR_R", "F: NACC_R"),
                     ncol = 2, nrow = 3,
                     vjust=3, hjust=0) 

figure2 <- annotate_figure(figure2,
                           top = text_grob("Coeficient of determination: CSp - CSm for LIKING", color = "black", face = "bold", size = 14),
                           bottom = "Figure 2", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_lik_coeff.pdf')
plot(figure2)
dev.off()

# For intensity

int = CSp_CSm_df$int
A3  <- ggplotRegression(lm(CSp_CSm_df[[2]]~int)) + rremove("x.title")
B3  <- ggplotRegression(lm(CSp_CSm_df[[3]]~int)) + rremove("x.title")
C3  <- ggplotRegression(lm(CSp_CSm_df[[4]]~int)) + rremove("x.title")
D3  <- ggplotRegression(lm(CSp_CSm_df[[5]]~int)) + rremove("x.title")
E3  <- ggplotRegression(lm(CSp_CSm_df[[6]]~int)) + rremove("x.title")
F3  <- ggplotRegression(lm(CSp_CSm_df[[8]]~int)) + rremove("x.title")


figure3 <- ggarrange(A3,B3,C3,D3,E3,F3,
                     labels = c("A: AMY_BM_L", "B: AMY_full_L","C: CAUD_ANT_R", "D: CAUD_VENTR_L", "E: CAUD_VENTR_R", "F: NACC_R"),
                     ncol = 2, nrow = 3,
                     vjust=3, hjust=0) 

figure3 <- annotate_figure(figure3,
                           top = text_grob("Coeficient of determination: CSp - CSm for INTENISTY", color = "black", face = "bold", size = 14),
                           bottom = "Figure 3", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_int_coeff.pdf')
plot(figure3)
dev.off()


#just AMY_BM_L
figure4 <- ggarrange(A1,A2,A3,
                     labels = c("  A: Effort", 
                                "  B: Liking", 
                                "  C: Intensity"),
                     ncol = 1, nrow = 3,
                     vjust=1, hjust=0) 

figure4 <- annotate_figure(figure4,
                           top = text_grob("Coeficient of determination: CSp - CSm for AMY_BM", color = "black", face = "bold", size = 14),
                           bottom = "Figure 4", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_BM_coeff.pdf')
plot(figure4)
dev.off()



# CORRELATIONS ------------------------------------------------------------


corr_CSp_CSm.rcorr = rcorr(as.matrix(CSp_CSm_df))
corr_CSp_CSm.coeff = corr_CSp_CSm.rcorr$r[c(2:6,8),12:14]
corr_CSp_CSm.p = corr_CSp_CSm.rcorr$P[c(2:6,8),12:14]

col3 <- colorRampPalette(c("blue", "white", "red")) 

# PLOT CORR
pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_corrplot.pdf')
corrplot(corr_CSp_CSm.coeff, method = "circle", tl.col = "black", tl.srt = 45, col = col3(20))
dev.off()



# Get R.adj & R.squared for CSp-CSm ---------------------------------------


CSp_CSm_R_squared_eff <- data_frame()
CSp_CSm_R_adj_eff<- data_frame()
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

