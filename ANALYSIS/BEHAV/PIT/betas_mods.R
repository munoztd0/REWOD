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
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',mod1,'.txt',sep="")), header = T, sep ='\t') # read in dataset

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
                       #"Intercept =",signif(fit$coef[[1]],5 ),
                       #" Slope =",signif(fit$coef[[2]], 5),
                       "  &  P =",signif(summary(fit)$coef[2,4], 5))) +
    theme(plot.title = element_text(size = 10))
  
}



# Plot CSp_CSm  -----------------------------------------------------------

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
A2  <- ggplotRegression(lm(eff_df[[10]]~eff)) + rremove("x.title")



figure1 <- ggarrange(A,B,C,D,A1,B1,C1,D1,A2,
                     labels = c("CAUD_ANT_RIGHT_betas", "CAUD_VENTR_LEFT_betas", "CAUD_VENTR_RIGHT_betas",
                      "CLOSTRUM_LEFT_betas",    "FRONTAL_LEFT_betas",     "FRONTAL_RIGHT_betas",    "OFC_LEFT_betas" ,       
                     "OFC_RIGHT_betas","SUBCAL_LEFT_betas"),
                     ncol = 2, nrow = 5,
                     vjust=3, hjust=0) 

figure1 <- annotate_figure(figure1,
                           top = text_grob("Coeficient of determination: CSp - CSm for EFFORT", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/eff1_coeff.pdf')
plot(figure1)
dev.off()

#secondpart
B2  <- ggplotRegression(lm(eff_df[[11]]~eff)) + rremove("x.title")
C2  <- ggplotRegression(lm(eff_df[[12]]~eff)) + rremove("x.title")
D2  <- ggplotRegression(lm(eff_df[[13]]~eff)) + rremove("x.title")
A3  <- ggplotRegression(lm(eff_df[[14]]~eff)) + rremove("x.title")
B3  <- ggplotRegression(lm(eff_df[[15]]~eff)) + rremove("x.title")
C3  <- ggplotRegression(lm(eff_df[[16]]~eff)) + rremove("x.title")
D3  <- ggplotRegression(lm(eff_df[[17]]~eff)) + rremove("x.title")
A4  <- ggplotRegression(lm(eff_df[[18]]~eff)) + rremove("x.title")

figure2 <- ggarrange(B2,C2,D2,A3,B3,C3,D3,A4,
                     labels = c("SUBCAL_RIGHT_betas", "aINS_RIGHT_betas" ,"dlPFC_LEFT_betas" ,      "dlPFC_RIGHT_betas" ,     "pINS_LEFT_betas",        "pINS_RIGHT_betas" ,     
                      "vmPFC_LEFT_betas",       "vmPFC_RIGHT_betas"),
                     ncol = 2, nrow = 4,
                     vjust=3, hjust=0) 

figure2 <- annotate_figure(figure2,
                           top = text_grob("Coeficient of determination: CSp - CSm for EFFORT", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")

pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/eff2_coeff.pdf')
plot(figure2)
dev.off()

# CORRELATIONS ------------------------------------------------------------


corr_CSp_CSm.rcorr = rcorr(as.matrix(eff_df))
corr_CSp_CSm.coeff = corr_CSp_CSm.rcorr$r[2:18,19]
corr_CSp_CSm.p = corr_CSp_CSm.rcorr$P[2:18,19]

col3 <- colorRampPalette(c("blue", "white", "red")) 

# PLOT CORR
pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/eff_corrplot.pdf')
corrplot(as.matrix(corr_CSp_CSm.coeff), method = "circle", tl.col = "black", tl.srt = 45, col = col3(20))
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

