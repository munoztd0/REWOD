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
task = 'hedonic'
con_name2 = 'R_NoR'
conEFF = 'CSp_CSm'
con2 = 'Reward_NoReward'
mod1 = 'lik'
mod2 = 'int'
mod3 = 'eff'

#

## R code for FOR REWOD_HED
# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
setwd(analysis_path)

# open dataset 
BETAS_R_NoR <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name2,'.txt',sep="")), header = T, sep ='\t') # read in dataset



LIK_R_NoR <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset



analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS/PIT') 
BETAS_CSp <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_', conEFF, '_via_', con_name2 , '.txt',sep="")), header = T, sep ='\t') # read in dataset
EFF_R_NoR <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste('CSp-CSm_', mod3, '_rank.txt',sep="")), header = T, sep ='\t')  # read in dataset


# merge
#R_NoR_CSp = merge(BETAS_R_NoR_CSp, LIK_R_NoR, by.x = "ID", by.y = "subj", all.x = TRUE)
#R_NoR_CSp = merge(R_NoR_CSp, INT_R_NoR, by.x = "ID", by.y = "subj", all.x = TRUE)

R_NoR_df = merge(BETAS_R_NoR, LIK_R_NoR, by.x = "ID", by.y = "subj", all.x = TRUE)
CSp_df = merge(BETAS_CSp, EFF_R_NoR, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
R_NoR_df$ID <- factor(R_NoR_df$ID)
CSp_df$ID <- factor(CSp_df$ID)
#R_NoR_CSp$ID <- factor(R_NoR_CSp$ID)



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




#  Plot for R_NoR  ----------------------------------------------------------

# For liking
#R_NoR_df <- filter(R_NoR_df, ID != "21") # or not !!!
lik = zscore(R_NoR_df$lik)
R_NoR_df$lik = zscore(lik)
A1 <- ggplotRegression(lm(R_NoR_df[[2]]~lik)) + rremove("x.title")
A2 <- ggplotRegression(lm(R_NoR_df[[4]]~lik)) + rremove("x.title")
A3 <- ggplotRegression(lm(CSp_df[[2]]~CSp_df$eff)) + rremove("x.title")
A4 <- ggplotRegression(lm(CSp_df[[4]]~CSp_df$eff)) + rremove("x.title")


figure1 <- ggarrange(A1,A2,A3,A4,
                     labels = c(  "cluster_Caud_Med_OFC",  "R_Caud_Med_OFC", "cluster_Caud_Med_OFC_EFF",  "R_Caud_Med_OFC_EFF"),
                     ncol = 2, nrow = 2,
                     vjust=3, hjust=0) 

figure1 <- annotate_figure(figure1,
                           top = text_grob("Coeficient of determination: Reward - No Reward for LIKING", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")



A2 <- ggplot(R_NoR_df, aes(lik, cmOFC_cluster_betas)) + #A2
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  #labs(title = paste("Adj R2 = ",signif(summary(lm(R_NoR_df$cmOFC_cluster_betas~lik))$adj.r.squared, 3),
                     #"Intercept =",signif(fit$coef[[1]],5 ),
                     #" Slope =",signif(fit$coef[[2]], 5),
                     #"  &  P =",signif(summary(lm(R_NoR_df$cmOFC_cluster_betas~lik))$coef[2,4], 3)))+
  scale_x_continuous(name="Hedonic experience", expand = c(0, 0), limits=c(-2, 2)) +
  scale_y_continuous(expression(paste(beta, "  Reward > Neutral+Control")), expand = c(0, 0), limits=c(-1, 1), breaks=c(seq.int(-1,1, by = 0.5))) +
  theme(plot.subtitle = element_text(size = 8, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)


cor.test(R_NoR_df$cmOFC_cluster_betas~lik)
#permutations
nperm <- 10000
x = R_NoR_df$cmOFC_cluster_betas
y = R_NoR_df$lik

cor.perm <- function (x, y, nperm)
{
  r.obs <- cor (x = x, y = y)
  P.par <- cor.test (x = x, y = y)$p.value
  # r.per <- replicate (nperm, expr = cor (x = x, y = sample (y)))
  r.per <- sapply (1:nperm, FUN = function (i) cor (x = x, y = sample (y)))
  r.per <- c(r.per, r.obs)
  hist (r.per, xlim = c(-1,1))
  abline (v = r.obs, col = 'red')
  P.per <- sum (abs (r.per) >= abs (r.obs))/(nperm + 1)
  return (list (r.obs = r.obs, P.par = P.par, P.per = P.per))
}

permut = cor.perm(x, y, nperm)


n = 24
k = 2

# Adj_R2_ = pirif _ int
rsq =signif(summary(lm(R_NoR_df$cmOFC_cluster_betas~lik))$adj.r.squared, 3)
# P = pirif _ int
p =permut$P.per

CI =CI.Rsq(rsq, n, k, level = 0.9)
paste("r² adj = ", signif(rsq,3),  ", 90% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]", ", p = ", round(p, 5))



