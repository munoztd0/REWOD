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
task = 'PIT'
con_name1 = 'CSp_CSm'
con_name2 = 'R_NoR'

con1 = 'CSp-CSm'
con2 = 'reward-neutral'

mod1 = 'eff'
mod2 = 'lik'


#

## R code for FOR REWOD_HED
# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS', task) 
setwd(analysis_path)

# open dataset 
BETAS_CSp <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset


EFF_CSp <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS/hedonic') 

BETAS_R_NoR <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name2, '_via_', con_name1, '.txt',sep="")), header = T, sep ='\t') # read in dataset
LIK_CSp    <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
R_NoR_df = merge(BETAS_R_NoR, LIK_CSp, by.x = "ID", by.y = "subj", all.x = TRUE)
#CSp_CSp = merge(CSp_CSp, INT_CSp, by.x = "ID", by.y = "subj", all.x = TRUE)

CSp_df = merge(BETAS_CSp, EFF_CSp, by.x = "ID", by.y = "subj", all.x = TRUE)



# define factors
CSp_df$ID <- factor(CSp_df$ID)
R_NoR_df$ID <- factor(R_NoR_df$ID)
#CSp_CSp$ID <- factor(CSp_CSp$ID)



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




#  Plot for CSp  ----------------------------------------------------------
CSp_df <- filter(CSp_df, ID != "6")  ## outlier
# For effing

eff = CSp_df$eff
eff = zscore(eff)
CSp_df$eff = zscore(eff)
A1 <- ggplotRegression(lm(CSp_df[[3]]~eff)) + rremove("x.title")
A2 <- ggplotRegression(lm(CSp_df[[4]]~eff)) + rremove("x.title")
A3 <- ggplotRegression(lm(CSp_df[[5]]~eff)) + rremove("x.title")
A4 <- ggplotRegression(lm(CSp_df[[6]]~eff)) + rremove("x.title")
A5 <- ggplotRegression(lm(R_NoR_df[[3]]~R_NoR_df$lik)) + rremove("x.title")
A6 <- ggplotRegression(lm(R_NoR_df[[4]]~R_NoR_df$lik)) + rremove("x.title")
A7 <- ggplotRegression(lm(R_NoR_df[[5]]~R_NoR_df$lik)) + rremove("x.title")
A8 <- ggplotRegression(lm(R_NoR_df[[6]]~R_NoR_df$lik)) + rremove("x.title")


figure1 <- ggarrange(A1,A2,A3,A4,A5,A6, A7, A8, 
                     labels = c( "LEFT_BLVP_betas",   "cluster_BLVP_L_betas"   ,  "cluster_core_right_betas", "pcore_RIGHT_betas", 
                                 "LEFT_BLVP_LIK"    ,      "cluster_BLVP_L_LIK"   ,  "cluster_core_right_LIK", "pcore_RIGHT_LIK"  ),
                     ncol = 2, nrow = 4,
                     vjust=3, hjust=0) 

figure1 <- annotate_figure(figure1,
                           top = text_grob("Coeficient of determination:CSp for EFFORT", color = "black", face = "bold", size = 14),
                           bottom = "Figure 1", fig.lab.face = "bold")


##BLVP##
A2 <- ggplot(CSp_df, aes(eff, LEFT_BLVP_betas)) + #A2
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  scale_x_continuous(name="Mobilized effort", expand = c(0, 0), limits=c(-2.5, 3)) +
  scale_y_continuous(expression(paste(beta, "  CS+ > CS-")), expand = c(0, 0), limits=c(-0.3, 0.3), breaks=c(seq.int(-0.5,0.3, by = 0.1))) +
  theme(plot.subtitle = element_text(size = 8, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)

A2
cor.test(CSp_df$LEFT_BLVP_betas~eff)
#permutations
nperm <- 10000
x = CSp_df$LEFT_BLVP_betas
y = CSp_df$eff

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

# Adj_R2_ = pirif _ int
rsq =signif(summary(lm(CSp_df$LEFT_BLVP_betas~eff))$adj.r.squared, 3)
# P = pirif _ int
p =permut$P.per

CI =CI.Rsq(rsq, n, k, level = 0.9)
paste("r² adj = ", signif(rsq,3),  ", 90% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]", ", p = ", round(p, 5))


####Core###    "cluster_core_right_betas", "pcore_RIGHT_betas"  ? Huuuuge outlier n⁶ 
CSp_df <- filter(CSp_df, ID != "6") # or not
A3 <- ggplot(CSp_df, aes(eff, pcore_RIGHT_betas)) + #A2
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  scale_x_continuous(name="Mobilized effort", expand = c(0, 0), limits=c(-2, 3)) +
  scale_y_continuous(expression(paste(beta, "  CS+ > CS-")), expand = c(0, 0), limits=c(-0.3, 0.4), breaks=c(seq.int(-0.5,0.4, by = 0.1))) +
  theme(plot.subtitle = element_text(size = 8, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)

A3
cor.test(CSp_df$pcore_RIGHT_betas~eff)
#permutations
nperm <- 10000
x = CSp_df$pcore_RIGHT_betas
y = CSp_df$eff

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

# Adj_R2_ = pirif _ int
rsq =signif(summary(lm(CSp_df$pcore_RIGHT_betas~eff))$adj.r.squared, 3)
# P = pirif _ int
p =permut$P.per

CI =CI.Rsq(rsq, n, k, level = 0.9)
paste("r² adj = ", signif(rsq,3),  ", 90% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]", ", p = ", round(p, 5))



