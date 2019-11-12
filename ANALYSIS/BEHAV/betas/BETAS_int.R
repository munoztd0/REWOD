## R code for FOR REWOD GENERAL
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(grid,gridExtra,magick, ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot, ggpubr, gridExtra, mosaic, psychometric)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

#SETUP
taskHED = 'hedonic'
taskPIT = 'PIT'
con_name = 'AMY'
con_name2 = 'Od_NoOd'
con1 = 'CSp-CSm'
con2 = 'Odor-NoOdor'
mod1 = 'eff'
mod2 = 'int'
mod3 = 'lik'



## R code for FOR REWOD_HED
# Set working directory -------------------------------------------------


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS') 
setwd(analysis_path)

# open dataset 
BETAS_O_N <- read.delim(file.path(analysis_path, taskHED, 'ROI', paste('extracted_betas_',con_name2,'.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_O_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset

LIK_O_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(con2,'_', mod3, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset



O_N_df = merge(BETAS_O_N, INT_O_N, by.x = "ID", by.y = "subj", all.x = TRUE)
O_N_df = merge(O_N_df, LIK_O_N, by.x = "ID", by.y = "subj", all.x = TRUE)

# define factors
O_N_df$ID <- factor(O_N_df$ID)




# open dataset 
BETAS_CSp_CSm <- read.delim(file.path(analysis_path, taskPIT, 'ROI', paste('extracted_betas_',con_name,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, taskPIT, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
CSp_CSm_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)

# zscore
CSp_CSm_df$eff = zscore(CSp_CSm_df$eff)

O_N_df$int = zscore(O_N_df$int )
O_N_df$lik = zscore(O_N_df$lik )


# PLOT FUNCTIONS --------------------------------------------------------------------



# A2 <- ggplot(O_N_df, aes(lik, AMY_AAA_betas)) + 
#   geom_point( size = 1) +
#   geom_smooth(method = "lm", col = "orange", fullrange = T) +
#   #labs(subtitle = paste("Adj R2 = ",signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$lik))$adj.r.squared, 3),
#   #"likercept =",signif(fit$coef[[1]],5 ),
#   #" Slope =",signif(fit$coef[[2]], 5),
#   #"  &  P =",signif(summary(lm(O_N_df$AMY_BLA_LEFT_betas~O_N_df$lik))$coef[2,4], 3)))+
#   scale_x_continuous(name="Hedonic experience", limits=c(-2.02, 3.02)) +
#   scale_y_continuous(name=expression(paste(beta, "  Reward + Neutral > Control")), limits=c(-0.5, 2)) +
#   theme(plot.subtitle = element_text(size = 8, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)
# 
# A3 <- ggplot(CSp_CSm_df, aes(eff, AMY_AAA_betas)) + 
#   geom_point(size = 1) +
#   geom_smooth(method = "lm", col = "orange", fullrange = T) +
#   #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_RN$AMY_AAA_betas~CSp_CSm_RN$eff))$adj.r.squared, 3),
#   #"Intercept =",signif(fit$coef[[1]],5 ),
#   #" Slope =",signif(fit$coef[[2]], 5),
#   #"  &  P =",signif(summary(lm(CSp_CSm_df$AMY_AAA_betas~CSp_CSm_df$eff))$coef[2,4], 3)))+
#   scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 3.02)) +
#   scale_y_continuous(name=expression(paste(beta, "  CSp > CSm")), limits=c(-0.5, 2)) +
#   theme(plot.subtitle = element_text(size = 8,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)

# 
# figure1 <- ggarrange(A1, 
#                      #labels = c("B: Coeficients of determination"),
#                      ncol = 2, nrow = 1) 

AMY_pirif = image_read_pdf('~/REWOD/DERIVATIVES/BEHAV/FIGURES/neat/AMY_pirif.pdf')

A4 <-  rasterGrob(AMY_pirif, interpolate=TRUE)


# 
# figure2 <- ggarrange(A4, figure1,
#                      labels = c("A","B"),
#                      vjust=1.5, hjust = -1,
#                      ncol = 1, nrow = 2) 

ggarrange(A4,                                                 # First row with scatter plot
          ggarrange(A1,  ncol = 2, labels = c("B", "C", "D"), vjust=-1), # Second row with box and dot plots
          nrow = 1, 
          labels = "A",
          hjust =0 # Labels of the scatter plot
) 

# signif ------------------------------------------------------------------

cor.test(O_N_df$AMY_AAA_betas~O_N_df$int)
#permutations
nperm <- 10000
x = O_N_df$AMY_AAA_betas
y = O_N_df$int

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
rsq =signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$int))$adj.r.squared, 3)
# P = pirif _ int
p =permut$P.per

CI =CI.Rsq(rsq, n, k, level = 0.9)
paste("r² adj = ", signif(rsq,3),  ", 90% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]", ", p = ", round(p, 5))


A1 <- ggplot(O_N_df, aes(int, AMY_AAA_betas)) + #A2
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "orange", fullrange = F) +
  #labs(subtitle = paste("Adj R2 = ",signif(summary(lm(O_N_df$AMY_AAA_betas~O_N_df$int))$adj.r.squared, 3),   "  &  P =", round(p, 4)))+
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  scale_x_continuous(name="Perceived intensity", expand = c(0, 0), limits=c(-2.02, 3.0), breaks=c(seq.int(-2,3, by = 1))) +
  scale_y_continuous(expression(paste(beta, "  Reward + Neutral > Control")), expand = c(0, 0), limits=c(-1, 2), breaks=c(seq.int(-1,2, by = 0.5))) +
  theme(plot.subtitle = element_text(size = 8, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)


# wihtout ouliter ---------------------------------------------------------

O_N_df2 <- filter(O_N_df, ID != "13")
x = O_N_df2$AMY_AAA_betas
y = O_N_df2$int

permut = cor.perm(x, y, nperm)

# Adj_R2_ = pirif _ int
rsq =signif(summary(lm(O_N_df2$AMY_AAA_betas~O_N_df2$int))$r.squared, 3)
# P = pirif _ int
p =permut$P.per

CI =CI.Rsq(rsq, n, k, level = 0.90)
paste("r² adj = ", signif(rsq,3),  ", 90% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]", ", p = ", signif(p,3))


# 
# A2 <- ggplot(O_N_df2, aes(int, AMY_AAA_betas)) + #A2
#   geom_point( size = 1) +
#   geom_smooth(method = "lm", col = "orange", fullrange = T) +
#   labs(subtitle = paste("Adj R2 = ",signif(summary(lm(O_N_df2$AMY_AAA_betas~O_N_df2$int))$adj.r.squared, 3),   "  &  P =", round(p, 4)))+
#   #"Intercept =",signif(fit$coef[[1]],5 ),
#   #" Slope =",signif(fit$coef[[2]], 5),
#   #"  &  P =",signif(summary(lm(O_N_df$AMY_BLA_LEFT_betas~O_N_df$lik))$coef[2,4], 3)))+
#   scale_x_continuous(name="Perceived intensity", limits=c(-2.02, 3.02)) +
#   scale_y_continuous(expression(paste(beta, "  Reward + Neutral > Control")), limits=c(-0.5, 2)) +
#   theme(plot.subtitle = element_text(size = 8, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)