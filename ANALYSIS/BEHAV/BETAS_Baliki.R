## R code for FOR REWOD GENERAL
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot, psychometric, ggpubr, gridExtra, grid, mosaic, magick, pdftools, devtools, margins, patchwork)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}


analysis_path <- file.path('~/REWOD/DERIVATIVES/ANALYSIS') 
setwd(analysis_path)


#SETUP
taskHED = 'hedonic'
taskPIT = 'PIT'
con_nameHED = 'R_N'
con_namePIT = 'CSp_CSm'
conHED = 'reward-neutral'
conPIT = 'CSp-CSm'
mod1 = 'lik'
mod2 = 'int'
mod3 = 'eff'
ROI = 'Baliki'



## R code for FOR REWOD_HED


# open dataset 
#BETAS_R_N <- read.delim(file.path(analysis_path, taskHED, 'ROI', paste('extracted_betas_',con_nameHED,'.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_R_N_df <- read.delim(file.path(analysis_path, taskHED,'ROI', paste('extracted_betas_',con_nameHED, '_via_', ROI, '.txt',sep="")), header = T, sep ='\t') # read in dataset

LIK_R_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(conHED,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_R_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(conHED,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset


# merge
R_N_df = merge(BETAS_R_N_df, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
R_N_df = merge(R_N_df, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)

#R_N_df = merge(BETAS_R_N, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
#R_N_df = merge(R_N_df, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
#R_N_df$ID <- factor(R_N_df$ID)
R_N_df$ID <- factor(R_N_df$ID)

#R_NoR_df <- filter(R_NoR_df, ID != "24")
## R code for FOR REWOD_PIT


# open dataset 
#BETAS_CSp_CSm <- read.delim(file.path(analysis_path, taskPIT, 'ROI', paste('extracted_betas_',con_namePIT,'.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_CSp_CSm_df <- read.delim(file.path(analysis_path, taskPIT, 'ROI', paste('extracted_betas_',con_namePIT,'_via_', ROI,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, taskPIT, 'GLM-04', 'group_covariates', paste(conPIT,'_', mod3, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
CSp_CSm_df = merge(BETAS_CSp_CSm_df, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# merge
#CSp_CSm_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)
#CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)

# zscore
CSp_CSm_df$eff = zscore(CSp_CSm_df$eff)
#CSp_CSm_df$eff = zscore(CSp_CSm_df$eff)


R_N_CSp$lik = zscore(R_N_CSp$lik )
#R_N_df$lik = zscore(R_N_df$lik )



# PLOT FUNCTIONS --------------------------------------------------------------------

# 
# ggplotRegression <- function (fit) {
#   
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point() +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        #"Intercept =",signif(fit$coef[[1]],5 ),
#                        #" Slope =",signif(fit$coef[[2]], 5),
#                        "  &  P =",signif(summary(fit)$coef[2,4], 5)))+
#     theme(plot.title = element_text(size = 10, hjust =1))
#   
# }
# 
# # poooto ------------------------------------------------------------------
# 
# lik = R_N_df$lik
# lik = zscore(lik)
# 
# A1 <- ggplotRegression(lm(R_N_df[[2]]~lik)) + rremove("x.title")
# B1 <- ggplotRegression(lm(R_N_df[[3]]~lik)) + rremove("x.title")
# C1 <- ggplotRegression(lm(R_N_df[[4]]~lik)) + rremove("x.title")
# D1 <- ggplotRegression(lm(R_N_df[[5]]~lik)) + rremove("x.title")
# 
# 
# figure1 <- ggarrange(A1,B1,C1, D1,
#                      labels = c( "pcore_L" ,  "pcore_R" , "pshell_L" , "pshell_R"),
#                      ncol = 2, nrow = 2,
#                      vjust=3, hjust=0) 
# 
# 
# figure1 <- annotate_figure(figure1,
#                            top = text_grob("Coeficient of determination: Reward-Neutral via Baliki for LIKING", color = "black", face = "bold", size = 14),
#                            bottom = "Figure 1", fig.lab.face = "bold")
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_via_baliki_lik_coeff.pdf')
# plot(figure1)
# dev.off()
# 
# 
# # int ---------------------------------------------------------------------
# 
# 
# int = R_N_df$int
# int = zscore(int)
# 
# A1 <- ggplotRegression(lm(R_N_df[[2]]~int)) + rremove("x.title")
# B1 <- ggplotRegression(lm(R_N_df[[3]]~int)) + rremove("x.title")
# C1 <- ggplotRegression(lm(R_N_df[[4]]~int)) + rremove("x.title")
# D1 <- ggplotRegression(lm(R_N_df[[5]]~int)) + rremove("x.title")
# 
# 
# figure2 <- ggarrange(A1,B1,C1, D1,
#                      labels = c( "pcore_L" ,  "pcore_R" , "pshell_L" , "pshell_R"),
#                      ncol = 2, nrow = 2,
#                      vjust=3, hjust=0) 
# 
# 
# figure2 <- annotate_figure(figure2,
#                            top = text_grob("Coeficient of determination: Reward-Neutral via Bailiki for int", color = "black", face = "bold", size = 14),
#                            bottom = "Figure 1", fig.lab.face = "bold")
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/HED/R_N_via_baliki_int_coeff.pdf')
# plot(figure2)
# dev.off()
# 
# 
# # eff ---------------------------------------------------------------------
# 
# 
# eff = CSp_CSm_df$eff
# eff = zscore(eff)
# 
# R_N_df$lik = zscore(R_N_df$lik )
# 
# A1 <- ggplotRegression(lm(CSp_CSm_df[[2]]~eff)) + rremove("x.title")
# B1 <- ggplotRegression(lm(CSp_CSm_df[[3]]~eff)) + rremove("x.title")
# C1 <- ggplotRegression(lm(CSp_CSm_df[[4]]~eff)) + rremove("x.title")
# D1 <- ggplotRegression(lm(CSp_CSm_df[[5]]~eff)) + rremove("x.title")
# 
# 
# figure3 <- ggarrange(A1,B1,C1, D1,
#                      labels = c( "pcore_L" ,  "pcore_R" , "pshell_L" , "pshell_R"),
#                      ncol = 2, nrow = 2,
#                      vjust=3, hjust=0) 
# 
# 
# figure3 <- annotate_figure(figure3,
#                            top = text_grob("Coeficient of determination: CSp-CSm via Bailiki for eff", color = "black", face = "bold", size = 14),
#                            bottom = "Figure 1", fig.lab.face = "bold")
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/PIT/CSp_CSm_via_baliki_eff_coeff.pdf')
# plot(figure3)
# dev.off()
# 
# 
# A4 <- ggplot(CSp_CSm_df, aes(eff, pcore_RIGHT_betas)) + 
#   geom_point(size = 1) +
#   geom_smooth(method = "lm", col = "red", fullrange = T) +
#   #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$adj.r.squared, 3),
#   #"Intercept =",signif(fit$coef[[1]],5 ),
#   #" Slope =",signif(fit$coef[[2]], 5),
#   #"  &  P =",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$coef[2,4], 3)))+
#   scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 2.02)) +
#   scale_y_continuous(name="Beta CSp > CSm", limits=c(-0.3, 0.5)) +
#   theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)
# 
# figure4 <- annotate_figure(A4,
#                   top = text_grob("Coeficient of determination:\nRight Putative Core of the Nucleus Accumbens", color = "black", face = "bold", size = 18, vjust=0.5, hjust=0.5),
#                   bottom = text_grob("Figure 4. Radj =  0.139385,  p = 0.0408", color = "black", face = "bold")) #, vjust = 0,  hjust = 4))
# 
# 
# pdf('~/REWOD/DERIVATIVES/BEHAV/pcore_RIGHT_eff.pdf')
# plot(figure4)
# dev.off()




A1 <- ggplot(CSp_CSm_df, aes(eff, pcore_RIGHT_betas)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "red", fullrange = T) +
  #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$coef[2,4], 3)))+
  scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 2.02)) +
  scale_y_continuous(name = expression(paste(beta, "  CSp > CSm")), limits=c(-0.3, 0.5)) +
  theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)

A2 <- ggplot(CSp_CSm_df, aes(eff, pshell_RIGHT_betas)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "blue", fullrange = T) +
  #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$coef[2,4], 3)))+
  scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 2.02)) +
  scale_y_continuous(name = expression(paste(beta, "  CSp > CSm")), limits=c(-0.3, 0.5)) +
  theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)

A3 <- ggplot(R_N_df, aes(lik, pcore_RIGHT_betas)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "red", fullrange = T) +
  #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  #"  &  P =",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$coef[2,4], 3)))+
  scale_x_continuous(name="Hedonic experience") + #, limits=c(-2.02, 2.02)) +
  scale_y_continuous(name = expression(paste(beta, "  Reward > Neutral"))) + #, limits=c(-1, 1)) +
  theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)



baliki = image_read_pdf('~/REWOD/DERIVATIVES/BEHAV/FIGURES/neat/core_shell.pdf')

A4 <-  rasterGrob(baliki, interpolate=TRUE)



# arranging ---------------------------------------------------------------


figure1 <- ggarrange(A1, A2, A3,
                     #labels = c("B: Coeficients of determination"),
                     ncol = 3, nrow = 1) 

# figure2 <- ggarrange(A3, A4,
#                      #labels = c("B: Coeficients of determination"),
#                      ncol = 2, nrow = 1) 

figure3 <- ggarrange(A4, figure1,
                     labels = c(" A"," B"),
                     vjust=1.5, hjust = 0,
                     ncol = 1, nrow = 2) 


ggarrange(A4,                                                 # First row with scatter plot
          ggarrange(A1, A2, A3, ncol = 3, labels = c("B", "C", "D")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A"                                        # Labels of the scatter plot
) 

# 
# figure4 <- annotate_figure(A4,
#                            top = text_grob("Coeficient of determination:\nRight Putative Core of the Nucleus Accumbens", color = "black", face = "bold", size = 18, vjust=0.5, hjust=0.5),
#                            bottom = text_grob("Figure 4. Radj =  0.139385,  p = 0.0408", color = "black", face = "bold")) #, vjust = 0,  hjust = 4))



# signif ------------------------------------------------------------------


n = 24
k = 2

# Adj_R2_ = pirif _ eff
rsq =signif(summary(lm(CSp_CSm_df$pcore_RIGHT_betas~CSp_CSm_df$eff))$r.squared, 3)
# P = pirif _ eff
p = signif(summary(lm(CSp_CSm_df$pcore_RIGHT_betas~CSp_CSm_df$eff))$coef[2,4], 3)

CI =CI.Rsq(rsq, n, k, level = 0.95)
paste("r² = ", signif(rsq,3), ", p = ", signif(p,3), ", 95% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]")


# 1 -----------------------------------------------------------------------


# Adj_R2_ = pirif _ lik
rsq =signif(summary(lm(CSp_CSm_df$pshell_RIGHT_betas~CSp_CSm_df$eff))$r.squared, 3)
# P = pirif _ eff
p = signif(summary(lm(CSp_CSm_df$pshell_RIGHT_betas~CSp_CSm_df$eff))$coef[2,4], 3)

CI =CI.Rsq(rsq, n, k, level = 0.95)
paste("r² = ", signif(rsq,3), ", p = ", signif(p,3), ", 95% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]")


# 1 -----------------------------------------------------------------------


# Adj_R2_ = pirif _ eff
rsq =signif(summary(lm(R_N_df$pcore_RIGHT_betas~R_N_df$lik))$r.squared, 3)
# P = pirif _ lik
p = signif(summary(lm(R_N_df$pcore_RIGHT_betas~R_N_df$lik))$coef[2,4], 3)

CI =CI.Rsq(rsq, n, k, level = 0.95)
paste("r² = ", signif(rsq,3), ", p = ", signif(p,3), ", 95% CI [", signif(CI$LCL,3), ",", signif(CI$UCL,3), "]")




