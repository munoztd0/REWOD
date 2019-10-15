## R code for FOR REWOD GENERAL
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, corrplot, ggpubr, gridExtra, grid, mosaic, magick, pdftools, devtools, margins, patchwork)

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
ROI = 'Prevost'



## R code for FOR REWOD_HED


# open dataset 
#BETAS_R_N <- read.delim(file.path(analysis_path, taskHED, 'ROI', paste('extracted_betas_',con_nameHED,'.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_R_N_CSp <- read.delim(file.path(analysis_path, taskHED,'ROI', paste('extracted_betas_',con_nameHED, '_via_', ROI, '.txt',sep="")), header = T, sep ='\t') # read in dataset

LIK_R_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(conHED,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_R_N <- read.delim(file.path(analysis_path, taskHED, 'GLM-04', 'group_covariates', paste(conHED,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset


# merge
R_N_CSp = merge(BETAS_R_N_CSp, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
R_N_CSp = merge(R_N_CSp, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)

#R_N_df = merge(BETAS_R_N, LIK_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)
#R_N_df = merge(R_N_df, INT_R_N, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
#R_N_df$ID <- factor(R_N_df$ID)
R_N_CSp$ID <- factor(R_N_CSp$ID)

#R_NoR_df <- filter(R_NoR_df, ID != "24")
## R code for FOR REWOD_PIT


# open dataset 
#BETAS_CSp_CSm <- read.delim(file.path(analysis_path, taskPIT, 'ROI', paste('extracted_betas_',con_namePIT,'.txt',sep="")), header = T, sep ='\t') # read in dataset
BETAS_CSp_CSm_RN <- read.delim(file.path(analysis_path, taskPIT, 'ROI', paste('extracted_betas_',con_namePIT,'_via_', ROI,'.txt',sep="")), header = T, sep ='\t') # read in dataset

EFF <- read.delim(file.path(analysis_path, taskPIT, 'GLM-04', 'group_covariates', paste(conPIT,'_', mod3, '_rank.txt',sep="")), header = T, sep ='\t') # read in dataset


# merge
CSp_CSm_RN = merge(BETAS_CSp_CSm_RN, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# merge
#CSp_CSm_df = merge(BETAS_CSp_CSm, EFF, by.x = "ID", by.y = "subj", all.x = TRUE)


# define factors
CSp_CSm_RN$ID <- factor(CSp_CSm_RN$ID)
#CSp_CSm_df$ID <- factor(CSp_CSm_df$ID)

# zscore
CSp_CSm_RN$eff = zscore(CSp_CSm_RN$eff)
#CSp_CSm_df$eff = zscore(CSp_CSm_df$eff)


R_N_CSp$lik = zscore(R_N_CSp$lik )
#R_N_df$lik = zscore(R_N_df$lik )


# PLOT BLA--------------------------------------------------------------------


A1 <- ggplot(R_N_CSp, aes(lik, AMY_BLA_specific_betas)) + #A2
  geom_point( size = 1) +
  geom_smooth(method = "lm", col = "green", fullrange = T) +
  labs(subtitle = paste(#"Adj R2 = ",signif(summary(lm(R_N_df$AMY_BLA_specific_betas~R_N_df$lik))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  "  &  P =",signif(summary(lm(R_N_CSp$AMY_BLA_specific_betas~R_N_CSp$lik))$coef[2,4], 3)))+
  #scale_x_continuous(name="Hedonic pleasure", limits=c(-2.02, 2.02)) +
  #scale_y_continuous(name="Beta Reward > neutral", limits=c(-0.6, 0.6)) +
  theme(plot.subtitle = element_text(size = 10, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), margin = NULL, aspect.ratio=1)

A2 <- ggplot(CSp_CSm_RN, aes(eff, AMY_BLA_specific_betas)) + #A2
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "green", fullrange = T) +
  labs(subtitle= paste(#"Adj R2 = ",signif(summary(lm(CSp_CSm_RN$AMY_BLA_LEFT_betas~CSp_CSm_RN$eff))$adj.r.squared, 3),
  #"Intercept =",signif(fit$coef[[1]],5 ),
  #" Slope =",signif(fit$coef[[2]], 5),
  "  &  P =",signif(summary(lm(CSp_CSm_RN$AMY_BLA_specific_betas~CSp_CSm_RN$eff))$coef[2,4], 3)))+
  #scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 2.02)) +
  #scale_y_continuous(name="Beta CSp > CSm", limits=c(-0.6, 0.6)) +
  theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)



# 
# A3 <- ggplot(R_N_CSp, aes(lik, AMY_BLVP_LEFT_betas)) + #A2
#   geom_point(size = 1) +
#   geom_smooth(method = "lm", col = "red", fullrange = T) +
#   #labs(subtitle = paste("Adj R2 = ",signif(summary(lm(R_N_CSp$AMY_BLVP_LEFT_betas~R_N_CSp$lik))$adj.r.squared, 3),
#   #"Intercept =",signif(fit$coef[[1]],5 ),
#   #" Slope =",signif(fit$coef[[2]], 5),
#   #"  &  P =",signif(summary(lm(R_N_CSp$AMY_BLVP_LEFT_betas~R_N_CSp$lik))$coef[2,4], 3)))+
#   scale_x_continuous(name="Hedonic pleasure", limits=c(-2.02, 2.02)) +
#   scale_y_continuous(name="Beta Reward > neutral", limits=c(-0.6, 0.6)) +
#   theme(plot.subtitle = element_text(size = 10, vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)
# 
# 
# A4 <- ggplot(CSp_CSm_df, aes(eff, AMY_BLVP_LEFT_betas)) + #A2
#   geom_point(size = 1) +
#   geom_smooth(method = "lm", col = "red", fullrange = T) +
#   #labs(subtitle= paste("Adj R2 = ",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$adj.r.squared, 3),
#   #"Intercept =",signif(fit$coef[[1]],5 ),
#   #" Slope =",signif(fit$coef[[2]], 5),
#   #"  &  P =",signif(summary(lm(CSp_CSm_df$AMY_BLVP_LEFT_betas~CSp_CSm_df$eff))$coef[2,4], 3)))+
#   scale_x_continuous(name="Mobilized effort", limits=c(-2.02, 2.02)) +
#   scale_y_continuous(name="Beta CSp > CSm", limits=c(-0.6, 0.6)) +
#   theme(plot.subtitle = element_text(size = 10,  vjust = -90, hjust =1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),  margin = NULL, aspect.ratio=1)


# AMY_axia = image_read_pdf('~/REWOD/DERIVATIVES/BEHAV/FIGURES/AMY_figure_axia.pdf')
# #AMY_axia = image_draw(AMY_axia, mar =c(100, 0, 0, 100))
# AMY_coro = image_read_pdf('~/REWOD/DERIVATIVES/BEHAV/FIGURES/AMY_figure_coro.pdf')
# 
# A5 <-  rasterGrob(AMY_axia, interpolate=TRUE)
# A6 <-  rasterGrob(AMY_coro, interpolate=TRUE)


# arranging ---------------------------------------------------------------


figure1 <- ggarrange(A1, A2,
                     #labels = c("B: Coeficients of determination"),
                     ncol = 2, nrow = 1) 

#figure2 <- ggarrange(A3, A4,
                     #labels = c("B: Coeficients of determination"),
                     #ncol = 2, nrow = 1) 

#figure3 <- ggarrange(A5,A6, figure1, figure2,
                     #labels = c(" A"," B"," C", " D"),
                     #vjust=1.5, hjust = 0,
                     #ncol = 2, nrow = 2) 


#figure4 <- annotate_figure(figure3,
#top = text_grob("Big Title", color = "black", face = "bold", size = 18, vjust=1, hjust=0.5),
#bottom = text_grob("Figure 1.", color = "black", face = "bold", vjust=0, hjust = 4))


pdf('~/REWOD/DERIVATIVES/BEHAV/Prevost.pdf')
plot(figure1)
dev.off()


