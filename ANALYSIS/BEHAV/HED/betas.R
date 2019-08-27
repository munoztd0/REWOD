## R code for FOR REWOD_HED
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc )

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

# Set working directory
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



# plot perceived_likings by time with regression lign
ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
# Plot R_C liking

lik = R_C_df$lik
ggplotRegression(lm(R_C_df$AMY_Left~lik))
ggplotRegression(lm(R_C_df$AMY_Right~lik))
ggplotRegression(lm(R_C_df$Piri_Left~lik))
ggplotRegression(lm(R_C_df$Piri_Right~lik))
ggplotRegression(lm(R_C_df$subcal_Left~lik))
ggplotRegression(lm(R_C_df$subcal_Right~lik))


# Plot R_C lintensity

int = R_C_df$int
ggplotRegression(lm(R_C_df$AMY_Left~int))
ggplotRegression(lm(R_C_df$AMY_Right~int))
ggplotRegression(lm(R_C_df$Piri_Left~int))
ggplotRegression(lm(R_C_df$Piri_Right~int))
ggplotRegression(lm(R_C_df$subcal_Left~int))
ggplotRegression(lm(R_C_df$subcal_Right~int))

# Plot # Plot R_N liking

lik = R_N_df$lik
ggplotRegression(lm(R_N_df$AMY_Left~lik))
ggplotRegression(lm(R_N_df$AMY_Right~lik))
ggplotRegression(lm(R_N_df$Piri_Left~lik))
ggplotRegression(lm(R_N_df$Piri_Right~lik))
ggplotRegression(lm(R_N_df$subcal_Left~lik))
ggplotRegression(lm(R_N_df$subcal_Right~lik))


# Plot R_N intensity

int = R_N_df$int
ggplotRegression(lm(R_N_df$AMY_Left~int))
ggplotRegression(lm(R_N_df$AMY_Right~int))
ggplotRegression(lm(R_N_df$Piri_Left~int))
ggplotRegression(lm(R_N_df$Piri_Right~int))
ggplotRegression(lm(R_N_df$subcal_Left~int))
ggplotRegression(lm(R_N_df$subcal_Right~int))


##


plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)


ggplotRegression(lm(perceived_liking ~ trialxcondition, data = bt))

lik_func <- function(index) {
  ggplotRegression(lm(R_N_df[,index] ~ R_N_df$lik))
}


lapply(2:6,FUN=lik_func)

func=function(x){lik_func(x)}
lapply(vars1beta, func)

mapply(lik_func(x,y), y = vars1mod,
       MoreArgs = list( vars1beta))


# Corr
corr_R_N.rcorr = rcorr(as.matrix(R_N_df))
corr_R_N.coeff = corr_R_N.rcorr$r[2:5,6:7]
corr_R_N.p = corr_R_N.rcorr$P[2:5,6:7]


corr_R_C.rcorr = rcorr(as.matrix(R_C_df))
corr_R_C.coeff = corr_R_C.rcorr$r[2:7,8:9]
corr_R_C.p = corr_R_C.rcorr$P[2:7,8:9]

