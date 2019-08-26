## R code for FOR REWOD_HED
# last modified on August 2019 by David


# -----------------------  PRELIMINARY STUFF ----------------------------------------
# load libraries
pacman::p_load(ggplot2, dplyr, plyr, tidyr, reshape, reshape2)

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
BETAS_R_N <- read.delim(file.path(analysis_path, 'ROI', paste('extracted_betas_',con_name1,'.txt',sep="")), header = T, sep ='\t') # read in dataset

LIK_R_C <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
LIK_R_N <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod1, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset

INT_R_C <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con1,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t') # read in dataset
INT_R_N <- read.delim(file.path(analysis_path, 'GLM-04', 'group_covariates', paste(con2,'_', mod2, '_meancent.txt',sep="")), header = T, sep ='\t')  # read in dataset


# define factors
factorFunc <- function(var) {
var             <- factor(var)
}

varID<- c("BETAS_R_C$ID", "BETAS_R_N$ID", "LIK_R_C$subj" , "LIK_R_N$subj", "INT_R_C$subj", "INT_R_N$subj")
func=function(x){factorFunc(x)}
lapply(varID, func)


formulas <- paste(varnames, "group", sep = " ~ ")
res <- lapply(formulas, function(f) t.test(as.formula(f), data = d))
names(res) <- varnames

# PLOTS

box_plot <- function(ROI, ID) {
  name <- deparse(substitute(ROI))
  output <- boxplot(ROI ~ ID, las = 1, ylab=name)
  return(output)
}

# betas boxplot by ROI

