## R code for FOR REWOD_PAV
# last modified on Nov 2018 by David


#-----------------------  PRELIMINARY STUFF ----------------------------------------
#load libraries
pacman::p_load(MBESS, afex, car, ggplot2, dplyr, plyr, tidyr, reshape, reshape2, Hmisc, Rmisc, corrplot, ggpubr, gridExtra)

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}


task = 'PAVCOND'

#SETUP

# Set working directory
analysis_path <- file.path('~/REWOD/DERIVATIVES/BEHAV', task) 
setwd(analysis_path)

# open dataset
REWOD_PAV <- read.delim(file.path(analysis_path,'REWOD_PAVCOND_ses_first.txt'), header = T, sep ='') # read in dataset

# define factors
REWOD_PAV$id               <- factor(REWOD_PAV$id)
REWOD_PAV$trial            <- factor(REWOD_PAV$trial)
REWOD_PAV$session          <- factor(REWOD_PAV$session)
REWOD_PAV$condition        <- factor(REWOD_PAV$condition)

# get times in milliseconds 
REWOD_PAV$RT       <- REWOD_PAV$RT * 1000

# remove sub 8 (bc we dont have scans)
REWOD_PAV <- subset (REWOD_PAV,!id == '8') 

#Cleaning
##only first round
REWOD_PAV.clean <- filter(REWOD_PAV, rounds == 1)
REWOD_PAV.clean$condition <- droplevels(REWOD_PAV.clean$condition, exclude = "Baseline")
full = length(REWOD_PAV.clean$RT)

##shorter than 100ms and longer than 3sd+mean
REWOD_PAV.clean <- filter(REWOD_PAV.clean, RT >= 100) # min RT is 106ms
mean <- mean(REWOD_PAV.clean$RT)
sd <- sd(REWOD_PAV.clean$RT)
REWOD_PAV.clean <- filter(REWOD_PAV.clean, RT <= mean +3*sd) #which is 854.4ms
#now accuracy is to a 100%
clean= length(REWOD_PAV.clean$RT)

dropped = full-clean
(dropped*100)/full
#PLOTS 


# create one with baslein for liking
baseline = filter(REWOD_PAV, condition == 'Baseline')
ratings = rbind(REWOD_PAV.clean, baseline)

##plot (non-averaged per participant) 

# reaction time by conditions #(baseline non included)
boxplot(REWOD_PAV.clean$RT ~ REWOD_PAV.clean$condition, las = 1)

# get RT and Liking means by condition (with baseline)
bc = ddply(REWOD_PAV, .(condition), summarise,  RT = mean(RT, na.rm = TRUE)) 

# get acc means by condition (without baseline)
ba = ddply(REWOD_PAV.clean, .(condition), summarise,  accuracy = mean(accuracy, na.rm = TRUE))

# get RT and Liking means by participant (with baseline)
bSRT = ddply(REWOD_PAV.clean, .(id, condition), summarise, RT = mean(RT, na.rm = TRUE))
bslik = ddply(REWOD_PAV, .(id, condition),liking_ratings = mean(liking_ratings, na.rm = TRUE))

# get acc means by participant (without baseline)
bsacc = ddply(REWOD_PAV.clean, .(id), summarise, accuracy = mean(accuracy, na.rm = TRUE))

## plot overall effect RT##


#RT average per subjects by condition (baseline non included)
bsrt <- filter(bsRT, condition != "Baseline")
bsrt$condition <- droplevels(bsrt$condition, exclude = "Baseline")
boxplot(bsrt$RT ~ bsrt$condition, las = 1)


## plot overall effect Ratings

# condition X ratings
boxplot(bslik$liking_ratings ~ bs$condition, las = 1)




#For the key-pressing task, we analyzed RTs on the first target of the on-task period. All responses that were more than 3 SDs from the mean (2% of the 
#trials) or absent (5.5% of the trials) were removed. A paired t test showed that participants were faster when the CS

# get RT  means by participant 
df1 = ddply(REWOD_PAV.clean, .(condition,id), summarise, RT = mean(RT, na.rm = TRUE))

RT_CSplus  <- REWOD_PAV.clean$RT[REWOD_PAV.clean$condition == 'CSplus']
RT_CSminus <- REWOD_PAV.clean$RT[REWOD_PAV.clean$condition == 'CSminus']

t.test(RT_CSplus, RT_CSminus)

#----EFFECT SIZE see yoann's script
cohen_d_ci(RT_CSplus,  RT_CSminus)

##

x = CI(RT_CSminus, ci=0.95)
y = CI(RT_CSplus, ci=0.95)

df1$UCI <- x[1]
df1$UCI[2] <- y[1]

df1$LCI <- x[3]
df1$LCI[2] <- y[3]

# ggplot(df1, aes(x=condition, y=RT, group=1)) + 
#   geom_errorbar(aes(ymin=UCI, ymax=LCI), width=.1)+
#   geom_line() +
#   geom_point()





# RATINGS LIKING ----------------------------------------------------------


ratings$cvalue <- rep(0, (length(ratings$trial)))
ratings$cvalue[ratings$condition== 'CSplus']        <- 2
ratings$cvalue[ratings$condition== 'CSminus']       <- -1
ratings$cvalue[ratings$condition== 'Baseline']      <- -1
ratings$cvalue       <- factor(ratings$cvalue)


#using afex
cond.aov <- aov_car(liking_ratings ~ cvalue + Error(id/cvalue), data = ratings, anova_table = list(es = "pes"))
cond.aov
cond.aov_sum <- summary(cond.aov)
cond.aov_sum

#----EFFECT SIZE

# Confidence interval (see http://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html)

cond.CSCategory_lims      <- conf.limits.ncf(F.value = cond.aov_sum$univariate.tests[2,5], conf.level = .90, df.1 <- cond.aov_sum$univariate.tests[2,2], df.2 <- cond.aov_sum$univariate.tests[2,4])
cond.CSCategory_lower.lim <- cond.CSCategory_lims$Lower.Limit/(cond.CSCategory_lims$Lower.Limit + df.1 + df.2 + 1)
cond.CSCategory_upper.lim <- cond.CSCategory_lims$Upper.Limit/(cond.CSCategory_lims$Upper.Limit + df.1 + df.2 + 1)

cond.effectsizes <- matrix(c(cond.aov$anova_table$pes[1], ifelse(is.na(cond.CSCategory_lower.lim) == F, cond.CSCategory_lower.lim, 0), ifelse(is.na(cond.CSCategory_upper.lim) == F, cond.CSCategory_upper.lim, .00059834237206)), #value computed with SPSS
                          ncol = 3, byrow = T)
colnames(cond.effectsizes) <- c("Partial eta squared", "90% CI lower limit", "90% CI upper limit")
rownames(cond.effectsizes) <- c("CSCategory")
cond.effectsizes

# get liking  means by participant (with baseline)
df2 = ddply(ratings, .(condition), summarise, liking_ratings = mean(liking_ratings, na.rm = TRUE))


##
plus = filter(ratings, condition == 'CSplus')
minus = filter(ratings, condition == 'CSminus')
base = filter(ratings, condition == 'Baseline')
x = CI(plus$liking_ratings, ci=0.95)
y = CI(minus$liking_ratings, ci=0.95)
z = CI(base$liking_ratings, ci=0.95)

df2$UCI <- x[1]
df2$UCI[2] <- y[1]
df2$UCI[3] <- z[1]

df2$LCI <- x[3]
df2$LCI[2] <- y[3]
df2$LCI[3] <- z[3]



add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


Alpha <- add.alpha('black', alpha=0.4)
# Plot the bar chart 

par(mar = c(5, 1.8, 2, 2))


ggplot() + 
  geom_bar(df2, mapping = aes(x = condition, y = liking_ratings), stat = "identity", fill = "white") +
  geom_errorbar(df1, mapping = aes(x = condition, y = RT, ymin=UCI, ymax=LCI), width=.1, color = 'black')+
  geom_line(df1, mapping = aes(x = condition, y = RT, group =1), color = 'black', lty = 2) + 
  theme(plot.margin = margin(2, 2, 2, 2, "cm")) +
  ylim(0, 500) + 
  #ylab("Teeth length") +
  theme_void()
  #name = "Interruptions/day", 
                     #sec.axis = sec_axis(~./5, name = "Productivity % of best", 
                                         #labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
par(new = TRUE)

foo <- barplot(df2$liking_ratings,names.arg=df2$condition,xlab="Pavlovian Stimulus",ylab="Liking Ratings",col=Alpha, space = 1, ylim
= c(0,100), border=NA)

df2 <- df2[order(-df2$liking_ratings),]

arrows(x0=foo,y0=df2$UCI,y1=df2$LCI,angle=90,code=3,length=0.05)


##

par(new = TRUE)
x = c(1:1000)
y = c(1:1000)
plot(x, y, ylim = c(0,500), axis(4, lty=2), col.axis = "black", lwd = 0.5, cex.axis = 0.5)
#axis(4, col = "black", col.axis = "black", lwd = 0.5, cex.axis = 0.5)
#axis(4, col = "darkgreen", lty = 2, lwd = 0.5)
legend("topright", inset=.05, legend=c("Pleasantness Ratings", "Latency"),
       col=c(Alpha, "black"), lty=c(1,2), lwd=c(8,1), cex=0.8, box.lty=0)







