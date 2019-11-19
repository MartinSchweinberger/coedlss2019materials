###############################################################
# Title:     Advanced Statz for Linguistics - CoEDL Summer School 2019 Workshop
# Part:      4 (Combining tree-based procedures and regressions)
# Author:    Martin Schweinberger
# Date:      20191117
# R version: 3.5.1 (2018-07-02) -- "Feather Spray"
# Contact:    m.schweinberger@uq.edu.au
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2019. "Advanced Statz for Linguistics.
#             CoEDL Summer School 2019 Workshop", unpublished R script.
#             Brisbane: The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))  
# set wd
setwd("D:\\Uni\\UQ\\CoEDL\\SummerSchool2019\\AdvancedStatzForLinguists") 
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# define image directory
imageDirectory<-"images"
# install libraries (remove # to activate)
#install.packages(c("partykit", "dplyr", "grid", "Gmisc", "Rling", "ggplot2", "cowplot", 
#                   "randomForest", "party", "Hmisc", "Boruta", "caret", "RCurl))
# activate libraries
library(partykit)              
library(dplyr)  
library(grid)
library(Gmisc) 
library(Rling) 
library(ggplot2)       
library(cowplot)       
library(randomForest)
library(party)
library(Hmisc)
library(Boruta) 
library(RCurl)
library(rms)
library(lme4)
library(car)
library(utils)
library(effects)
# to install the caret library, it was neccessary to go through the installation 
# process below - once caret is installed once, you do not need to go through 
# these steps again
# install caret library
#source("https://bioconductor.org/biocLite.R"); biocLite(); library(Biobase)
#install.packages("Biobase", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com", 
#                                      "http://cran.rstudio.com/", dependencies=TRUE))
#install.packages("dimRed", dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)
# activate caret library
library(caret) 
###############################################################
#                        BORUTA
# create Boruta data
data <- read.table("https://slcladal.github.io/data/mblrdata.txt", 
                   comment.char = "", quote = "", sep = "\t",  header = T)
# inspect data
head(data); str(data)

# factorize variables (boruta - like rf - require factors instead of character vectors)
fcts <- c("ID", "Gender", "Age", "ConversationType", "Priming", "SUFlike")
data[fcts] <- lapply(data[fcts], factor)
# inspect data
str(data)

# set.seed (to store random numbers and thus make results reporducible)
set.seed(2019120209)
# initial run
boruta1 <- Boruta(SUFlike~.,data=data)
print(boruta1)

getConfirmedFormula(boruta1)

plot(boruta1, cex = .5)

plotImpHistory(boruta1)

par(mar = c(8, 8, 4, 2) + 0.1)
plot(boruta1, cex.axis=.75, las=2, xlab="", ylab = "", cex = .75, 
     col = c(rep("grey50", 5),rep("grey90", 3)))
abline(v = 3.5, lty = "dashed")
mtext("Predictors", 1, line = 7, at = 5, cex = 1)
mtext("Control", 1, line = 6, at = 2, cex = 1)
mtext("Importance", 2, line = 2.5, at = 2.5, cex = 1, las = 0)
par(mar = c(5, 4, 4, 2) + 0.1)

###############################################################
#             DATA VISUALIZATION
data$SUFlike <- as.numeric(as.character(data$SUFlike))
# plot random effect variable
hist(table(data$ID))

# plot dependent variable
mean(data$SUFlike); sd(data$SUFlike)

# create data for plotting: gender
p1d <- data %>%
  dplyr::select(ID, SUFlike, Gender) %>%
  group_by(ID, Gender) %>%
  dplyr::summarize(SUFlike=mean(SUFlike)) %>%
  dplyr::ungroup(Gender) %>%
  dplyr::mutate(Mean=round(mean(SUFlike), 3)) %>%
  dplyr::mutate(SD=round(sd(SUFlike),3))
# start plot
ggplot(p1d, aes(Gender, SUFlike, color = Gender, fill = Gender)) +
  geom_violin(trim=FALSE, color = "gray20")+ 
  geom_boxplot(width=0.1, fill="white", color = "gray20") +
  geom_text(aes(y=-.5,label=paste("mean: ", Mean, sep = "")), size = 3, color = "black") +
  geom_text(aes(y=-.75,label=paste("SD: ", SD, sep = "")), size = 3, color = "black") +
  scale_fill_manual(values=rep("grey90", 2)) + 
  theme_set(theme_bw(base_size = 10)) +
  ylim(-1, 1.5) +
  labs(x = "Gender", y = "Speech-unit final LIKE") +
  theme(legend.position="none")

# create data for plotting: Age
p2d <- data %>%
  dplyr::select(ID, SUFlike, Age) %>%
  group_by(ID, Age) %>%
  dplyr::summarize(SUFlike=mean(SUFlike)) %>%
  dplyr::ungroup(Age) %>%
  dplyr::mutate(Mean=round(mean(SUFlike), 3)) %>%
  dplyr::mutate(SD=round(sd(SUFlike), 3))
# start plot
ggplot(p2d, aes(Age, SUFlike, color = Age, fill = Age)) +
  geom_violin(trim=FALSE, color = "gray20")+ 
  geom_boxplot(width=0.1, fill="white", color = "gray20") +
  geom_text(aes(y=-.25,label=paste("mean: ", Mean, sep = "")), size = 3, color = "black") +
  geom_text(aes(y=-.5,label=paste("SD: ", SD, sep = "")), size = 3, color = "black") +
  scale_fill_manual(values=rep("grey90", 2)) + 
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position="top", legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ylim(-1, 2) +
  labs(x = "Age", y = "Speech-unit final LIKE") +
  theme(legend.position="none")

###############################################################
#                REGRESSION MODELLING
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
data.dist <- datadist(data)
options(datadist = "data.dist")

# baseline model glm
m0.glm = glm(SUFlike ~ 1, family = binomial, data = data) 
# baseline model lrm
m0.lrm = lrm(SUFlike ~ 1, data = data, x = T, y = T) 
# base-line mixed-model
m0.glmer = glmer(SUFlike ~ (1|ID), data = data, family = binomial) 

aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# test random effects
null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F) 
# sig m0.glmer better than m0.glm

###############################################################
#                    MODEL FITTING
# add optimizer (prevents non-conversion and improves model fit process)
m0.glmer <- glmer(SUFlike ~ 1+ (1|ID), family = binomial, data = data, control=glmerControl(optimizer="bobyqa"))

# add Priming
ifelse(min(ftable(data$Priming, data$SUFlike)) == 0, "incomplete information", "okay")
m1.glm <- update(m0.glm, .~.+Priming)
m1.glmer <- update(m0.glmer, .~.+Priming)
anova(m1.glmer, m0.glmer, test = "Chi")      # SIG (p<.0.00000000000000022 ***) 
Anova(m1.glmer, test = "Chi")                # SIG (p<.0.00000000000000022 ***) 

# add Gender
ifelse(min(ftable(data$Gender, data$SUFlike)) == 0, "incomplete information", "okay")
m2.glm <- update(m1.glm, .~.+Gender)
ifelse(max(vif(m2.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m2.glmer <- update(m1.glmer, .~.+Gender)
anova(m2.glmer, m1.glmer, test = "Chi")      # SIG (p<.0.00000000000000022 ***) 
Anova(m2.glmer, test = "Chi")                # SIG (p<.0.00000000000000022 ***)

# add Age
ifelse(min(ftable(data$Age, data$SUFlike)) == 0, "incomplete information", "okay")
m3.glm <- update(m2.glm, .~.+Age)
ifelse(max(vif(m3.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m3.glmer <- update(m2.glmer, .~.+Age)
anova(m3.glmer, m2.glmer, test = "Chi")      # not sig (p=0.7635) 

# add ConversationType
ifelse(min(ftable(data$ConversationType, data$SUFlike)) == 0, "incomplete information", "okay")
m4.glm <- update(m2.glm, .~.+ConversationType)
ifelse(max(vif(m4.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m4.glmer <- update(m2.glmer, .~.+ConversationType)
anova(m4.glmer, m2.glmer, test = "Chi")      # SIG (p=0.0000000003838 ***) 
Anova(m4.glmer, test = "Chi")                # SIG (p=0.0000000000287 ***)

###########################################################################
# find all 2-way interactions
colnames(data)
# define variables included in interactions
vars <- c("Priming", "Gender", "Age", "ConversationType")
intac <- t(combn(vars, 2))
intac

# add Priming*Gender
ifelse(min(ftable(data$Priming, data$Gender, data$SUFlike)) == 0, "incomplete information", "okay")
m5.glm <- update(m4.glm, .~.+Priming*Gender)
ifelse(max(vif(m5.glm)) <= 10,  "VIFs okay", "high VIFs") # VIFs ok
m5.glmer <- update(m4.glmer, .~.+Priming*Gender)
anova(m5.glmer, m4.glmer, test = "Chi")      # SIG (p=0.000000000005307 ***) 
Anova(m5.glmer, test = "Chi")                # SIG (p=0.000000000004814 ***)

# add Priming*Age
ifelse(min(ftable(data$Priming, data$Age, data$SUFlike)) == 0, "incomplete information", "okay")
m6.glm <- update(m5.glm, .~.+Priming*Age)
ifelse(max(vif(m6.glm)) <= 10,  "VIFs okay", "high VIFs") # VIFs ok
m6.glmer <- update(m5.glmer, .~.+Priming*Age)
anova(m6.glmer, m5.glmer, test = "Chi")      # not sig (p=0.6988) 

# add Priming*ConversationType
ifelse(min(ftable(data$Priming, data$ConversationType, data$SUFlike)) == 0, "incomplete information", "okay")
m7.glm <- update(m5.glm, .~.+Priming*ConversationType)
ifelse(max(vif(m7.glm)) <= 10,  "VIFs okay", "high VIFs") # VIFs ok
m7.glmer <- update(m5.glmer, .~.+Priming*ConversationType)
anova(m7.glmer, m5.glmer, test = "Chi")      # SIG (p=0.007764 **) 
Anova(m7.glmer, test = "Chi")                # SIG (p=0.007662 **)

# add Gender*Age
ifelse(min(ftable(data$Gender, data$Age, data$SUFlike)) == 0, "incomplete information", "okay")
m8.glm <- update(m7.glm, .~.+Gender*Age)
ifelse(max(vif(m8.glm)) <= 10,  "VIFs okay", "high VIFs") # VIFs ok
m8.glmer <- update(m7.glmer, .~.+Gender*Age)
anova(m8.glmer, m7.glmer, test = "Chi")      # not sig (p=0.7971) 

# add Gender*ConversationType
ifelse(min(ftable(data$Gender, data$ConversationType, data$SUFlike)) == 0, "incomplete information", "okay")
m9.glm <- update(m7.glm, .~.+Gender*ConversationType)
ifelse(max(vif(m9.glm)) <= 10,  "VIFs okay", "high VIFs") # VIFs ok
vif(m9.glm)                                               # not excessive!
m9.glmer <- update(m7.glmer, .~.+Gender*ConversationType)
anova(m9.glmer, m7.glmer, test = "Chi")      # SIG (p=0.0007925 ***) 
Anova(m9.glmer, test = "Chi")                # SIG (p=0.001061 **)

# add Age*ConversationType
ifelse(min(ftable(data$Age, data$ConversationType, data$SUFlike)) == 0, "incomplete information", "okay")
m10.glm <- update(m9.glm, .~.+Age*ConversationType)
ifelse(max(vif(m10.glm)) <= 10,  "VIFs okay", "high VIFs") # VIFs ok
vif(m10.glm)                                               # not excessive!
m10.glmer <- update(m9.glmer, .~.+Age*ConversationType)
anova(m10.glmer, m9.glmer, test = "Chi")      # not sig (p=0.6335)

# final model = m9.glmer
# diagnostic plots
plot(m9.glmer, pch = 20, col = "black", lty = "dotted", ylab = "Pearson's residuals",
     xlab = "Fitted values")

# diagnostic plot: examining residuals (Pinheiro & Bates 2000:175)
plot(m9.glmer, ID ~ resid(.), abline = 0 , cex = .5)

##########################################
#           VISUALIZE EFFECTS
##########################################
# create effect plot for comparison purposes
plot(predictorEffects(m9.glmer)) 

# create data frame with effects
eff_cf <- predictorEffects(m9.glmer)
eff_df <- data.frame(eff_cf)

# create data frame with effects
ef <- as.data.frame(effect("IndicatedErrors:Revision", m5.glmer, xlevels=list(IndicatedErrors=seq(0, 13, 1))))
ef$Type <- rep("Predicted", 28)
ef$lower <- NULL
ef$upper <- NULL
ef <- ef %>%
  dplyr::filter(IndicatedErrors != 11)
# create table
Errors_mean <- t(tapply(data$Errors, list(data$Revision, data$IndicatedErrors), mean))
Errors_se <- t(tapply(data$Errors, list(data$Revision, data$IndicatedErrors), se))
Revision_obs <- c(rep("correct", 13), rep("incorrect", 13))
IndicatedErrors_obs <- c(seq(0,10,1), 12:13, seq(0,10,1), 12:13)  
Errors_obs <- c(Errors_mean[,1], Errors_mean[,2])
Se_obs <- c(Errors_se[,1], Errors_se[,2])
df <- data.frame(Revision_obs, IndicatedErrors_obs, Errors_obs, Se_obs)
colnames(df) <- c("Revision", "IndicatedErrors", "fit", "se")
df$Type <- rep("Observed", 26)
# inspect data
head(df); head(ef)

# combine tables
tbd <- rbind(ef, df)

ggplot(tbd, aes(IndicatedErrors, fit, color = Type)) +
  facet_wrap(~Revision) +
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.5, position="dodge") +
  theme_set(theme_bw(base_size = 20)) +
  scale_color_manual(values = c("grey70", "grey20")) +
  theme(legend.position="top", legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ylim(-2.5, 5) +
  labs(x = "Indicated errors", y = "Number of errors") +
  ggsave(file = paste(imageDirectory,"Effects_quasiPoisson.png",sep="/"), 
         width = 20,  height = 10, units = c("cm"),  dpi = 320)
##########################################


###############################################################
#                      END PART 4
###############################################################
