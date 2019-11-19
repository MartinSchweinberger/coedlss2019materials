###############################################################
# Title:     Advanced Statz for Linguistics - CoEDL Summer School 2019 Workshop
# Part:      1 (Basics)
# Author:    Martin Schweinberger
# Date:      20191112
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
# load libraries
library(ggplot2)
library(car)
library(QuantPsyc)
library(boot)
library(ggplot2)
# load functions
source("https://slcladal.github.io/rscripts/multiplot_ggplot2.r")
source("https://slcladal.github.io/rscripts/slr.summary.tb.r")
source("https://slcladal.github.io/rscripts/multiplot_ggplot2.r")
source("https://slcladal.github.io/rscripts/mlinr.summary.r")
source("https://slcladal.github.io/rscripts/SampleSizeMLR.r")
source("https://slcladal.github.io/rscripts/ExpR.r")
###############################################################
#              INTRODUCTION
# Scatterplot with lines from the regression line to the dots
x <- c(173, 169, 176, 166, 161, 164, 160, 158, 180, 187)
y <- c(80, 68, 72, 75, 70, 65, 62, 60, 85, 92) # plot scatterplot and the regression line
mod1 <- lm(y ~ x)
mod2 <- lm(y ~ 1)
par(mfrow=c(1, 4)) # plot window: 1plot/row and 4 plots/column
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(h=mean(y), col="blue")
res_mean <- signif(residuals(mod2), 5)
pre_mean <- predict(mod2) # plot distances between points and the regression line
segments(x, y, x, pre_mean, col="red")
text(x = 170, y = 100, 
     labels = paste("Res.Dev.:\n", round(sqrt(sum(residuals(mod2)^2)), 1)))

plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2, col = "blue")
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2, col = "blue")

# calculate residuals and predicted values
res <- signif(residuals(mod1), 5)
pre <- predict(mod1) # plot distances between points and the regression line
segments(x, y, x, pre, col="red")
# add labels (res values) to points
library(calibrate)
#textxy(x, y, round(res,1), cex=1)
text(x = 170, y = 100, 
     labels = paste("Res.Dev.:\n", round(sqrt(sum(residuals(mod1)^2)), 1)))
par(mfrow=c(1, 1)) # restore original graphics parameters

###############################################################
#              SIMPLE LINEAR REGRESSION
# load data
slrdata <- read.delim("https://slcladal.github.io/data/slrdata.txt", header = TRUE)
# attach data
attach(slrdata)
slrdata <- as.data.frame(cbind(datems, pptw))  # remove superfluous columns
colnames(slrdata) <- c("year", "prep.ptw")     # add column names
slrdata <- slrdata[!is.na(slrdata$year) == T,] # remove NAs from data
# inspect structure of the data
str(slrdata)                                   

# visualize data
ggplot(slrdata, aes(year, prep.ptw)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth()              # with loess smoothing!
ggplot(slrdata, aes(year, prep.ptw)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm") # with linear model abline!

# scale date
slrdata$prep.ptw <- slrdata$prep.ptw - mean(slrdata$prep.ptw) 

# create initial model
prep.lm <- lm(prep.ptw ~ year, data = slrdata)
# inspect results
summary(prep.lm)

# plot model: 3 plots per row in one window
par(mfrow = c(1, 3))
plot(resid(prep.lm))
plot(rstandard(prep.lm))
plot(rstudent(prep.lm))
par(mfrow = c(1, 1)) # restore default parameters

par(mfrow = c(2, 2)) # plot window: 2 plots/row, 2 plots/column
plot(prep.lm)        # generate diagnostic plots
par(mfrow = c(1, 1)) # restore normal plot window

# tabulate results
slr.summary(prep.lm)  

###############################################################
#               MULTIPLE LINEAR REGRESSION
# load data
mlrdata <- read.delim("https://slcladal.github.io/data/mlrdata.txt", header = TRUE)
head(mlrdata)    # inspect first 6 lines
str(mlrdata)     # inspect structure
summary(mlrdata) # summarize data

# create plot
# plot 1
p1 <- ggplot(mlrdata, aes(status, money)) +      # def. data/x/y-axes
  geom_boxplot(fill=c("gold", "indianred4")) + # def. col.
  theme_set(theme_bw(base_size = 8))+   # black and white theme
  labs(x = "") +                        # x-axis label
  labs(y = "Money spent on present (AUD)", cex = .75) +   # y-axis label
  coord_cartesian(ylim = c(0, 250)) +   # y-axis range
  guides(fill = FALSE) +                # no legend
  ggtitle("Status")                     # title
# plot 2
p2 <- ggplot(mlrdata, aes(attraction, money)) +
  geom_boxplot(fill=c("grey30", "grey70")) +
  theme_set(theme_bw(base_size = 8))+
  labs(x = "") +                              # x-axis label
  labs(y = "Money spent on present (AUD)") +  # y-axis label
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE) +
  ggtitle("Attraction")
# plot 3
p3 <- ggplot(mlrdata, aes(x = money)) +
  geom_histogram(aes(y=..density..),    # add density statistic
                 binwidth = 10,         # def. bin width
                 colour = "black",      # def. bar edge colour
                 fill = "white") +      # def. bar col.
  theme_bw() +                        # black-white theme
  geom_density(alpha=.2, fill = "#FF6666") # def. col. of overlay
# plot 4
p4 <- ggplot(mlrdata, aes(status, money)) +
  geom_boxplot(notch = F, aes(fill = factor(status))) + # create boxplot
  scale_fill_brewer(palette="Set1") +   # def. col. palette
  facet_wrap(~ attraction, nrow = 1) +  # separate panels for attraction
  theme_set(theme_bw(base_size = 8)) +
  labs(x = "") +
  labs(y = "Money spent on present (Euro)") +
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE)
# show plots
multiplot(p1, p3, p2, p4, cols = 2)

# create initial models
m0.mlr = lm(         # generate lm regression object
  money ~ 1,         # def. regression formula (1 = intercept)
  data = mlrdata)    # def. data
m0.glm = glm(        # generate glm regression object
  money ~ 1,         # def. regression formula (1 = intercept)
  family = gaussian, # def. linkage function
  data = mlrdata)    # def. data
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian, data = mlrdata)

# automated AIC based model fitting
step(m1.mlr, direction = "both")

# create final models
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ (status + attraction)^2, family = gaussian, data = mlrdata) 
# inspect final minimal model
summary(m2.mlr)

#intercept  Single  NotInterested  Single:NotInterested
#99.15     + 57.69  + 0           + 0     # 156.8 single + interested
#99.15     + 57.69  - 47.66       - 63.18 # 46.00 single + not interested
#99.15     - 0      + 0           - 0     # 99.15 relationship + interested
#99.15     - 0      - 47.66       - 0     # 51.49 relationship + not interested

# make prediction based on the model for original data
prediction <- predict(m2.mlr, newdata = mlrdata)
# inspect predictions
table(round(prediction,2))

# extract confidence intervals of the coefficients
confint(m2.mlr)
# compare baseline- and minimal adequate model
anova(m0.mlr, m2.mlr)

# compare baseline- and minimal adequate model
Anova(m0.mlr, m2.mlr, type = "III")

# start plotting
par(mfrow = c(1, 4)) # display plots in 3 rows/2 columns
plot(m2.mlr)         # plot fitted values
par(mfrow = c(1, 1)) # restore original settings

# determine a cutoff for data points that have D-values higher than 4/(n-k-1)
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
# start plotting
par(mfrow = c(1, 2))           # display plots in 3 rows/2 columns
qqPlot(m2.mlr, main="QQ Plot") # create qq-plot
plot(m2.mlr, which=4, cook.levels = cutoff) # plot cook*s distance
par(mfrow = c(1, 1))           # restore original settings

# extract influence statistics
infl <- influence.measures(m2.mlr)
# add infl. statistics to data
mlrdata <- data.frame(mlrdata, infl[[1]], infl[[2]])
# annotate too influential data points
remove <- apply(infl$is.inf, 1, function(x) {
  ifelse(x == TRUE, return("remove"), return("keep")) } )
# add annotation to data
mlrdata <- data.frame(mlrdata, remove)
# number of rows before removing outliers
nrow(mlrdata)
# remove outliers
mlrdata <- mlrdata[mlrdata$remove == "keep", ]
# number of rows after removing outliers
nrow(mlrdata)

# recreate regression models on new data
m0.mlr = lm(money ~ 1, data = mlrdata)
m0.glm = glm(money ~ 1, family = gaussian, data = mlrdata)
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian, data = mlrdata)
# automated AIC based model fitting
step(m1.mlr, direction = "both")

# create new final models
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ status * attraction, family = gaussian,
             data = mlrdata)
# inspect final minimal model
summary(m2.mlr)

# extract confidence intervals of the coefficients
confint(m2.mlr)

# compare baseline with final model
anova(m0.mlr, m2.mlr)

# compare baseline with final model
Anova(m0.mlr, m2.mlr, type = "III")

# start plotting
par(mfrow = c(2, 2)) # display plots in 2 rows/2 columns
plot(m2.mlr)         # plot fitted values
par(mfrow = c(1, 1)) # restore original settings

# determine a cutoff for data points that have
# D-values higher than 4/(n-k-1)
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
# start plotting
par(mfrow = c(1, 2))           # display plots in 1 row/2 columns
qqPlot(m2.mlr, main="QQ Plot") # create qq-plot
plot(m2.mlr, which=4, cook.levels = cutoff) # plot cook*s distance
par(mfrow = c(1, 1))           # restore original settings

# add model diagnostics to the data
mlrdata$residuals <- resid(m2.mlr)
mlrdata$standardized.residuals <- rstandard(m2.mlr)
mlrdata$studentized.residuals <- rstudent(m2.mlr)
mlrdata$cooks.distance <- cooks.distance(m2.mlr)
mlrdata$dffit <- dffits(m2.mlr)
mlrdata$leverage <- hatvalues(m2.mlr)
mlrdata$covariance.ratios <- covratio(m2.mlr)
mlrdata$fitted <- m2.mlr$fitted.values

# plot 5
p5 <- ggplot(mlrdata,
             aes(studentized.residuals)) +
  theme(legend.position = "none") +
  theme_set(theme_bw(base_size = 8))+
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour="black",
                 fill="white") +
  labs(x = "Studentized Residual", y = "Density") +
  stat_function(fun = dnorm,
                args = list(mean = mean(mlrdata$studentized.residuals, na.rm = TRUE),
                            sd = sd(mlrdata$studentized.residuals, na.rm = TRUE)),
                colour = "red", size = 1)
# plot 6
p6 <- ggplot(mlrdata, aes(fitted, studentized.residuals)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Red")+
  theme_set(theme_bw(base_size = 8))+
  labs(x = "Fitted Values",
       y = "Studentized Residual")
# plot 7
p7 <- qplot(sample = mlrdata$studentized.residuals, stat="qq") +
  theme_set(theme_bw(base_size = 8))+
  labs(x = "Theoretical Values",
       y = "Observed Values")
multiplot(p5, p6, p7, cols = 3)

# 1: optimal = 0
# (aufgelistete datenpunkte sollten entfernt werden)
which(mlrdata$standardized.residuals > 3.29)

# 2: optimal = 1
# (listed data points should be removed)
stdres_258 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
  ifelse(sqrt((x^2)) > 2.58, 1, 0) } ))
(sum(stdres_258) / length(stdres_258)) * 100

# 3: optimal = 5
# (listed data points should be removed)
stdres_196 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
  ifelse(sqrt((x^2)) > 1.96, 1, 0) } ))
(sum(stdres_196) / length(stdres_196)) * 100

# 4: optimal = 0
# (listed data points should be removed)
which(mlrdata$cooks.distance > 1)

# 5: optimal = 0
# (data points should be removed if cooks distance is close to 1)
which(mlrdata$leverage >= (3*mean(mlrdata$leverage)))

# 6: checking autocorrelation:
# Durbin-Watson test (optimal: grosser p-wert)
dwt(m2.mlr)

# 7: test multicolliniarity 1
vif(m2.mlr)

# 8: test multicolliniarity 2
1/vif(m2.mlr)

# 9: mean vif should not exceed 1
mean(vif(m2.mlr))

# check if sample size is sufficient
smplesz(m2.mlr)
# check beta-error likelihood
expR(m2.mlr)

# tabulate regression results
mlrsummary <- mlr.summary(m2.mlr, m2.glm, ia = T)
# remove columns with confidence intervals
mlrsummary[,-c(4:5)]

###############################################################
# Multiple Binomial Logistic Regression
# clean workspace
rm(list=ls(all=T))
# set options
options("scipen" = 100, "digits" = 4)
# load libraries
library(effects)
library(ggplot2)
library(mlogit)
library(plyr)
library(rms)
library(sjPlot)
library(visreg)
# load functions
source("https://slcladal.github.io/rscripts/multiplot_ggplot2.R")
source("https://slcladal.github.io/rscripts/blr.summary.R")
# load data
blrdata <- read.table("https://slcladal.github.io/data/blrdata.txt",
                      comment.char = "",  # data does not contain comments
                      quote = "",         # data does not contain quotes
                      sep = "\t",         # data is tab separetd
                      header = T)         # variables have headers
# inspect data
str(blrdata)

vrs <- c("Age", "Gender", "Ethnicity", "ID")  # define variables to be factorized
fctr <- which(colnames(blrdata) %in% vrs)     # define vector with variables
blrdata[,fctr] <- lapply(blrdata[,fctr], factor) # factorize variables
blrdata$Age <- relevel(blrdata$Age, "Young") # relevel Age (Young = Reference)
blrdata$Ethnicity <- relevel(                # relevel Ethnicity
  blrdata$Ethnicity, "Pakeha") # define Pakeha as Reference level)
p1 <- ggplot(blrdata,
             aes(Gender, EH, color = Gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Gender", y = "Probability of EH")+
  guides(fill=FALSE, color=FALSE) +
  scale_color_manual(values = c("blue", "red"))
p2 <- ggplot(blrdata,
             aes(Age, EH, color = Age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Probability of EH") +
  guides(fill=FALSE, color=FALSE) +
  scale_color_manual(values = c("darkblue", "lightblue"))
p3 <- ggplot(blrdata,
             aes(Ethnicity, EH, colour = Ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Probability of EH", colour = "Ethnicity") +
  guides(fill=FALSE, color=FALSE) +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
p4 <- ggplot(blrdata,
             aes(Ethnicity, EH, colour = Gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Probability of EH", colour = "Gender")+
  scale_color_manual(values = c("blue", "red"))
p5 <- ggplot(blrdata,
             aes(Gender, EH, colour = Age)) +
  stat_summary(fun.y = mean, geom = "point",
               aes(group= Age)) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Sex", y = "Probability of EH", colour = "Age") +
  guides(fill = FALSE) +
  scale_color_manual(values = c("darkblue", "lightblue"))
p6 <- ggplot(blrdata,
             aes(Age, EH, colour = Ethnicity)) +
  stat_summary(fun.y = mean, geom = "point",
               aes(group= Ethnicity)) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Probability of EH", colour = "Ethnicity") +
  guides(fill = FALSE) +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
# display the plots
multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

# set contrasts
options(contrasts  =c("contr.treatment", "contr.poly"))
# create distance matrix
blrdata.dist <- datadist(blrdata)
# include distance matrix in options
options(datadist = "blrdata.dist")

# baseline glm model
m0.glm = glm(EH ~ 1, family = binomial, data = blrdata)
# baseline lrm model
m0.lrm = lrm(EH ~ 1, data = blrdata, x = T, y = T)

# start manula model fitting
# Age
# check incomplete information
ifelse(min(ftable(blrdata$Age, blrdata$EH)) == 0, "not possible", "possible")
m1.glm = glm(EH ~ Age, family = binomial, data = blrdata)
ifelse(max(vif(m1.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m1.glm, m0.glm, test = "Chi")
Anova(m1.glm, test = "LR")

# Gender
ifelse(min(ftable(blrdata$Gender, blrdata$EH)) == 0, "not possible", "possible")
m2.glm <- update(m1.glm, . ~ . +Gender)
ifelse(max(vif(m2.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m2.glm, m1.glm, test = "Chi")
Anova(m2.glm, test = "LR")

# Ethnicity
ifelse(min(ftable(blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m3.glm <- update(m2.glm, . ~ . +Ethnicity)
ifelse(max(vif(m3.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m3.glm, m2.glm, test = "Chi")

# Age*Gender
ifelse(min(ftable(blrdata$Age, blrdata$Gender, blrdata$EH)) == 0, "not possible", "possible")
m4.glm <- update(m2.glm, . ~ . +Age*Gender)
ifelse(max(vif(m4.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m4.glm, m2.glm, test = "Chi")

# Age*Ethnicity
ifelse(min(ftable(blrdata$Age, blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m5.glm <- update(m2.glm, . ~ . +Age*Ethnicity)
ifelse(max(vif(m5.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m5.glm, m2.glm, test = "Chi")

# Gender*Ethnicity
ifelse(min(ftable(blrdata$Gender, blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m6.glm <- update(m2.glm, . ~ . +Gender*Ethnicity)
ifelse(max(vif(m6.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m6.glm, m2.glm, test = "Chi")

# Age*Gender*Ethnicity
ifelse(min(ftable(blrdata$Age, blrdata$Gender, blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m7.glm <- update(m2.glm, . ~ . +Gender*Ethnicity)
ifelse(max(vif(m7.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m7.glm, m2.glm, test = "Chi")

m2.lrm <- lrm(EH ~ Age+Gender, data = blrdata, x = T, y = T, linear.predictors = T)
m2.lrm

anova(m2.lrm)

pentrace(m2.lrm, seq(0, 0.8, by = 0.05)) # determine penalty

lr.glm <- m2.glm  # rename final minimal adeqaute glm model
lr.lrm <- m2.lrm  # rename final minimal adeqaute lrm model

modelChi <- lr.glm$null.deviance - lr.glm$deviance
chidf <- lr.glm$df.null - lr.glm$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(m0.glm, lr.glm, test = "Chi") # Model Likelihood Ratio Test

# calculate pseudo R^2
# number of cases
ncases <- length(fitted(lr.glm))
R2.hl <- modelChi/lr.glm$null.deviance
R.cs <- 1 - exp ((lr.glm$deviance - lr.glm$null.deviance)/ncases)
R.n <- R.cs /( 1- ( exp (-(lr.glm$null.deviance/ ncases))))
# function for extracting pseudo-R^2
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n") }
logisticPseudoR2s(lr.glm)

# extract the confidence intervals for the coefficients
confint(lr.glm)

exp(lr.glm$coefficients) # odds ratios
exp(confint(lr.glm))     # confidence intervals of the coefficients

blrdata_byspeaker <- table(blrdata$ID, blrdata$EH)
blrdata_byspeaker <- data.frame(rownames(blrdata_byspeaker), blrdata_byspeaker[, 1], blrdata_byspeaker[, 2])
names(blrdata_byspeaker) <- c("ID", "NOEH", "EH")
rownames(blrdata_byspeaker) <- 1:length(blrdata_byspeaker[,1])

blrdata_byspeaker <- join(blrdata_byspeaker,  # join by-speaker data and biodata
                          blrdata, by = "ID", # join by ID
                          type = "left",      # only speakers for which bio data is provided
                          match = "first")    #
blrdata_byspeaker$EH <- NULL                  # remove EH column

# use by.spk data to fit another model which we will use to test the accuracy of the model
lr.glm.spk <- glm(cbind(EH, NOEH) ~ Age*Gender + Ethnicity + Age:Ethnicity, data = blrdata_byspeaker, family = binomial)
correct <- sum(blrdata_byspeaker$EH * (predict(lr.glm.spk, type = "response") >= 0.5)) + sum(blrdata_byspeaker$NOEH * (predict(lr.glm.spk, type="response") < 0.5))
tot <- sum(blrdata_byspeaker$EH) + sum(blrdata_byspeaker$NOEH)
predict.acc <- (correct/tot)*100
predict.acc

# extract prediction accuracy
lr.glm.spk.base <- glm(cbind(EH, NOEH) ~ 1, data = blrdata_byspeaker, family = binomial)
correct.b <- sum(blrdata_byspeaker$EH * (predict(lr.glm.spk.base, type = "response") >= 0.5)) + sum(blrdata_byspeaker$NOEH * (predict(lr.glm.spk.base, type="response") < 0.5))
tot.b <- sum(blrdata_byspeaker$EH) + sum(blrdata_byspeaker$NOEH)
predict.acc.base <- (correct.b/tot.b)*100
# inspect prediction accuracy
predict.acc.base

# compare preictions of final and base line model
which(lr.glm.spk$fitted > .5)
which(lr.glm.spk.base$fitted > .5)

# create plot
par(mfrow = c(1, 2))
visreg(lr.glm, "Age", xlab = "Age",
       ylab = "Logged Odds (EH)",
       ylim = c(-3, 0))
visreg(lr.glm, "Gender", xlab = "Gender",
       ylab = "Logged Odds (EH)",
       ylim = c(-3, 0))
par(mfrow = c(1, 1))

# check multicollinearity
vif(lr.glm)

mean(vif(lr.glm))

infl <- influence.measures(lr.glm) # calculate influence statistics
blrdata <- data.frame(blrdata, infl[[1]], infl[[2]]) # add influence statistics

# function to evaluate sample size
smplesz <- function(x) {
  ifelse((length(x$fitted) < (104 + ncol(summary(x)$coefficients)-1)) == TRUE,
         return(
           paste("Sample too small: please increase your sample by ",
                 104 + ncol(summary(x)$coefficients)-1 - length(x$fitted),
                 " data points", collapse = "")),
         return("Sample size sufficient")) }
# apply unction to model
smplesz(lr.glm)

# summarize regression analysis
blrm.summary(lr.glm, lr.lrm, predict.acc)

###############################################################
#                         END PART 1
###############################################################


