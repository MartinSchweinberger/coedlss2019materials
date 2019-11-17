###############################################################
# Title:     Advanced Statz for Linguistics - CoEDL Summer School 2019 Workshop
# Part:      3 (Tree-based procedures)
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
#                   "randomForest", "party", "Hmisc", "Boruta", "caret"))
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
#                  LOAD AND INSPECT DATA
citdata <- read.delim("https://slcladal.github.io/data/treedata.txt", header = T, sep = "\t")
# inspect data
head(citdata); str(citdata)

# factorize variables (cit require factors instead of character vectors)
fcts <- c("Age", "Gender", "Status", "LikeUser")
citdata[fcts] <- lapply(citdata[fcts], factor)
# inspect data
str(citdata)

# tabulate data
table(citdata$LikeUser, citdata$Gender)

table(citdata$LikeUser, citdata$Age)

table(citdata$LikeUser, citdata$Status)

# set.seed (to store random numbers and thus make results reporducible)
set.seed(2019120201)        
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*(ncol(citdata)-1)))
# create initial conditional inference tree model
citd.ctree <- ctree(LikeUser ~ Age + Gender + Status,
                    data = citdata)
plot(citd.ctree, gp = gpar(fontsize = 8)) # plot final ctree

###############################################################
#         HOW DO CITs DETERMINE WHEN TO SPLIT?
#              EXPLANATION OF GINI VALUES
#             SPLITS AT LOWEST GINI VALUE!
# 1ST NODE
# GENDER
# re-inspect gender distribution
tblikegender <- table(citdata$LikeUser, citdata$Gender)
tblikegender

chisq.test(tblikegender) # sig difference: data can be split!

# calculate Gini for men
gini_men <- 1-(42/(42+75))^2 - (75/(42+75))^2
# calculate Gini for women
gini_women <- 1-(91/(91+43))^2 - (43/(91+43))^2
# calculate weighted average of Gini for Gender
gini_gender <- 42/(42+75)* gini_men +  91/(91+43) * gini_women
gini_gender

# AGE
# re-inspect age distribution
tblikeage <- table(citdata$LikeUser, citdata$Age)
tblikeage

chisq.test(tblikeage) # sig difference: data can be split!

# calculate Gini for age groups
gini_young <- 1-(92/(92+34))^2 - (34/(92+34))^2  # Gini: young
gini_old <- 1-(41/(41+84))^2 - (84/(41+84))^2    # Gini: old
# calculate weighted average of Gini for Age
gini_age <- 92/(92+34)* gini_young +  41/(41+84) * gini_old
gini_age

# STATUS
# re-inspect status distribution
tblikestatus <- table(citdata$LikeUser, citdata$Status)
tblikestatus

chisq.test(tblikestatus) # sig difference: data can be split!

# calculate Gini for status groups
gini_high <- 1-(73/(33+73))^2 - (33/(33+73))^2   # Gini: high
gini_low <- 1-(60/(60+85))^2 - (85/(60+85))^2    # Gini: low
# calculate weighted average of Gini for Status
gini_status <- 73/(33+73)* gini_high +  60/(60+85) * gini_low
gini_status

# compare age, gender, and status ginis
gini_gender; gini_age; gini_status

# gini_age has lowest value: split by Age!

# plot tree we have so far!
grid.newpage()
# set some parameters to use repeatedly
leftx <- .25
midx <- .5
rightx <- .75
width <- .4
gp <- gpar(fill = "lightgrey")
# create boxes
(rando <- boxGrob("Age", 
                  x=midx, y=.9, box_gp = gp, width = width))
# connect boxes like this
(g1 <- boxGrob("15-40\n NonLikeUsers  LikeUsers \n 34                  92", 
               x=leftx, y=.5, box_gp = gp, width = width))
(g2 <- boxGrob("41-80\n  NonLikeUsers  LikeUsers \n 84                 41", 
               x=rightx, y=.5, box_gp = gp, width = width))
connectGrob(rando, g1, "N")
connectGrob(rando, g2, "N")

# 2ND NODE
# split data according to first split (only old data for now)
old <- citdata[citdata$Age == "41-80",]
# inspect distribution
tboldgender <- table(old$LikeUser, old$Gender)
tboldgender

chisq.test(tboldgender) # sig difference: data can be split!

# calculate Gini for Gender
# calculate Gini for men
gini_oldmen <- 1-(tboldgender[2,2]/sum(tboldgender[,2]))^2 - (tboldgender[1,2]/sum(tboldgender[,2]))^2
# calculate Gini for women
gini_oldwomen <- 1-(tboldgender[2,1]/sum(tboldgender[,1]))^2 - (tboldgender[1,1]/sum(tboldgender[,1]))^2
# # calculate weighted aAverage of Gini for Gender
gini_oldgender <- sum(tboldgender[,2])/sum(tboldgender)* gini_oldmen +  sum(tboldgender[,1])/sum(tboldgender) * gini_oldwomen
gini_oldgender

# calculate Gini for Status
# inspect distribution
tboldstatus <- table(old$LikeUser, old$Status)
tboldstatus

chisq.test(tboldstatus) # sig difference: data can be split!

# calculate Gini for low
gini_oldlow <- 1-(tboldstatus[2,2]/sum(tboldstatus[,2]))^2 - (tboldstatus[1,2]/sum(tboldstatus[,2]))^2
# calculate Gini for high
gini_oldhigh <- 1-(tboldstatus[2,1]/sum(tboldstatus[,1]))^2 - (tboldstatus[1,1]/sum(tboldstatus[,1]))^2
# calculate weighted average of Gini for Status
gini_oldstatus <- sum(tboldstatus[,2])/sum(tboldstatus)* gini_oldlow +  sum(tboldstatus[,1])/sum(tboldstatus) * gini_oldhigh
gini_oldstatus

# compare ginis of gender and status
gini_oldgender; gini_oldstatus

# gini_oldgender has lowest value: split by Gender!

# 3RD NODE
# split data according to first split (only old data for now)
oldmale <- citdata %>%
  dplyr::filter(Age == "41-80") %>%
  dplyr::filter(Gender == "male")
# inspect distribution
tboldmalestatus <- table(oldmale$LikeUser, oldmale$Status)
tboldmalestatus

chisq.test(tboldmalestatus) # no sig difference: no more splits!

# 4TH NODE
# split data according to first split (only old data for now)
oldfemale <- citdata %>%
  dplyr::filter(Age == "41-80") %>%
  dplyr::filter(Gender == "female")
# inspect distribution
tboldfemalestatus <- table(oldfemale$LikeUser, oldfemale$Status)
tboldfemalestatus

chisq.test(tboldfemalestatus) # no sig difference: no more splits!

# 5TH NODE
# split data according to first split (only young data)
young <- citdata[citdata$Age == "15-40",]
# inspect distribution
tbyounggender <- table(young$LikeUser, young$Gender)
tbyounggender

chisq.test(tbyounggender) # no sig difference: do not split!

# calculate Gini for Gender
# calculate Gini for men
gini_youngmen <- 1-(tbyounggender[2,2]/sum(tbyounggender[,2]))^2 - (tbyounggender[1,2]/sum(tbyounggender[,2]))^2
# calculate Gini for women
gini_youngwomen <- 1-(tbyounggender[2,1]/sum(tbyounggender[,1]))^2 - (tbyounggender[1,1]/sum(tbyounggender[,1]))^2
# # calculate weighted aAverage of Gini for Gender
gini_younggender <- sum(tbyounggender[,2])/sum(tbyounggender)* gini_youngmen +  sum(tbyounggender[,1])/sum(tbyounggender) * gini_youngwomen
gini_younggender

# calculate Gini for Status
# inspect distribution
tbyoungstatus <- table(young$LikeUser, young$Status)
tbyoungstatus

chisq.test(tbyoungstatus) # sig difference: split!

# calculate Gini for low
gini_younglow <- 1-(tbyoungstatus[2,2]/sum(tbyoungstatus[,2]))^2 - (tbyoungstatus[1,2]/sum(tbyoungstatus[,2]))^2
# calculate Gini for high
gini_younghigh <- 1-(tbyoungstatus[2,1]/sum(tbyoungstatus[,1]))^2 - (tbyoungstatus[1,1]/sum(tbyoungstatus[,1]))^2
# calculate weighted average of Gini for Status
gini_youngstatus <- sum(tbyoungstatus[,2])/sum(tbyoungstatus)* gini_younglow +  sum(tbyoungstatus[,1])/sum(tbyoungstatus) * gini_younghigh
gini_youngstatus

# compare ginis of gender and status
gini_younggender; gini_youngstatus

# gini_youngstatus has lowest value: split by Status!

# 6TH NODE
# split data according to first and second split (young and low status data)
younglow <- citdata %>%
  filter(Age == "15-40") %>%
  filter(Status == "low")
# inspect gender distribution
tbyounglowgender <- table(younglow$LikeUser, younglow$Gender)
tbyounglowgender

chisq.test(tbyounglowgender) # no sig difference: no more splits!

# 7TH node
# split data according to first and second split (young and high status data)
younghigh <- citdata %>%
  filter(Age == "15-40") %>%
  filter(Status == "high")
# inspect gender distribution
tbyounghighgender <- table(younghigh$LikeUser, younghigh$Gender)
tbyounghighgender

chisq.test(tbyounghighgender) # no sig difference: no more splits!

###############################################################
#                   CIT: R IMPLEMENTATION         
###############################################################
# set.seed (to store random numbers and thus make results reporducible)
set.seed(2019120202) 
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*(ncol(citdata)-1)))
# create initial conditional inference tree model
citd.ctree <- ctree(LikeUser ~ Age + Gender + Status, data = citdata)
plot(citd.ctree, gp = gpar(fontsize = 8)) # plot final ctree

# test prediction accuracy
ptb <- table(predict(citd.ctree), citdata$LikeUser)
(((ptb[1]+ptb[4])+(ptb[2]+ptb[3]))/sum(table(predict(citd.ctree), citdata$LikeUser)))*100
# inspect results
ptb

# determine baseline
(table(citdata$LikeUser)[[2]]/sum(table(citdata$LikeUser)))*100

###############################################################
#              CIT: SPLITS IN NUMERIC VARIABLES        
###############################################################
# load new data
citdata2 <- read.delim("https://slcladal.github.io/data/numerictreedata.txt", header = T, sep = "\t")
citdata2 <- citdata2 %>%
  arrange(Age)
# inspect data
citdata2

# create a table with LikeUser and Age values that are inbetween the Ages provided
Age <- c(15, ((15+22)/2), 22, ((22+27)/2), 27, ((27+37)/2), 37, ((37+42)/2), 42, ((42+63)/2), 63)
LikeUser <- c("yes", "", "yes", "", "yes", "", "no", "", "yes", "", "no")
citdata3 <- data.frame(Age, LikeUser)
# inspect data
citdata3

# 18.5
1-(1/(1+0))^2 - (0/(1+0))^2
1-(2/(2+3))^2 - (3/(2+3))^2
1/6 * 0.0 +  5/6 * 0.48 # Gini for 18.5 = 0.4
# 24.5
1-(2/(2+0))^2 - (0/(2+0))^2
1-(3/(3+1))^2 - (2/(3+1))^2
2/6 * 0.0 +  4/6 * 0.1875 # Gini for 24.5 = 0.125
# 32
1-(3/(3+0))^2 - (0/(3+0))^2
1-(1/(1+2))^2 - (2/(1+2))^2
3/6 * 0.0 +  3/6 * 0.4444444 # Gini for 32.0 = 0.2222222
# 39.5
1-(3/(3+1))^2 - (1/(3+1))^2
1-(1/(1+1))^2 - (1/(1+1))^2
4/6 * 0.375 +  2/6 * 0.5 # Gini for 39.5 = 0.4166667
# 52.5
1-(4/(4+1))^2 - (1/(4+1))^2
1-(0/(0+1))^2 - (1/(0+1))^2
5/6 * 0.32 +  1/6 * 0.0 # Gini for 52.5 = 0.2666667

AgeSplit <- c(((15+22)/2), ((22+27)/2), ((27+37)/2), ((37+42)/2), ((42+63)/2))
Gini <- c(0.4, 0.125,0.222, 0.417, 0.267)
citdata3 <- data.frame(AgeSplit, Gini)
citdata3 # split at 24.5 (lowest Gini value)

###############################################################
#                     RANDOM FOREST
###############################################################
# process data
rfdata <- citdata %>%
  dplyr::mutate(ID = 1:nrow(citdata)) %>%
  dplyr::select(ID, Age, Gender, Status, LikeUser)

citdata <- citdata[c(6, 3, 4, 1, 2, 2),]
rownames(citdata) <- NULL

# define path to data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
# load data 
data <- read.csv(url, header=FALSE)
# inspect data
head(data) 

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar greater than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)
head(data) # now we have data and column names

str(data)

data[data == "?"] <- NA
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca) 

data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
str(data) 

set.seed(42)
## impute any missing values in the training set using proximities
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)

model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)
model 

confusionmatrixtb <- matrix(c("", "Healthy", "Unhealthy", "Healthy", "Number of healthy people correctly called healthy by the forest.", "Number of healthy people incorectly called unhealthy by the forest", "Unhealthy", "Number of unhealthy people incorrectly called healthy by the forest", "Number of unhealthy people correctly called unhealthy by the forest"), ncol = 3, byrow = T)
confusionmatrixtb


oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Healthy"],
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
# ggsave("oob_error_rate_500_trees.pdf")

model <- randomForest(hd ~ ., data=data.imputed, ntree=1000, proximity=TRUE)
model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Healthy"],
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
# ggsave("oob_error_rate_1000_trees.pdf")

oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

varImpPlot(model, main = "", pch = 20) 

# example 2
rfd <- read.delim("data/treedata.txt", header = T, sep = "\t")
set.seed(222)                       # set seed

id <- sample(2, nrow(rfd), replace = T, prob = c(.7, .3))
train <- rfd[id == 1, ]
test <- rfd[id == 2,]

like_rf1 <- randomForest(LikeUser~., data = train)
print(like_rf1) # inspect model

attributes(like_rf1)

ptrain1 <- predict(like_rf1, train) # extract prediction for training data
head(ptrain1); head(train$LikeUser)         # inspect predictions

confusionMatrix(ptrain1, train$LikeUser)  
ptest1 <- predict(like_rf1, test)
confusionMatrix(ptest1, test$LikeUser)

plot(like_rf1, main = "")

like_rf2 <- tuneRF(train[, !colnames(train)== "LikeUser"], 
                   train[, colnames(train)== "LikeUser"], 
                   stepFactor = 3, # for most values 3 appears to be optimal
                   plot = T, ntreeTry = 200, trace = T, improve = .05)

like_rf2 <- randomForest(LikeUser~., data = train, 
                         ntree = 200, ntry = 6, importance= T, proximity = T)
# inspect model
print(like_rf2)   

ptrain2 <- predict(like_rf2, train)
confusionMatrix(ptrain2, train$LikeUser)

ptest2 <- predict(like_rf2, test)
confusionMatrix(ptest2, test$LikeUser)

hist(treesize(like_rf2), main = "", col = "lightgray")

varImpPlot(like_rf2, main = "", pch = 20) 

importance(like_rf2)

varUsed(like_rf2)

partialPlot(like_rf2, train, Age)

getTree(like_rf2, 1, labelVar = T)

MDSplot(like_rf2, test$LikeUser)

###############################################################
# EXAMPLE 3

# set seed
set.seed(333)
# create initial model
like.rf <- cforest(LikeUser ~ Age + Gender + Status,
                   data = rfd, controls = cforest_unbiased(ntree = 50, mtry = 3))
# determine importance of factors
like.varimp <- varimp(like.rf, conditional = T)
round(like.varimp, 3)

# plot result
dotchart(sort(like.varimp), pch = 20, main = "Conditional importance of variables")


# evaluate random forst
like.rf.pred <- unlist(treeresponse(like.rf))[c(FALSE,TRUE)]
somers2(like.rf.pred, as.numeric(rfd$LikeUser) - 1)


cf1 <- cforest(LikeUser ~ . , data= rfd, control=cforest_unbiased(mtry=2,ntree=100)) # fit the random forest
varimp(cf1) # get variable importance, based on mean decrease in accuracy

# conditional=True, adjusts for correlations between predict
varimp(cf1, conditional=TRUE) 

varimpAUC(cf1)  # more robust towards class imbalance.

par(mar = c(5, 8, 4, 2) + 0.1)
plot(y = 1:length(varimpAUC(cf1)), x = varimpAUC(cf1)[order(varimpAUC(cf1))], 
     axes = F, ann = F, pch = 20, xlim = c(-0.01, 0.2), main = "Predictor Importance")
axis(1, at = seq(-0.01, 0.2, 0.05), seq(-0.01, 0.2, 0.05))
axis(2, at = 1:length(varimpAUC(cf1)), names(varimpAUC(cf1))[order(varimpAUC(cf1))], las = 2)
grid()
box()
par(mar = c(5, 4, 4, 2) + 0.1)

###############################################################
#                        BORUTA
# load data
borutadata <- read.delim("data/treedata.txt", header = T, sep = "\t")
head(borutadata)

# initial run
boruta1 <- Boruta(LikeUser~.,data=borutadata)
print(boruta1)

getConfirmedFormula(boruta1)

plot(boruta1, cex = .5)

plotImpHistory(boruta1)

par(mar = c(8, 8, 4, 2) + 0.1)
plot(boruta1, cex.axis=.75, las=2, xlab="", ylab = "", cex = .75, 
     col = c(rep("grey50", 3),rep("grey90", 3)))
abline(v = 3.5, lty = "dashed")
mtext("Predictors", 1, line = 6, at = 5, cex = 1)
mtext("Control", 1, line = 6, at = 2, cex = 1)
mtext("Importance", 2, line = 2.5, at = 20, cex = 1, las = 0)
par(mar = c(5, 4, 4, 2) + 0.1)

###############################################################
#                      END PART 2
###############################################################
