rm (list=ls())
train.raw <- read.csv("WomenHealth_Training.csv", header=TRUE, stringsAsFactors=FALSE)
train.raw$religion.adj <- train.raw$religion

# Classify religion into Christian vs. Non-Christian vs. Other (Unknowns)
#train.raw$religion.adj[train.raw$religion == "Evangelical/Bo"] <- "Christian"               #count: 416
#train.raw$religion.adj[train.raw$religion == "Other Christia"] <- "Christian"               #count: 1071
#train.raw$religion.adj[train.raw$religion == "Roman Catholic"] <- "Christian"               #count: 640
#train.raw$religion.adj[train.raw$religion == "Russian/Easter"] <- "Christian"               #count: 211
#train.raw$religion.adj[train.raw$religion == "Jewish"] <- "Non-Christian"                   #count: 2
#train.raw$religion.adj[train.raw$religion == "Traditional/An"] <- "Non-Christian"           #count: 32
#train.raw$religion.adj[train.raw$religion == "Muslim"] <- "Non-Christian"                   #count: 1226
#train.raw$religion.adj[train.raw$religion == "Buddhist"] <- "Non-Christian"                 #count: 580
#train.raw$religion.adj[train.raw$religion == "Hindu"] <- "Non-Christian"                    #count: 998
#train.raw$religion.adj[train.raw$religion == ""] <- "Other"                                 #count: 15+92

# Classify religion into Muslim vs. Non-Muslim [Other/Unknowns treated as Non-Muslim]
# This seems to be the best approach, in that it both simplifies the model
# and contributes to better accuracy.
train.raw$religion.adj[train.raw$religion == "Evangelical/Bo"] <- "Non-Muslim"              #count: 416
train.raw$religion.adj[train.raw$religion == "Other Christia"] <- "Non-Muslim"              #count: 1071
train.raw$religion.adj[train.raw$religion == "Roman Catholic"] <- "Non-Muslim"              #count: 640
train.raw$religion.adj[train.raw$religion == "Russian/Easter"] <- "Non-Muslim"              #count: 211
train.raw$religion.adj[train.raw$religion == "Jewish"] <- "Non-Muslim"                      #count: 2
train.raw$religion.adj[train.raw$religion == "Traditional/An"] <- "Non-Muslim"              #count: 32
train.raw$religion.adj[train.raw$religion == "Muslim"] <- "Muslim"                          #count: 1226
train.raw$religion.adj[train.raw$religion == "Buddhist"] <- "Non-Muslim"                    #count: 580
train.raw$religion.adj[train.raw$religion == "Hindu"] <- "Non-Muslim"                       #count: 998
train.raw$religion.adj[train.raw$religion == ""] <- "Non-Muslim"                            #count: 15+92

# Classify religion into Christian vs. Muslim vs. vs. Buddhist vs. Other (Unknowns)
#train.raw$religion.adj[train.raw$religion == "Evangelical/Bo"] <- "Christian"               #count: 416
#train.raw$religion.adj[train.raw$religion == "Other Christia"] <- "Christian"               #count: 1071
#train.raw$religion.adj[train.raw$religion == "Roman Catholic"] <- "Christian"               #count: 211
#train.raw$religion.adj[train.raw$religion == "Russian/Easter"] <- "Christian"               #count: 211
#train.raw$religion.adj[train.raw$religion == "Muslim"] <- "Muslim"                          #count: 1226
#train.raw$religion.adj[train.raw$religion == "Buddhist"] <- "Buddhist"                      #count: 580
#train.raw$religion.adj[train.raw$religion == "Jewish"] <- "Other"                           #count: 2
#train.raw$religion.adj[train.raw$religion == "Traditional/An"] <- "Other"                   #count: 32
#train.raw$religion.adj[train.raw$religion == "Hindu"] <- "Other"                            #count: 998
#train.raw$religion.adj[train.raw$religion == ""] <- "Other"                                 #count: 15+92

# Making some assumptions here to eliminate some NAs in the hivknow column:
# * if EVER_HAD_SEX==0 (never had sex), assume you know you don't have HIV (hivknow==1)
train.raw$hivknow[is.na(train.raw$hivknow) & train.raw$EVER_HAD_SEX==1] <- 1

train.raw$geosegsub <- paste(train.raw$geo, train.raw$segment, train.raw$subgroup, sep="")

train <- train.raw

factorit <- function (dframe, na="Unk") 
  {
    dframe[which(is.na(dframe))] <- na
    dframe[which(dframe=="")] <- na
    return (as.factor(dframe))
  }

train$geo <- factorit(train$geo)
train$christian <- factorit(train$christian)
train$muslim <- factorit(train$muslim)
train$hindu <- factorit(train$hindu)
train$other <- factorit(train$other)
train$cellphone <- factorit(train$cellphone)
train$motorcycle <- factorit(train$motorcycle)
train$radio <- factorit(train$radio)
train$cooker <- factorit(train$cooker)
train$fridge <- factorit(train$fridge)
train$furniture <- factorit(train$furniture)
train$computer <- factorit(train$computer)
train$cart <- factorit(train$cart)
train$irrigation <- factorit(train$irrigation)
train$thrasher <- factorit(train$thrasher)
train$car <- factorit(train$car)
train$generator <- factorit(train$generator)
train$electricity <- factorit(train$electricity)
train$age <- factorit(train$age)
train$foodinsecurity <- factorit(train$foodinsecurity)
train$EVER_HAD_SEX <- factorit(train$EVER_HAD_SEX)
train$EVER_BEEN_PREGNANT <- factorit(train$EVER_BEEN_PREGNANT)
train$CHILDREN <- factorit(train$CHILDREN)
train$india <- factorit(train$india)
train$married <- factorit(train$married)
train$multpart <- factorit(train$multpart)
train$inschool <- factorit(train$inschool)
train$ownincome <- factorit(train$ownincome)
train$literacy <- factorit(train$literacy)
train$religion <- factorit(train$religion)
train$urbanicity <- factorit(train$urbanicity)
train$LaborDeliv <- factorit(train$LaborDeliv)
train$babydoc <- factorit(train$babydoc)
train$Debut <- factorit(train$Debut)
train$ModCon <- factorit(train$ModCon)
train$usecondom <- factorit(train$usecondom)
train$hivknow <- factorit(train$hivknow)
train$lowlit <- factorit(train$lowlit)
train$highlit <- factorit(train$highlit)
train$urban <- factorit(train$urban)
train$rural <- factorit(train$rural)
train$single <- factorit(train$single)
train$segment <- factorit(train$segment)
train$subgroup <- factorit(train$subgroup)
train$geosegsub <- factorit(train$geosegsub)
train$religion.adj <- factorit(train$religion.adj)
train$geo.adj <- factorit(train$geo.adj)

# Load up ggplot2 package to use for visualizations
library(ggplot2)

showhist <- function(data, xaxis, fillgrp, facetwrap) 
  {
    ggplot(data, aes(x = data[,xaxis], fill = data[,fillgrp])) +
      stat_count(width = 0.5) + 
      facet_wrap(facetwrap) +
      #ggtitle(paste(xaxis,"by", fillgrp)) +
      xlab(xaxis) +
      ylab("Total Count") +
      labs(fill = fillgrp)
  }

#showhist(train, "india", "subgroup", ~geo+india)


library(corrgram)
#corrgram(train.raw)

# random forests
library(randomForest)
set.seed(1234)
rf.1.features <- c("geo", "religion.adj", "hivknow", "multpart", "married", "CHILDREN")
rf.1 <- randomForest(x = train[, rf.1.features], y = as.factor(train$subgroup), 
                     importance = T, ntree = 1000)
rf.1
varImpPlot(rf.1)

# cross validation
library(caret)
set.seed(37596)
cv.10.folds <- createMultiFolds(as.factor(train$subgroup), k = 3, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.10.folds)

set.seed(94622)
rf.1.cv.1 <- train(x=train[, rf.1.features], y=as.factor(train$subgroup), 
                   method="rf", tuneLength=2, ntree=64, trControl=ctrl.1)
rf.1.cv.1


library(rpart)
library(rpart.plot)

# rpart.cv() trains a predictive model 
rpart.cv <- function(seed, training, labels, ctrl)
{
  set.seed(seed)
  rpart.cv <- train(x=training, y=labels, method="rpart", tuneLength=30, trControl=ctrl)
  return (rpart.cv)
}




rpart.train.1 <- train[, rf.1.features]
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, as.factor(train$subgroup), ctrl.1)
rpart.1.cv.1
prp(rpart.1.cv.1$finalModel, type=0, extra=1, under=T)
rpart.1.cv.1$finalModel
