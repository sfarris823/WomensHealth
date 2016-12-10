########################
#### The purpose of this project is to be able to predict the segment and subgroup of an individual
########################

# Reset the environment
rm (list=ls())


##
## Install/Load needed libraries
##

# List of packages for session
.packages = c("caret",          # data partitioning
              "ggplot2",        # visualizations
              "randomForest",   # exploratory data analysis
              "rpart",
              "rpart.plot",
              "corrgram"
            )

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, library, character.only=TRUE)
rm(.packages)
rm(.inst)


##
## Custom functions
##

# factorit() will take a data frame column and replace NAs, empty 
# strings with another value and returns the column as a factor
factorit <- function (dframe, na="Unk") 
{
  dframe[which(is.na(dframe))] <- na
  dframe[which(dframe=="")] <- na
  return (as.factor(dframe))
}

# showhist() builds a histogram using a facetwrap
showhist <- function(data, xaxis, fillgrp, facetwrap, title) 
{
  ggplot(data, aes_string(x = xaxis, fill = fillgrp)) +
    stat_count(width = 0.5) + 
    facet_wrap(facetwrap) +
    ggtitle(title) +
    xlab(xaxis) +
    ylab("Total Count") +
    labs(fill = fillgrp)
}

# rpart.cv() trains a predictive model 
rpart.cv <- function(seed, training, labels, ctrl)
{
  set.seed(seed)
  rpart.cv <- train(x=training, y=labels, method="rpart", tuneLength=30, trControl=ctrl)
  return (rpart.cv)
}


##
## Get data
##
train.orig <- read.csv("WomenHealth_Training.csv", header=TRUE, stringsAsFactors=FALSE)

# Since I have only training data, I need to separate it into a train and test data set.
# I want a 70/30 split with stratification based on geo, segment, and subgroup.

# First I need to create the geosegsub variable.
train.orig$geosegsub <- paste(train.orig$geo, train.orig$segment, train.orig$subgroup, sep="")

# Using the 'caret' library, I'll create a 70/30 split based on geosegsub and extract the data
# into a train and test data set.
partition <- createDataPartition(train.orig$geosegsub, times=1, p=.70, list=FALSE)
train.raw <- train.orig[ partition,]
test.raw  <- train.orig[-partition,]

# Cleanup variables no longer needed
rm(partition)
rm(train.orig)
test.raw$geosegsub <- "Unk"
test.raw$segment <-"Unk"
test.raw$subgroup <- "Unk"

# Now that the data is the way I want it to be, I'll recombined the data so that I can 
# performance exploratory data analysis
combined <- rbind(train.raw, test.raw)


##
## Explore the data
##

combined$religion.adj <- combined$religion

# Classify religion into Christian vs. Non-Christian vs. Other (Unknowns)
#combined$religion.adj[combined$religion == "Evangelical/Bo"] <- "Christian"               #count: 416
#combined$religion.adj[combined$religion == "Other Christia"] <- "Christian"               #count: 1071
#combined$religion.adj[combined$religion == "Roman Catholic"] <- "Christian"               #count: 640
#combined$religion.adj[combined$religion == "Russian/Easter"] <- "Christian"               #count: 211
#combined$religion.adj[combined$religion == "Jewish"] <- "Non-Christian"                   #count: 2
#combined$religion.adj[combined$religion == "Traditional/An"] <- "Non-Christian"           #count: 32
#combined$religion.adj[combined$religion == "Muslim"] <- "Non-Christian"                   #count: 1226
#combined$religion.adj[combined$religion == "Buddhist"] <- "Non-Christian"                 #count: 580
#combined$religion.adj[combined$religion == "Hindu"] <- "Non-Christian"                    #count: 998
#combined$religion.adj[combined$religion == ""] <- "Other"                                 #count: 15+92

# Classify religion into Muslim vs. Non-Muslim [Other/Unknowns treated as Non-Muslim]
# This seems to be the best approach, in that it both simplifies the model
# and contributes to better accuracy.
combined$religion.adj[combined$religion == "Evangelical/Bo"] <- "Non-Muslim"              #count: 416
combined$religion.adj[combined$religion == "Other Christia"] <- "Non-Muslim"              #count: 1071
combined$religion.adj[combined$religion == "Roman Catholic"] <- "Non-Muslim"              #count: 640
combined$religion.adj[combined$religion == "Russian/Easter"] <- "Non-Muslim"              #count: 211
combined$religion.adj[combined$religion == "Jewish"] <- "Non-Muslim"                      #count: 2
combined$religion.adj[combined$religion == "Traditional/An"] <- "Non-Muslim"              #count: 32
combined$religion.adj[combined$religion == "Muslim"] <- "Muslim"                          #count: 1226
combined$religion.adj[combined$religion == "Buddhist"] <- "Non-Muslim"                    #count: 580
combined$religion.adj[combined$religion == "Hindu"] <- "Non-Muslim"                       #count: 998
combined$religion.adj[combined$religion == ""] <- "Non-Muslim"                            #count: 15+92

# Classify religion into Christian vs. Muslim vs. vs. Buddhist vs. Other (Unknowns)
#combined$religion.adj[combined$religion == "Evangelical/Bo"] <- "Christian"               #count: 416
#combined$religion.adj[combined$religion == "Other Christia"] <- "Christian"               #count: 1071
#combined$religion.adj[combined$religion == "Roman Catholic"] <- "Christian"               #count: 211
#combined$religion.adj[combined$religion == "Russian/Easter"] <- "Christian"               #count: 211
#combined$religion.adj[combined$religion == "Muslim"] <- "Muslim"                          #count: 1226
#combined$religion.adj[combined$religion == "Buddhist"] <- "Buddhist"                      #count: 580
#combined$religion.adj[combined$religion == "Jewish"] <- "Other"                           #count: 2
#combined$religion.adj[combined$religion == "Traditional/An"] <- "Other"                   #count: 32
#combined$religion.adj[combined$religion == "Hindu"] <- "Other"                            #count: 998
#combined$religion.adj[combined$religion == ""] <- "Other"                                 #count: 15+92

# Making some assumptions here to eliminate some NAs in the hivknow column:
# If EVER_HAD_SEX==0 (never had sex), assume you know you don't have HIV (hivknow==1)
combined$hivknow[is.na(combined$hivknow) & combined$EVER_HAD_SEX==1] <- 1

# Convert as much as possible to factors.
combined$geo <- factorit(combined$geo)
combined$christian <- factorit(combined$christian)
combined$muslim <- factorit(combined$muslim)
combined$hindu <- factorit(combined$hindu)
combined$other <- factorit(combined$other)
combined$cellphone <- factorit(combined$cellphone)
combined$motorcycle <- factorit(combined$motorcycle)
combined$radio <- factorit(combined$radio)
combined$cooker <- factorit(combined$cooker)
combined$fridge <- factorit(combined$fridge)
combined$furniture <- factorit(combined$furniture)
combined$computer <- factorit(combined$computer)
combined$cart <- factorit(combined$cart)
combined$irrigation <- factorit(combined$irrigation)
combined$thrasher <- factorit(combined$thrasher)
combined$car <- factorit(combined$car)
combined$generator <- factorit(combined$generator)
combined$electricity <- factorit(combined$electricity)
combined$age <- factorit(combined$age)
combined$foodinsecurity <- factorit(combined$foodinsecurity)
combined$EVER_HAD_SEX <- factorit(combined$EVER_HAD_SEX)
combined$EVER_BEEN_PREGNANT <- factorit(combined$EVER_BEEN_PREGNANT)
combined$CHILDREN <- factorit(combined$CHILDREN)
combined$india <- factorit(combined$india)
combined$married <- factorit(combined$married)
combined$multpart <- factorit(combined$multpart)
combined$inschool <- factorit(combined$inschool)
combined$ownincome <- factorit(combined$ownincome)
combined$literacy <- factorit(combined$literacy)
combined$religion <- factorit(combined$religion)
combined$urbanicity <- factorit(combined$urbanicity)
combined$LaborDeliv <- factorit(combined$LaborDeliv)
combined$babydoc <- factorit(combined$babydoc)
combined$Debut <- factorit(combined$Debut)
combined$ModCon <- factorit(combined$ModCon)
combined$usecondom <- factorit(combined$usecondom)
combined$hivknow <- factorit(combined$hivknow)
combined$lowlit <- factorit(combined$lowlit)
combined$highlit <- factorit(combined$highlit)
combined$urban <- factorit(combined$urban)
combined$rural <- factorit(combined$rural)
combined$single <- factorit(combined$single)
combined$segment <- factorit(combined$segment)
combined$subgroup <- factorit(combined$subgroup)
combined$geosegsub <- factorit(combined$geosegsub)
combined$religion.adj <- factorit(combined$religion.adj)

# Let's see if there are any standout indicators or correlation in our data.
# Segment - geo, muslim, babydoc
# Subgroup - hivknow, modcon, multpart, christian
#corrgram(train.raw)

# random forests
set.seed(1234)
#rf.1.features <- c("geo", "religion.adj", "hivknow", "multpart", "married", "CHILDREN")
#rf.1.features <- c("geo", "muslim", "babydoc")
rf.1.features <- c("geo", "hivknow", "ModCon", "multpart", "religion.adj")
rf.1 <- randomForest(x = combined[1:3715, rf.1.features], y = as.factor(train.raw$subgroup), 
                     importance = T, ntree = 1000)
rf.1
varImpPlot(rf.1)

# cross validation

set.seed(37596)
cv.10.folds <- createMultiFolds(as.factor(train.raw$subgroup), k = 3, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.10.folds)

set.seed(94622)
rf.1.cv.1 <- train(x=combined[1:3715, rf.1.features], y=as.factor(train.raw$subgroup), 
                   method="rf", tuneLength=2, ntree=64, trControl=ctrl.1)
rf.1.cv.1



rpart.train.1 <- combined[1:3715, rf.1.features]
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, as.factor(train.raw$subgroup), ctrl.1)
rpart.1.cv.1
prp(rpart.1.cv.1$finalModel, type=0, extra=1, under=T)
rpart.1.cv.1$finalModel
