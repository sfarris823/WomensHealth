########################################################################
##
## Dataset:    Women's Health
## Purpose:    Predict and individual's Segment and Subgroup
## URL:        https://www.kaggle.com/c/titanic
## Submitted:  December 11, 2016 - 0.80383
##
########################################################################

# Reset the environment
rm (list=ls())


########################################################################
##
## Install/Load needed libraries
##
########################################################################

# List of packages for session
.packages = c("caret",          # data partitioning
              "corrgram",       # correlation of variables
              "ggplot2",        # visualizations
              "randomForest",   # exploratory data analysis
              "rpart",          # recursive partitioning
              "rpart.plot",     # plotting decision trees
              "lubridate"       # generating unique filenames
            )

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, library, character.only=TRUE)
rm(.packages, .inst)


########################################################################
##
## Custom functions
##
########################################################################

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


########################################################################
##
## Get data
##
########################################################################

train.orig <- read.csv("WomenHealth_Training.csv", header=TRUE, stringsAsFactors=FALSE)

# Since I have only training data, I need to separate it into a train and test data set.
# I want a 70/30 split with stratification based on geo, segment, and subgroup.

# First I need to create the geosegsub variable (for stratification).
train.orig$geosegsub <- paste(train.orig$geo, train.orig$segment, train.orig$subgroup, sep="")

# Then I need to create the segsub variable, which I'll use for prediction later)
train.orig$segsub <- paste(train.orig$segment, train.orig$subgroup, sep="")

# Using the 'caret' library, I'll create a 70/30 split based on geosegsub and extract the data
# into a train and test data set.
partition <- createDataPartition(train.orig$geosegsub, times=1, p=.70, list=FALSE)
train.raw <- train.orig[ partition,]
test.orig  <- train.orig[-partition,]   # We'll use test.orig later to compare the model predictions
test.raw <- test.orig

# Cleanup variables no longer needed
rm(partition, train.orig)
test.raw$geosegsub <- "Unk"
test.raw$segment <-"Unk"
test.raw$subgroup <- "Unk"
test.raw$segsub <- "Unk"

# Now that the data is the way I want it to be, I'll recombined the data so that I can 
# performance exploratory data analysis
combined <- rbind(train.raw, test.raw)


########################################################################
##
## Add new features
##
########################################################################

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

# if multpart=="Unk" & EVER_HAD_SEX==0 & EVER_BEEN_PREGNANT==0 then multpart=0
combined[which(combined$multpart=="Unk" & combined$EVER_HAD_SEX==0 & combined$EVER_BEEN_PREGNANT==0),"multpart"] <- 0

combined$sexactive <- 0
combined[which(combined$EVER_HAD_SEX==1 | combined$EVER_BEEN_PREGNANT==1 | 
               combined$multpart!="0"   | combined$ModCon==1 |
               combined$usecondom==1 ),"sexactive"]  <- 1

#######################################################################
##
## Exploratory Data Analysis
##
########################################################################

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
combined$segsub <- factorit(combined$segsub)
combined$sexactive <- factorit(combined$sexactive)

# Let's see if there are any standout indicators or correlation in our data.
# Segment - geo, muslim, babydoc
# Subgroup - hivknow, modcon, multpart, christian
train.raw$segsub <- as.numeric(train.raw$segsub)
#corrgram(train.raw)

# random forests
set.seed(1234)
rf.1.features <- c("ModCon", "LaborDeliv", "multpart", "tribe", "REGION_PROVINCE", "geo", "babydoc", "hivknow", "sexactive") #~19.84
rf.1 <- randomForest(x = combined[1:3715, rf.1.features], y = as.factor(train.raw$segsub), importance = T, ntree = 3000)
rf.1
varImpPlot(rf.1)

# cross validation

set.seed(37596)
cv.10.folds <- createMultiFolds(as.factor(train.raw$segsub), k = 3, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.10.folds)

set.seed(94622)
rf.1.cv.1 <- train(x=combined[1:3715, rf.1.features], y=as.factor(train.raw$segsub), 
                   method="rf", tuneLength=2, ntree=64, trControl=ctrl.1)
rf.1.cv.1



rpart.train.1 <- combined[1:3715, rf.1.features]
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, as.factor(train.raw$segsub), ctrl.1)
rpart.1.cv.1
prp(rpart.1.cv.1$finalModel, type=0, extra=1, under=T)
rpart.1.cv.1$finalModel

########################################################################
##
##  Generate Submission
##
########################################################################

########################################################################
##
##  Generate Submission
##
########################################################################

# Collect the test data with all the new features I've built.
test.submission <- combined[3716:5283, rf.1.features]

# Predict whether the test data individuals survived or perished
rpart.1.predictions <- predict(rpart.1.cv.1$finalModel, test.submission, type="class")
length(rpart.1.predictions)
table(rpart.1.predictions)

# Build and format the actual submission data according to requirements
# +-------------+---------+----------+
# | patientID   | Segment | Subgroup |
# +-------------+---------+----------+
# | xxxx        | 1 to 4  | 1 or 2   |
# +-------------+---------+----------+
submission <- data.frame(patientID=test.orig[labels(rpart.1.predictions),"patientID"], 
                         segment=substring(rpart.1.predictions,1,1), 
                         subgroup=substring(rpart.1.predictions,2,2))

# Write the submission out to CSV with no row names.
filename <- paste(c("WOMENSHEALTH-", format(now(), "%Y%m%d_%H%M%S"), ".csv"), sep="", collapse="")
write.csv(submission, file=filename, row.names=FALSE)

# Compare results for improvements
compare <- data.frame(patientID=test.orig$patientID,
                      segment=test.orig$segment,
                      subgroup=test.orig$subgroup,
                      predicted_segment=submission$segment,
                      predicted_subgroup=submission$subgroup,
                      prediction_correct=ifelse(test.orig$segment==submission$segment & test.orig$subgroup==submission$subgroup, TRUE, FALSE))

message("Accuracy:  ",format(length(which(compare$prediction_correct==TRUE))/nrow(compare)*100, digits=4), "%")

