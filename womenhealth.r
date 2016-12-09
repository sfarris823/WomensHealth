rm (list=ls())
train.raw <- read.csv("WomenHealth_Training.csv", header=TRUE, stringsAsFactors=FALSE)

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

showhist(train, "india", "subgroup", ~geo+india)


library(corrgram)
#corrgram(train.raw)
