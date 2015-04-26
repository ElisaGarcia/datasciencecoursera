setwd("/Users/elisagarciacorisco/Documents/workspace/DataScience/cleaning data")
library(plyr)
library(dplyr)

# read test data
Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
# build data set test data
testAndTrain <- data.frame(id = subjectTest[[1]], labels = Ytest[[1]])
testAndTrain <- cbind(testAndTrain, Xtest)

# read train data
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
# build data set train data
train <-  data.frame(id = subjectTrain[[1]], labels = Ytrain[[1]])
train <- cbind(train, Xtrain)

# merge train and test data in a data set
testAndTrain <- merge(testAndTrain, train, all=TRUE)

# select those columns which contain the mean and the std
measurements <- c(1:6, 41:46, 81:86, 121:126, 161:166, 201:206, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)
names <- read.table("././UCI HAR Dataset/features.txt")
names <- names[measurements,2]
measurements <- measurements + 2

# create the data frame with the id (subject), the label(still in number) the means, and the stds
dataTestAndTrain <- data.frame(id = testAndTrain$id, labels = testAndTrain$labels)
dataTestAndTrain <- cbind(dataTestAndTrain, testAndTrain[, measurements])

# rename the activities
dataTestAndTrain$labels <- revalue(as.factor(dataTestAndTrain$labels), c("1" = "Walking", "2" = "Walking.Upstairs", "3" = "Walking.DownStairs", "4" = "Sitting", "5" = "Standing", "6" = "Laying"))

# Appropriately labels the data set with descriptive variable names
extraNames <- c("Subject", "ActivityLabel")
names <- c(extraNames, as.character(names))
colnames(dataTestAndTrain) <- names

names[3:length(names)] <- paste("mean.", names[3:length(names)], sep="")
dataClean <- read.table(text="", col.names=names)
for(subject in levels(as.factor(dataTestAndTrain$Subject)))
{
  for(activity in levels(dataTestAndTrain$ActivityLabel))
  {
    dataSubjectActivity <- filter(dataTestAndTrain, Subject == subject, ActivityLabel == activity)
    dataTmp <- data.frame(Subject = subject, ActivityLabel = activity)
    for(variable in colnames(dataTestAndTrain)[3:length(colnames(dataTestAndTrain))])
    {
      dataTmp[[variable]] <- mean(dataSubjectActivity[[variable]])
    }
    dataClean <- rbind(dataClean, dataTmp) 
  }
}

dataClean



