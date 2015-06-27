require(tidyr)
require(plyr)
require(dplyr)

## read in various data sets

subjects_train <- read.table("subject_train.txt", header=F)
subjects_test <- read.table("subject_test.txt", header = F)

trainData <- tbl_df(read.table("x_train.txt", header=F))
trainLabels <- tbl_df(read.table("y_train.txt",
                                 stringsAsFactors=F,
                                 header = F))

testData <- tbl_df(read.table("x_test.txt", header=F))
testLabels <- tbl_df(read.table("y_test.txt",
                                stringsAsFactors=F,
                                header=F))

activityLabels <- tbl_df(read.table("activity_labels.txt",
                                    stringsAsFactors=F,
                                    header=F))
measurementLabels <- read.csv("features.txt",sep=" ", header=F)

##consolidate test information to subject, activity, 
testSubAct <- cbind(subjects_test, testLabels)

## again for train information
trainSubAct <- cbind(subjects_train, trainLabels)

## consolidate activities and subjects, 
subAct <- rbind(testSubAct, trainSubAct)
colnames(subAct) <- c("Subject", "Activity")

## add descriptive labels to the activities 
## Requirement 3
for (x in 1:nrow(subAct)){
    y <- as.numeric(subAct$Activity[x])
    subAct$Activity[x] <- activityLabels[y,2]
}

## consolidate data
completeData <- rbind(testData, trainData)

## Clean column names
measurementLabels[,2] <- gsub("mean[(][)][-]", "Mean_", 
                              measurementLabels[,2])
measurementLabels[,2] <- gsub("mean[(][)]", "Mean_", 
                              measurementLabels[,2])
measurementLabels[,2] <- gsub("std[(][)][-]", "STD_",
                              measurementLabels[,2])
measurementLabels[,2] <- gsub("std[(][)]", "STD_",
                              measurementLabels[,2])
measurementLabels[,2] <- gsub("tBody", "Body", 
                              measurementLabels[,2])
measurementLabels[,2] <- gsub("fBodyBody", "fBody", 
                              measurementLabels[,2])
measurementLabels[,2] <- gsub("tGravity", "Gravity", 
                              measurementLabels[,2])


## assign clean labels to data 
## Requirement 4
colnames(completeData) <- measurementLabels$V2

## subset  columns for just means and stds, 
## Requirement 2
cleanData <- completeData[grepl("Mean", names(completeData)) |
                              grepl("STD", names(completeData)) |
                              grepl("meanFreq", names(completeData))]

## and combine into a complete table, 
## Requirement 1
finalInputData <- cbind(subAct, cleanData)

## housekeeping
rm(list = c("activityLabels", "cleanData", "completeData", "measurementLabels", "subAct", "subjects_test", "subjects_train", "testData", "trainData", "trainLabels", "testLabels", "testSubAct", "trainSubAct"))

## ****Requirement 5***
## New table with Mean for each variable,
## for each Subject,
## for each activity
## and write new table to file


tidyData <- tbl_df(finalInputData)
##convert list
tidyData[2] <- as.character(unlist(tidyData[2]))

## --break out subject and activity data again for use in Aggregate
##   which allows use of FUN mean to generate single mean for each 
##   activity/subject/value, then put it all back together in order.
tidySub <- tidyData$Subject
tidyAct <- tidyData$Activity
tidyData <- tidyData[3:88]
tidyData <- aggregate(tidyData, 
                      list(Activity = tidyAct, 
                           Subject = tidySub), 
                      FUN = "mean") %>%
    arrange(Activity, Subject)


write.csv(file="2run_analysis.csv", tidyData)