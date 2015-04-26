#set working dir
setwd("C:\\coursera\\GettingAndCleanData")

library(data.table)
library(dplyr)

#Create dir for analysis
if(!file.exists("./data")){dir.create("./data")}

#Download the data file and unzip the file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Data.zip",mode="wb")
unzip(zipfile="./data/Data.zip",exdir="./data")

#Read the variuos data files 
featuresData <- read.table("./data/UCI HAR Dataset/features.txt")
actlabelData <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE)
subtrainData <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
ytrainData <- read.table("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
xtrainData <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjtestData <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
ytestData <- read.table("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
xtestData <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Merge the train and test data sets
subject <- rbind(subtrainData, subjtestData)
features <- rbind(xtrainData, xtestData)
activity <- rbind(ytrainData, ytestData)

#Rename the columns for subject and activity DF which has one varaible
names(subject)<-c("subject")
names(activity)<- c("activity")

#Assign column names to features DF
names(features) <- featuresData$V2

#Merge all the 3 datasets to get the final data set
finalData <- cbind(features,activity,subject)

#Extracts only the measurements on the mean and standard deviation for each measurement.
meansdData <- grep(".*Mean.*|.*Std.*", names(finalData), ignore.case=TRUE)

#Derive at the final extracted DF
requiredColumns <- c(meansdData, 562, 563)
extractedData <- finalData[,requiredColumns]

#Using descriptive activity names to name the activities in the data set
#Convert the activity field to character
extractedData$activity <- as.character(extractedData$activity)

#Assign activity label names to activtity field in extractedData
for (i in 1:nrow(actlabelData)){
        extractedData$activity[extractedData$activity == i] <- as.character(actlabelData[i,2])
}

#Convert the activity column to factor
extractedData$activity <- as.factor(extractedData$activity)

#Appropriately labels the data set with descriptive variable names. 
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
#Change subject as factory variable
extractedData$subject <- as.factor(extractedData$subject)
extractedData <- data.table(extractedData)
#Aggregateby each variable by each activity and foreach subject
tidyData <- aggregate(. ~subject + activity, extractedData, mean)
#order the data
tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
#write thefinal output to table
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)