# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for 
# each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

setwd("~/rcode/gettingcleaningdata")
if(!file.exists("data")){
  dir.create("data")
}

zipfileurl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(url = zipfileurl,temp)
unziped_data <- unzip(temp, exdir = "./data")
# Remove temp file.
unlink(temp)
list.files("./data/UCI HAR Dataset")

# List of all features.
featuresList = read.table("./data/UCI HAR Dataset/features.txt", sep = "-", fill = TRUE )
colnames(featuresList) <- c("Signal", "Summary statistic", "Direction")
# Links the class labels with their activity name.
activityLabels = read.table("./data/UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c("ActivityIndex", "ActivityLabel")
# Training set.
trainFeatures = read.table("./data/UCI HAR Dataset/train/X_train.txt")
# Training labels.
trainLabels = read.table("./data/UCI HAR Dataset/train/y_train.txt")
# Test set.
testFeatures = read.table("./data/UCI HAR Dataset/test/X_test.txt")
# Test labels.
testLabels = read.table("./data/UCI HAR Dataset/test/y_test.txt")
# Subjects  in train set
trainSubjects = read.table("./data/UCI HAR Dataset/train/subject_train.txt")
colnames(trainSubjects) <- "Subject"
# Subjects in test set
testSubjects = read.table("./data/UCI HAR Dataset/test/subject_test.txt")
colnames(testSubjects) <- c("Subject")

# Data cleaning
featuresList$Signal <- str_trim(gsub(pattern = "\\d", replacement = "", x = featuresList$Signal))
featuresList$`Summary statistic` <- as.character(featuresList$`Summary statistic`)
featuresList$`Summary statistic` = substr(featuresList$`Summary statistic`,1,nchar(featuresList$`Summary statistic`)-2)

# 1. Merge training and test set into one.
trainData <- data.frame(trainFeatures, trainLabels, trainSubjects)
testData <- data.frame(testFeatures, testLabels, testSubjects)
mergedData <- rbind(trainData, testData)
colnames(mergedData)[dim(mergedData)[2]-1] <- "Activity"
colnames(mergedData)[dim(mergedData)[2]] <- "Subject"

# 2. Extract only the measurements on the mean and standard deviation for 
# each measurement.
library(stringr)
summarystatistic = str_trim(featuresList$`Summary statistic`)
index <- sort(c(grep(pattern  = "mean()", summarystatistic) , grep("std()", summarystatistic)))
indexmeanfreq <- grep(pattern  = "meanFreq()", summarystatistic)
index <-index[!index %in% indexmeanfreq]

#filter out data based on the desired index
filteredData <- mergedData[, index]
filteredData <- cbind(filteredData, mergedData$Activity)
colnames(filteredData)[dim(filteredData)[2]] <- "Activity"

filteredData <- cbind(filteredData, mergedData$Subject)
colnames(filteredData)[dim(filteredData)[2]] <- "Subject"
#filter out features based on the desired index
featuresList <- featuresList[index,]

# 3. Use descriptive activity names to name the activities in the data set
activityData <- filteredData$Activity
activityData <- activityLabels[activityData,]
filteredData$Activity <- activityData[,2]

# 4. Appropriately label the data set with descriptive variable names.
featuresList[, "Fullname"] <- paste(featuresList[,1], featuresList[,2], featuresList[,3], sep = "_")
# strip last character if it is "_" (i.e. no direction info associated to the feature name)
fullfeaturename <- featuresList[, "Fullname"] 
trim_index <- substring(fullfeaturename, nchar(fullfeaturename), nchar(fullfeaturename))=="_"
fullfeaturename[trim_index] <- substr(fullfeaturename[trim_index], 0, nchar(fullfeaturename[trim_index])-1)
featuresList[, "Fullname"]  <- fullfeaturename

colnames(filteredData) <- c(featuresList$Fullname, "Activity", "Subject")

# 5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
library(dplyr)
tidyData <- filteredData %>% 
  group_by(Activity, Subject) %>% 
  summarise_each(funs(mean))

