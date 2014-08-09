## Setting working directory.
setwd("/Users/adrienne716/Desktop/Summer/Coursera/Getting&CleaningData/Project")

## Quetion 1: merge the training and test sets to create one data set.
# Reading the data.
trainData <- read.table("./UCI_HAR_Dataset/train/X_train.txt")
dim(trainData) # 7352*561
trainLabel <- read.table("./UCI_HAR_Dataset/train/y_train.txt")
dim(trainLabel) # 7352*1
trainSubject <- read.table("./UCI_HAR_Dataset/train/subject_train.txt")
dim(trainSubject) # 7352*1
testData <- read.table("./UCI_HAR_Dataset/test/X_test.txt")
dim(testData) # 2947*561
testLabel <- read.table("./UCI_HAR_Dataset/test/y_test.txt")
dim(testLabel) # 2947*1
testSubject <- read.table("./UCI_HAR_Dataset/test/subject_test.txt")
dim(testSubject) # 2947*1

# Merging the data.
mergeData <- rbind(trainData, testData)
dim(mergeData) # 10299*561
mergeLabel <- rbind(trainLabel, testLabel)
dim(mergeLabel) # 10299*1
mergeSubject <- rbind(trainSubject, testSubject)
dim(mergeSubject) # 10299*1

## Question 2: Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("./UCI_HAR_Dataset/features.txt")
dim(features) # 561*2
index <- grep("mean\\(\\)|std\\(\\)", features[, 2], ignore.case = TRUE)
length(index) # 66
mergeData <- mergeData[,index]
dim(mergeData) # 10299*66
names(mergeData) <- gsub("\\(\\)", "", features[index, 2]) # remove "()"
names(mergeData) <- gsub("mean", "Mean", names(mergeData)) # capitalize M in mean
names(mergeData) <- gsub("std", "Std", names(mergeData)) # capitalize S in std
names(mergeData) <- gsub("-", "", names(mergeData)) # remove "-" in column names 

## Question 3: Uses descriptive activity names to name the activities in the data set
activity <- read.table("./UCI_HAR_Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[mergeLabel[,1], 2]
mergeLabel[,1] <- activityLabel
names(mergeLabel) <- "activity"

## Question 4: Appropriately labels the data set with descriptive activity names.
names(mergeSubject) <- "subject"
cleanedData <- cbind(mergeSubject, mergeLabel, mergeData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

## Question 5: Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(mergeSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) # create an empty matrix
result <- as.data.frame(result) # Coerce the matrix into a data frame
colnames(result) <- colnames(cleanedData) # label the data set
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(mergeSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
head(result)
write.table(result, "data_with_means.txt") # write out the 2nd dataset