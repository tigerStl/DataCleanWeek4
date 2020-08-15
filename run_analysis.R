library(dplyr)

fn <- "getdata_projectfiles_UCI HAR Dataset.zip"

if (!file.exists(fn)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, fn)
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(fn) 
}

# read files one by one to variables,and set colnames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
nrow(subject_test)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# step 1 Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

# step 2 get the tidyData from select and
# step 3 Uses descriptive activity names to name the activities in the data set
dataBeenTidy <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
dataBeenTidy$code <- activities[dataBeenTidy$code, 2]

# Step 4: Appropriately labels the data set with descriptive variable names
names(dataBeenTidy)[2] = "activity"
names(dataBeenTidy)<-gsub("Acc", "Accelerometer", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("Gyro", "Gyroscope", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("BodyBody", "Body", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("Mag", "Magnitude", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("^t", "Time", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("^f", "Frequency", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("tBody", "TimeBody", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("-mean()", "Mean", names(dataBeenTidy), ignore.case = TRUE)
names(dataBeenTidy)<-gsub("-std()", "STD", names(dataBeenTidy), ignore.case = TRUE)
names(dataBeenTidy)<-gsub("-freq()", "Frequency", names(dataBeenTidy), ignore.case = TRUE)
names(dataBeenTidy)<-gsub("angle", "Angle", names(dataBeenTidy))
names(dataBeenTidy)<-gsub("gravity", "Gravity", names(dataBeenTidy))

# write data to targetdata.txt
# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TargetData <- dataBeenTidy %>%
group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(TargetData, "TargetData.txt", row.name=FALSE)
