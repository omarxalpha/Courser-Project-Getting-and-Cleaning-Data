#

if(!file.exists("./Assignment_GandCD")){dir.create("./Assignment_GandCD")}

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileURL, "./Assignment_GandCD/projectdata.zip")

if(!file.exists("./Assignment_GandCD/UCI_dataset")){
unzip("./Assignment_GandCD/projectdata.zip")  
}

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

Xvars <- rbind(x_test, x_train)
Yvars <- rbind(y_test, y_train)
Subject_vars <- rbind(subject_test, subject_train)


base_complete <- cbind(Subject_vars, Yvars, Xvars)



Tidy <- base_complete %>% 
  select(subject, code, contains("mean"), contains("std"))

TidyData$code <- activities_labels[TidyData$code, 2]

names(Tidy)[2] = "activity"
names(Tidy)<-gsub("Acc", "Accelerometer", names(Tidy))
names(Tidy)<-gsub("Gyro", "Gyroscope", names(Tidy))
names(Tidy)<-gsub("BodyBody", "Body", names(Tidy))
names(Tidy)<-gsub("Mag", "Magnitude", names(Tidy))
names(Tidy)<-gsub("^t", "Time", names(Tidy))
names(Tidy)<-gsub("^f", "Frequency", names(Tidy))
names(Tidy)<-gsub("tBody", "TimeBody", names(Tidy))
names(Tidy)<-gsub("-mean()", "Mean", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("-std()", "STD", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("-freq()", "Frequency", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("angle", "Angle", names(Tidy))
names(Tidy)<-gsub("gravity", "Gravity", names(Tidy))

Second_tidy <- Tidy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(Second_tidy, "./Assignment_GandCD/ResumeData.txt", row.name=FALSE)
