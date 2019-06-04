#**********************************************#
#    Merges the training and the test sets to  #
#       create one data set                    #  
#**********************************************#

#Generate a folder to save the files

if(!file.exists("./Assignment_GandCD")){dir.create("./Assignment_GandCD")}

#Save the link and then use it to download the files.

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, "./Assignment_GandCD/projectdata.zip")

#Unzip the files and generate the necessary folder

if(!file.exists("./Assignment_GandCD/UCI_dataset")){
unzip("./Assignment_GandCD/projectdata.zip")  
}

#Call the folder and all the data inside it

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Combine bases within the file to generate an ordered base

Xvars <- rbind(x_test, x_train)
Yvars <- rbind(y_test, y_train)
Subject_vars <- rbind(subject_test, subject_train)

base_complete <- cbind(Subject_vars, Yvars, Xvars)

#***************************************************#
#  Extracts only the measurements on the mean and   #
#     standard deviation for each measurement.      #
#***************************************************#

#Filter the information to only work with the data indicated in the project

Tidy <- base_complete %>% 
  select(subject, code, contains("mean"), contains("std"))

#*******************************************************#
#Uses descriptive activity names to name the activities # 
#                        in the data set                #
#*******************************************************#

#Change the description of the activities

TidyData$code <- activities_labels[TidyData$code, 2]
names(Tidy)[2] = "activity"

#****************************************************#
#Appropriately labels the data set with descriptive  #
#                 variable names                     # 
#****************************************************#
#change the descprición of each variable, eliminating 
#its avrebiación and using labels more accurate

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

#*************************************************#
#From the data set in step 4, creates a second,   #
#independent tidy data set with the average of each# 
#variable for each activity and each subject.     #  
#*************************************************#

#with the dplyr package we create a table that groups the 
#data according to the activity and the subject

Second_tidy <- Tidy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

#This new table is saved in a new txt file format.

write.table(Second_tidy, "./Assignment_GandCD/ResumeData.txt", row.name=FALSE)
