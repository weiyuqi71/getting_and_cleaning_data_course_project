#read data sets
library(dplyr)
file<-download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile = "Dataset.zip",method = "curl")
unzip("Dataset.zip")
features<-read.table("UCI HAR Dataset/features.txt",col.names = c("n","function"))
activity<-read.table("UCI HAR Dataset/activity_labels.txt",col.names = c("code","activity_name"))
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt",col.names = "subject")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",col.names = "subject")
xtrain<-read.table("UCI HAR Dataset/train/X_train.txt",col.names = features$function.)
xtest<-read.table("UCI HAR Dataset/test/X_test.txt",col.names = features$function.)
ytrain<-read.table("UCI HAR Dataset/train/y_train.txt",col.names = "code")
ytest<-read.table("UCI HAR Dataset/test/y_test.txt",col.names = "code")

#Merges the training and the test sets to create one data set
x<-rbind(xtest,xtrain)
y<-rbind(ytrain,ytest)
subject<-rbind(subject_test,subject_train)
merged_data <- cbind(subject, y, x)

#Extracts only the measurements on the mean and standard deviation for each measurement.
TidyData <- merged_data %>% select(subject, code, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set.
TidyData$code<-activity[TidyData$code,2]

#Appropriately labels the data set with descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
finaldata<-TidyData%>%group_by(subject,activity)%>%summarise_all(funs(mean))
write.table(finaldata, "finaldata.txt", row.name=FALSE)
