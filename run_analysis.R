####################################################################################

## Coursera 
## Data Science Specialization
## Course 3: Getting and Cleaning Data 
## week 4: Project

####################################################################################

# Purpose: collect, work with, clean the data set
# Goal: prepare tidy data that can be used for later analysis

# Review criteria 
# 1.The submitted data set is tidy.
# 2.The Github repo contains the required scripts.
# 3.GitHub contains a code book that modifies and updates the available 
#   codebooks with the data to indicate all the variables and summaries calculated, 
#   along with units, and any other relevant information.
# 4.The README that explains the analysis files is clear and understandable.
# 5.The work submitted for this project is the work of the student who submitted it.

# Steps:
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation 
#    for each measurement.
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive variable names.
# 5. From the data set in step 4, create a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

####################################################################################

# Step 0
# 0.1 Working directory + data folder
# 0.2 libraries
# 0.3.import data


#0.1 set wd:
setwd("~/R/R data/DSC/course3/week4")


#0.2 import library:
library(dplyr)
library(reshape2)
library(data.table)

#0.3 import data:

path<- getwd()
if(!file.exists("./data")){dir.create("./data")}
path <- file.path(path , "data")

dataurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataurl, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")


#########################
# Step 1
# Merge the training and the test sets to create one data set.
# 1.1 load train data set and features
# 1.2 load test data set and features
# 1.3 merge data sets


path <- file.path(path, "UCI HAR Dataset")

# 1.1 load train data set and features
ActivityTest  <- read.table(file.path(path, "test" , "Y_test.txt" ),header = FALSE)
SubjectTest  <- read.table(file.path(path, "test" , "subject_test.txt"),header = FALSE)
FeaturesTest  <- read.table(file.path(path, "test" , "X_test.txt" ),header = FALSE)

# 1.2 load test data set and features
SubjectTrain <- read.table(file.path(path, "train", "subject_train.txt"),header = FALSE)
ActivityTrain <- read.table(file.path(path, "train", "Y_train.txt"),header = FALSE)
FeaturesTrain <- read.table(file.path(path, "train", "X_train.txt"),header = FALSE)

# 1.3 merge data sets

# Concatenation of the data tables by rows
dataSubject <- rbind(SubjectTrain, SubjectTest)
dataActivity<- rbind(ActivityTrain, ActivityTest)
dataFeatures<- rbind(FeaturesTrain, FeaturesTest)

# Seting the names to variables
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

# merging into columns
dataCombined <- cbind(dataSubject, dataActivity)
DataTable <- cbind(dataFeatures, dataCombined)


#########################
# Step 2
# Extract only the measurements on the mean and standard deviation 
# for each measurement.

# Extracting 
subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

# Subsetting the data frame 
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
DataTable<-subset(DataTable,select=selectedNames)


#########################
# Step 3
# Use descriptive activity names to name the activities in the data set

activityLabels <- read.table(file.path(path, "activity_labels.txt"),header = FALSE)
DataTable$activity<-factor(DataTable$activity,labels=activityLabels[,2])



#########################
# Step 4
# Appropriately label the data set with descriptive variable names.

names(DataTable)<-gsub("^t", "time", names(DataTable))
names(DataTable)<-gsub("^f", "frequency", names(DataTable))
names(DataTable)<-gsub("Acc", "Accelerometer", names(DataTable))
names(DataTable)<-gsub("Gyro", "Gyroscope", names(DataTable))
names(DataTable)<-gsub("Mag", "Magnitude", names(DataTable))
names(DataTable)<-gsub("BodyBody", "Body", names(DataTable))



#########################
# Step 5
# From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

TidyDataSet <- aggregate(. ~subject + activity, DataTable, mean)
TidyDataSet <- TidyDataSet[order(TidyDataSet$subject, TidyDataSet$activity),]
write.table(TidyDataSet, "TidyDataSet.txt", row.name=FALSE,quote = FALSE, sep = '\t')



