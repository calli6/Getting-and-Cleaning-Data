## Getting and Cleaning Data ##
##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement. 
##3.Uses descriptive activity names to name the activities in the data set
##4.Appropriately labels the data set with descriptive variable names. 
##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Set Working Directory
getwd()
setwd("C:/Users/Calli6/Desktop/Getting and Cleaning Data")

# Libraries used
library(dplyr)
library(reshape2)

# Download and unzip dataset in working directory
dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataset_url, "data.zip")
unzip("data.zip", exdir = "data")

# List all files in working directory
list.files("C:/Users/Calli6/Desktop/Getting and Cleaning Data")


## 1.Merges the training and the test sets to create one data set.

## Read in training and test datasets for X
X_train <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/train/X_train.txt"
                      , quote="\"", comment.char="", stringsAsFactors=FALSE)
X_test <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/test/X_test.txt"
                     , quote="\"", comment.char="", stringsAsFactors=FALSE)

# Combine training and test datasets
x_total <- rbind(X_train, X_test)

# Read in features 
features <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/features.txt"
                       , quote="\"", comment.char="", stringsAsFactors=FALSE)
# Put features as variable names in x_total
names(x_total) <- features[,2]



## Read in training and test datasets for y (6 measurements)
y_train <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/train/y_train.txt"
                      , quote="\"", comment.char="", stringsAsFactors=FALSE)
y_test <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/test/y_test.txt"
                     , quote="\"", comment.char="", stringsAsFactors=FALSE)

# Combine training and test datasets
y_total <- rbind(y_train, y_test)
#summary(y_total)

# Rename V1 to measurement_id
y_total <- rename(y_total, measurementid = V1)


## Read in subject ID for test and training data
subject_test <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/test/subject_test.txt"
                           , quote="\"", comment.char=""
                           , stringsAsFactors=FALSE)
subject_train <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/train/subject_train.txt"
                            , quote="\"", comment.char="", stringsAsFactors=FALSE)

# Combine subject ID for test and training data
sub_total <- rbind(subject_train, subject_test)
summary(sub_total)

# Rename V1 to subject_id
subject_total <- rename(sub_total, subjectid = V1)

# Add subject ID (sub_total) and measurement (y_total) as columns in x_total
data <- cbind(subject_total, y_total, x_total) 



## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

# Get a vector of columns with mean or standard deviation 
mean_std_vars <- (grep("[Mm]ean|[Ss]tandard [Dd]eviation|[Ss]td", names(data)))

# Get only columns from data that have a mean or std
data_mean_std <- data[ ,mean_std_vars]

data2 <- cbind(subject_total, y_total, data_mean_std)



## 3.Uses descriptive activity names to name the activities in the data set

# Read in activity_labels
activity_labels <- read.table("C:/Users/Calli6/Desktop/Getting and Cleaning Data/data/UCI HAR Dataset/activity_labels.txt"
                              , quote="\"", comment.char="", stringsAsFactors=FALSE)

# Lower case the values in activitydescription and remove underscores
activity_labels_rn <-sapply(2, function(x,y) tolower(y[,x]), y = activity_labels)
activity_labels_rm <- as.data.frame(sub("_"," ", activity_labels_rn))
#is.array(activity_labels_rm)

# Join activity_labels_rm to activity_labels
activity_labels_3 <- cbind(activity_labels, activity_labels_rm)
activity_labels_edit <- activity_labels_3[,c(1,3)]
activity_labels_edit

# Rename V2 in activity_labels to activitydescription and Rename V1 to measurementid
activity_labels <- rename(activity_labels_edit, activitydescription = V1.1, measurementid = V1)
activity_labels

# Join 
data_activity <- inner_join(activity_labels, data2, by = "measurementid")
data_activity <- data_activity[,-1]



## 4.Appropriately labels the data set with descriptive variable names. 
names(data_activity)

# Turn all variable names to lower case letters
data_lower<- tolower(names(data_activity))
data_lower
# Remove parenthesis, dashes, and commas from variable names
data_rm <- (gsub("\\(|\\)|-|_|,", "", data_lower))
data_rm
as.vector(data_rm)

# Put data_rm as variable  names for data_activity
names(data_activity) <- data_rm


## 5.From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.


data_melt <- melt(data_activity, id = c("activitydescription","subjectid"))
head(data_melt,200)

decast_data <- dcast(data_melt, activitydescription + subjectid ~ variable, mean)

write.table(decast_data, file = "tidy_data.txt", row.name=FALSE)