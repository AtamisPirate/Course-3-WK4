library(tidyverse)
# Set up the lables for each column
datapath <- getwd()
activity_Labels <- fread(file.path(datapath, "UCI HAR Dataset/activity_labels.txt")
                         ,col.names = c("dataclass_Labels", "activity_labels"))
features <- fread(file.path(datapath, "UCI HAR Dataset/features.txt"),
                  col.names = c("feature#","feature_Name"))

# This loads in training and test datasets and formats them with feature named columns
##Creates a table of features to be used as column names of the Xtrain/test data
transposefeatures <- transpose(features[,2])
featurecolnames <-unlist(transposefeatures, recursive = FALSE)
##Creates data for train and test vaules with their properly named feature columns
trainAll <- read.table("UCI HAR Dataset/train/X_train.txt",col.names = featurecolnames)
testAll <- read.table("UCI HAR Dataset/test/X_test.txt",col.names = featurecolnames)

## takes only columns which contain mean or std
trainmeancolumns <- trainAll %>% select(contains("mean"))
trainstdcolumns <- trainAll %>% select(contains("std"))
train <- cbind(trainmeancolumns,trainstdcolumns)

testmeancolumns <- testAll %>% select(contains("mean"))
teststdcolumns <- testAll %>% select(contains("std"))
test <- cbind(testmeancolumns,teststdcolumns)

##Creates data for subject numbers to be added to test and train data
trainsubjects <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subjectnumber")
testsubjects <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subjectnumber")
##combines subject data to measurment data
trainWsubject <- cbind(trainsubjects,train)
testWsubject <- cbind(testsubjects,test)
##reads in activity data
trainActivity <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "activity")
testActivity <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "activity")


##joins activity data with test data

prefinaltestdata <- cbind(testActivity,testWsubject)
prefinaltraindata <- cbind(trainActivity,trainWsubject)

##final combination of data
finaltestdata <- left_join(activity_Labels, prefinaltestdata, by= c("dataclass_Labels" = "activity"))
finaltraindata <- left_join(activity_Labels, prefinaltraindata, by= c("dataclass_Labels" = "activity"))

combineddata <- rbind(finaltestdata,finaltraindata)
##final tidy data set
TidydataSet <- combineddata %>% group_by(subjectnumber,activity_labels) %>% summarise_all(funs(mean))
write.table(TidydataSet, "TidyDataSet.txt", row.names = FALSE)
