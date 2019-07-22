# FINAL ASSIGNMENT for course Getting and Cleaning Data

# Note: the aim of this script is to tidy some raw datasets and create:
# - finalData, a tidied merged dataset with only data on means and SDs
#               of the measurements that are present in the raw data
# - groupedData, a summarised version of the dataset above by subject and
#               type of activity, where measurements are summarised by mean

### Initialise   
    rm(list = ls())
    
### Install/Load libraries
    # install.packages("dplyr"); install.packages("plyr")
    library(plyr); library(dplyr)
    
### Check work directory
    getwd()


### PART 1    
### Download files with raw data
    fileUrl <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = "Dataset.zip", method = "curl")
    unzip("Dataset.zip")
    dir()
    file.remove("Dataset.zip")
    list.files("./UCI HAR Dataset")
    
### Read in training datasets
    list.files("./UCI HAR Dataset/train")
        
        # 1) Subject IDs
        mydata <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        idData <- tbl_df(mydata)
        head(idData)
        tail(idData)
        dim(idData)     # 7352 obs
        names(idData)
        summary(idData)  # values from 1 to 30 (subject IDs)
        idData <- rename(idData, subjectId = V1)
        table(idData$subjectId) # subject IDs have gaps
                                # (the other subjects are in test dataset)
        length(unique(idData$subjectId))    # 21 different subjects
        idData <- mutate(idData, counter = 1:nrow(idData))  # add counter
                                                            # for merging later
    
        # 2) Training measurements
        mydata <- read.table("./UCI HAR Dataset/train/X_train.txt")
        train <- tbl_df(mydata)
        head(train)
        tail(train)
        dim(train)      # 7352 obs x 561 variables
        names(train)
        train <- mutate(train, counter = 1:nrow(train))     # add counter
                                                            # for merging later
    
        # 3) Training - activity names
        mydata <- read.table("./UCI HAR Dataset/train/y_train.txt")
        trainNames <- tbl_df(mydata)
        head(trainNames)
        tail(trainNames)
        dim(trainNames)      # 7352 obs
        names(trainNames)
        summary(trainNames) # range: 1 to 6
        trainNames <- rename(trainNames, activity = V1)
        trainNames <- mutate(trainNames, counter = 1:nrow(trainNames))
                            # add counter for merging later
        
        # Merge 1), 2) and 3) - subject IDs, training measurements and activities 
        merged <- join_all(list(idData, trainNames, train), by = "counter")
        trainingData <- tbl_df(merged)
        dim(trainingData)   # 7352 obs x 564 vars
        head(trainingData)
        tail(trainingData)
        trainingData <- select(trainingData, -counter)  # delete counter
    
### Read in test datasets
        list.files("./UCI HAR Dataset/test")
        
        # 1) Subject IDs
        mydata <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        idData <- tbl_df(mydata)
        head(idData)
        tail(idData)
        dim(idData)     # 2947 obs
        names(idData)
        summary(idData)  # values from 2 to 24 (subject IDs)
        idData <- rename(idData, subjectId = V1)
        table(idData$subjectId)
        length(unique(idData$subjectId))
        idData <- mutate(idData, counter = 1:nrow(idData))  # add counter
                                                            # for merging later
        
        # 2) Test measurements
        mydata <- read.table("./UCI HAR Dataset/test/X_test.txt")
        test <- tbl_df(mydata)
        rm("mydata")
        head(test)
        tail(test)
        dim(test)      # 2947 obs x 561 variables
        names(test)
        test <- mutate(test, counter = 1:nrow(test))    # add counter
                                                        # for merging later
        
        # 3) Test - activity names
        testNames <- read.table("./UCI HAR Dataset/test/y_test.txt")
        head(testNames)
        tail(testNames)
        dim(testNames)      # 2947 obs
        names(testNames)
        summary(testNames) # range: 1 to 6
        testNames <- rename(testNames, activity = V1)
        testNames <- mutate(testNames, counter = 1:nrow(testNames))
                            # add counter for merging later
    
        # Merge 1), 2) and 3) - subject IDs, test measurements and activities 
        merged <- join_all(list(idData, testNames, test), by = "counter")
        testData <- tbl_df(merged)
        dim(testData)   # 2947 obs x 564 vars
        head(testData)
        tail(testData)
        testData <- select(testData, -counter)  # delete counter
    
        
### Append training and test dataset
    intersect(trainingData$subjectId, testData$subjectId)
        # OK: the two datasets have data on different subjects
    finalData <- full_join(trainingData, testData)
    dim(finalData)  # 10,299 obs x 563 vars
    head(finalData)
    tail(finalData)
    str(finalData)
    sort(unique(finalData$subjectId))   # all subjects represented now (1 to 30)

    
### Extract measurements on the mean and SD for each measurement
    
    # 1) Load names for feature variables
    list.files("./UCI HAR Dataset")
    features <- read.table("./UCI HAR Dataset/features.txt")
    features <- tbl_df(features)
    dim(features)   # labels for 561 feature variables
    head(features)
    tail(features)
    features <- select(features, V2)
    
    # 2) Assign variable names in final dataset
    class(features$V2)
    features$V2 <- as.character(features$V2)
    names(finalData)[3:length(names(finalData))] <- features$V2
    names(finalData)
    
    # 3) Keep only means and standard deviations
    table(grepl("mean\\(\\)", names(finalData)))    # 33 "mean()" functions
    table(grepl("std\\(\\)", names(finalData)))    # 33 "std()" functions
    mean <- grepl("mean\\(\\)", names(finalData))
    sd <- grepl("std\\(\\)", names(finalData))
    mean[1:2] <- TRUE    # to keep subjectID and activity in final dataset
    sd[1:2] <- TRUE      # to keep subjectID and activity in final dataset
    finalData <- finalData[, mean | sd]
    dim(finalData)  # 10299 obs x 68 vars
    names(finalData)
    

### Translate activity numbers into activity names and add to final dataset
    
    # 1) Read in labels
    labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
    labels <- tbl_df(labels)
    head(labels)
    tail(labels)    # 6 activity labels
    labels <- rename(labels, activityName = V2)
    
    # 2) Add labels to final dataset
    finalData <- merge(finalData, labels, by.x = "activity", by.y = "V1",
                       sort = FALSE)
    names(finalData)
    head(select(finalData, subjectId, activity, activityName))
    tail(select(finalData, subjectId, activity, activityName))
    finalData <- select(finalData, -activity)
    finalData <- rename(finalData, activity = activityName)
    class(finalData$activity)   # factor variable
    finalData <- finalData[, c(1, ncol(finalData), 2:(ncol(finalData) - 1))]
                            # rearrange columns
    head(finalData[, 1:2])
    finalData <- tbl_df(finalData)
    dim(finalData)  # 10299 obs x 68 vars
    

### PART 2        
### Create a second dataset (groupedData) with the average of each variable
### for each activity and each subject
    
    # 1) Set groups
    groupedData <- group_by(finalData, subjectId, activity)
    
    # 2) Summarise by groups
    secondData <- summarise_all(groupedData, base::mean)    
        # specified base package for mean function because I would get error
    dim(secondData) # 180 obs x 68 vars
                    # (30 subjects x 6 activities = 180 obs)
    select(head(secondData), 1:5)
    select(tail(secondData), 1:5)
    sum(duplicated(secondData[1:2]))    # OK: 0. To check if there are
                                        # subject-activity combinations
                                        # that are the same