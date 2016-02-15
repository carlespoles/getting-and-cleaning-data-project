setwd("C:\\Users\\Carles\\Documents\\R_data_sets")

library(httr)
# Below library is used to unzip later the file to be downloaded.
library(downloader)
# plyr library will be required for later file/data processing.
library(plyr)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileZIP <- "dataSetUCI.zip"
download.file(fileURL, destfile = paste(".\\", fileZIP, sep = ""), method = "curl")

# Make sure this folder exists.
#fileDestination <- "UCI_HAR_Files"

# We are NOT using the above fileDestination variable because the zip file contains the files inside
# a folder "UCI HAR Dataset", so unzipping under fileDestination would create an extra folder.

#unzip(fileZIP, list = FALSE, overwrite = TRUE, exdir = paste(".\\", fileDestination, sep = ""))

# Files are unzipped in this folder => "UCI HAR Dataset".

unzip(fileZIP, list = FALSE, overwrite = TRUE, exdir = getwd())

# Helper function to convert input text files into data frames.
loadInputFiles <- function (filename, cols = NULL){
    
    textFile <- paste("UCI HAR Dataset", filename, sep = "/")
    
    dataToRead <- data.frame()
    
    if(is.null(cols)){
        
        dataToRead <- read.table(textFile, sep = "", stringsAsFactors = F)
        
    } else {
        
        dataToRead <- read.table(textFile, sep = "", stringsAsFactors = F, col.names = cols)
    }
    
    return (dataToRead)
}

# Getting contents of features.txt using helper function loadInputFiles().
readFeaturesFile <- loadInputFiles("features.txt")

# Helper function to get the data from input text files. 
# The function will be used for both testing and training data sets.
getInputData <- function(dataSetType, readFeaturesFile){
   
    # Reading subject_train.txt and subject_test.txt.
    subjectData <- loadInputFiles(paste(dataSetType, "/", "subject_", dataSetType, ".txt", sep = ""), "id")
    # Reading y_train.txt and y_test.txt.
    yData <- loadInputFiles(paste(dataSetType, "/", "y_", dataSetType, ".txt", sep = ""), "activity")
    # Reading x_train.txt and x_test.txt. IMPORTANT => X is capital!!
    xData <- loadInputFiles(paste(dataSetType, "/", "X_", dataSetType, ".txt", sep = ""), readFeaturesFile$V2)
    
    # All datasets are combined using cbind.
    return (cbind(subjectData, yData, xData))
}


# Helper function to output the results required by the project requirements.
# Make sure that folder 'output' exists in the working directory.
createOutput <- function (dataToOutput, fileName){
    
    fileSaved <- paste("output", "/", fileName, ".csv", sep = "")
    
    write.csv(dataToOutput, fileSaved)
    
    # Creating also a text file output.
    write.table(dataToOutput, paste("output", "/", fileName, ".txt", sep = ""), sep="\t", row.names = FALSE)
}

# Now we will make use of the helper functions to create the desired output per project requirements.

# First, load the appropiate data after been downloaded.
myTestingData <- getInputData("test", readFeaturesFile)
myTrainingData <- getInputData("train", readFeaturesFile)

# Second we need to merge training and testing data.
mergedData <- rbind(myTrainingData, myTestingData)
mergedData <- arrange(mergedData, id)

# Third, for each measurement we will only extract measurements on the mean and standard deviation. 
meanAndStdDevData <- mergedData[,c(1,2,grep("std", colnames(mergedData)), grep("mean", colnames(mergedData)))]
# We save the information in a .csv file.
createOutput(meanAndStdDevData,"mean_and_std")

# Forth, we need to use descriptive activity names to name the activities in the data set.
activityLabels <- loadInputFiles("activity_labels.txt")

# Fifth, we appropriately labels the data set with descriptive variable names. 
mergedData$activity <- factor(mergedData$activity, levels=activityLabels$V1, labels=activityLabels$V2)

# Finally, from previous step, we create a second, independent tidy data set with the average of each variable 
# for each activity and each subject. 
cleanDataset <- ddply(meanAndStdDevData, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

colnames(cleanDataset)[-c(1:2)] <- paste(colnames(cleanDataset)[-c(1:2)], "_mean", sep = "")

createOutput(cleanDataset,"tidy_dataset")
