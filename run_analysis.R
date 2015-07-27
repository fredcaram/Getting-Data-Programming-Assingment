require(plyr)
require(reshape)

##Load one dataset type, ex: dataSetPath <- "./UCI HAR Dataset", dataSetType <- "test"
LoadDataSet <- function (dataSetPath, dataSetType)
{
  ##Define the filenames
  xFileName <- paste(paste("X", dataSetType, sep = "_"), "txt", sep = ".")
  yFileName <- paste(paste("y", dataSetType, sep = "_"), "txt", sep = ".")
  subjectFileName <- paste(paste("subject", dataSetType, sep = "_"), "txt", sep = ".")
  
  ##Load the datasets
  x_set <- read.table(paste(dataSetPath, dataSetType, xFileName, sep =  "/"), header = F, sep = "")
  y_set <- read.table(paste(dataSetPath, dataSetType, yFileName, sep =  "/"), header = F, sep = "")
  subject_set <- read.table(paste(dataSetPath, dataSetType, subjectFileName, sep = "/"), header = F, sep = "")
  
  ##Bind the x_set to the subject and the activity
  transformationSet <- cbind2(x_set, subject_set)
  transformationSet <- cbind2(transformationSet, y_set)
  
  transformationSet
}

Run_Analisys <- function(dataSetPath)
{
  ##Load the datasets
  feature_set <- read.table(paste(dataSetPath, "features.txt", sep =  "/"), header = F, sep = "")
  test_set <- LoadDataSet("./UCI HAR Dataset", "test")
  train_set <- LoadDataSet("./UCI HAR Dataset", "train")
  
  transformationSet <- rbind(test_set, train_set)
  
  ##Put the column names
  names(transformationSet) <- feature_set[,2]
  names(transformationSet)[[562]] <- "subject"
  names(transformationSet)[[563]] <- "activity_id"
  
  transformationSet <- melt(transformationSet, id=c("subject", "activity_id"))
  meanDataSet <- cast(transformationSet, subject~activity_id, mean)
  sdDataSet <- cast(transformationSet, subject~activity_id, sd)
  
  completeSet <- cbind(meanDataSet, sdDataSet)
}

