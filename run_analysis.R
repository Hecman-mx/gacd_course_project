## Installing and Loading Packages
## script created by Hecman-mx

install.packages("data.table")
library(data.table)
install.packages("reshape2")
library(reshape2)

## getting the working directory
wd <- getwd()

## creating and setting a working directory that work everywere
dir.create("gacd_course_project")
wd2 <- paste0(wd,"/gacd_course_project", collapse = NULL)
setwd(wd2)

## create a temporary directory for the zip file
td <- tempdir()
## create the destination directory for the data
dir.create(wd2,"data")
dd <- paste0(wd2,"/data", collapse = NULL)

## getting the file url
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fn <- "Dataset.zip"

## create the placeholder file
tf <- tempfile(tmpdir=td, fileext=".zip")

# download into the placeholder file
download.file(url, tf)

# get the names of the files in the ".zip" archive
fname <- unzip(tf, list=TRUE)$Name
# unzip the files to the temporary directory
unzip(tf, files=fname, exdir=dd, overwrite=TRUE)

## getting the name of the rawdata folder
fd <- paste0(dd,"/",list.dirs(dd, full.names = FALSE, recursive = FALSE), collapse= NULL)


#### Merge the training and the test sets to create one data set ####

## read in the train data from files
features     <- read.table(file.path(fd, "features.txt"),header=FALSE)
activityType <- read.table(file.path(fd, "activity_labels.txt"),header=FALSE)
subjectTrain <- read.table(file.path(fd, "train", "subject_train.txt"),header=FALSE)
xTrain       <- read.table(file.path(fd, "train", "x_train.txt"),header=FALSE)
yTrain       <- read.table(file.path(fd, "train", "y_train.txt"),header=FALSE)
## read in the test data from files
subjectTest <- read.table(file.path(fd, "test", "subject_test.txt"),header=FALSE)
xTest       <- read.table(file.path(fd, "test", "x_test.txt"),header=FALSE)
yTest       <- read.table(file.path(fd, "test", "y_test.txt"),header=FALSE)

## Assigin column names to the data imported above
colnames(activityType)  <- c('activityId','activityType')
colnames(subjectTrain)  <- "subjectId"
colnames(xTrain)        <- features[,2]
colnames(yTrain)        <- "activityId"

## Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData <- cbind(yTrain,subjectTrain,xTrain)

## Assign column names to the test data imported above
colnames(subjectTest) <- "subjectId"
colnames(xTest)       <- features[,2] 
colnames(yTest)       <- "activityId"


## Create the final test set by merging the xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest)


## Combine training and test data to create a final data set
finalData <- rbind(trainingData,testData)

## Create a vector for the column names from the finalData, which will be used
colNames  <- colnames(finalData)


#### Extract only the measurements on the mean and standard deviation for each measurement ####

## Create a vector to get the TRUE vals for the ID, mean() & stddev()
meanstd <- (grepl("activityId|subjectId|mean\\(\\)|std\\(\\)", colNames))

## filter the dires columns only
finalData <- finalData[meanstd==TRUE]


####  Uses descriptive activity names to name the activities in the data set ####

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData <- merge(finalData,activityType,by="activityId", all = TRUE)

# Updating the colNames vector to include the new column names after merge
colNames <- colnames(finalData)


#### Appropriately label the data set with descriptive activity names ####

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- colNames



#### Create a second, independent tidy data set with the average of each variable for each activity and each subject #### 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  <- finalData[,names(finalData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    <- merge(tidyData,activityType,by="activityId",all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',col.names=TRUE,row.names=FALSE,sep=',')