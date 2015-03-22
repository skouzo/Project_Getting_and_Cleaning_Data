# File: run_analysis.R
# by Skouzo
# Directory setting command for Project
# I assume current directory is: ..../UCI HAR Dataset
# setwd("./Getting and Cleaning Data/Project/UCI HAR Dataset")

#Import features_info.txt from the current directory: (UCI HAR Dataset)
features = read.table('./features.txt',header=FALSE)
head(features);
#Import activity_labels.txt from the current directory: (UCI HAR Dataset)
activityType = read.table('./activity_labels.txt',header=FALSE)
head(activityType);

#Import activity_labels.txt from the current directory: (UCI HAR Dataset)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)

#Import x_train.txt
xTrain = read.table('./train/x_train.txt',header=FALSE)

#import y_train.txt
yTrain = read.table('./train/y_train.txt',header=FALSE)

#=============================================================

# Lets assign names to columns of the data sets
colnames(activityType) = c('activityId','activityType')
colnames(subjectTrain) = "subjectId"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityId"

## Now merge yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)


# ----------- Do the same with Test data -------------
## Import subject_test.txt
subjectTest = read.table('./test/subject_test.txt',header=FALSE)

#importing x_test.txt
xTest = read.table('./test/x_test.txt',header=FALSE)

#importing x_test.txt
yTest = read.table('./test/y_test.txt',header=FALSE)

# Assign column names to the test data sets
colnames(subjectTest) = "subjectId"
colnames(xTest) = features[,2]
colnames(yTest) = "activityId"

# Merge subjectTest sets in one data set
testData = cbind(yTest,subjectTest,xTest)

######################################### STEP 1 ###################################################
#  Merge the training and the test sets to create one data set
####################################################################################################

# Now combine (actually concatenate) the training data set and test data set
training_Test = rbind(trainingData,testData)
# Label columns
colNames = colnames(training_Test)


##################################### STEP 2 #########################################################
# Extract only the measurements on the mean and standard deviation for each measurement.
####################################################################################################

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) &
                   !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) &
                   !grepl("-std()..-",colNames))

# Subset training_Test table based on the logicalVector
training_Test = training_Test[logicalVector==TRUE]


#################################### STEP 3 ##########################################################
#
# Use descriptive activity names to name the activities in the data set
#
######################################################################################################

# Combine training_Test data set with activityType table to include descriptive activity names
training_Test = merge(training_Test,activityType,by='activityId',all.x=TRUE)

# Lets rename the colNames vector to include the new column names after merge
colNames = colnames(training_Test)


######################################## STEP 4 #################################################
#
#  Appropriately label the data set with descriptive activity names.
#
#################################################################################################
# Although names look OK we perform some cleaning up to make them nicer
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
# Rename the descriptive column names to the training_Test data set
colnames(training_Test) = colNames



####################################### STEP 5 ##########################################################
#
# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject.
#
##############################################################################################


# Create a new table, training_Test without the activityType column
training_Test_NoActivityType = training_Test[,names(training_Test) != 'activityType']


## Final Data Set
tidyData = aggregate(training_Test_NoActivityType[,names(training_Test_NoActivityType) != c('activityId','subjectId')],
                     by=list(activityId=training_Test_NoActivityType$activityId,subjectId = training_Test_NoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE)


### Export the tidyData set

write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t')

