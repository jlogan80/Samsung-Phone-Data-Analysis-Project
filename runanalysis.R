  # Please note: this function assumes that you have the plyr and dplyr libraries downloaded from R
  library(plyr)
  library(dplyr)
  # read in files from working directory (assuming unzipped folder)
  xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
  ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
  ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  features <- read.table("./UCI HAR Dataset/features.txt")
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
  
  #1. Merge train and test datasets
  
  featuresnames <- features[,2]
  colnames(xtrain) <- featuresnames
  colnames(xtest) <- featuresnames
  
  full_train <- cbind(subject_train, ytrain, xtrain)
  names(full_train)[1] <- "subject" #as there are three V1
  names(full_train)[2] <- "activity" #as there are three V1
  
  full_test <- cbind(subject_test, ytest, xtest)
  names(full_test)[1] <- "subject" #as there are three V1
  names(full_test)[2] <- "activity" #as there are three V1
  
  full_data <- rbind(full_train,full_test)
  
  #2. EXtract only the means and standard deviations of the measurements
  
  #Correct error that happens because of bandEnergy coordinates
  full_data <- full_data[!duplicated(names(full_data))]
  
  full_data_mean <- select(full_data, contains("mean"))
  full_data_std <- select(full_data, contains("std"))
  mydata <- cbind(full_data$subject, full_data$activity, full_data_mean, full_data_std)
  
  #3. Uses descriptive activity names to label the activities in the data set
  
  colnames(mydata)[1] <- "subject"
  colnames(mydata)[2] <- "activity"
  
  colnames(activity_labels)[1] <- "activity"
  mydata <- left_join(mydata, activity_labels)
  colnames(mydata)[89] <- "activity_label"
  
  #4. Appropriately labels the data set with descriptive variable names 
  colnames(mydata) <- c("subject", "activity", "timeBodyAccelerometerMeanX", "timeBodyAccelerometerMeanY", "timeBodyAccelerometerMeanZ", "timeGravityAccelerometerMeanX",	"timeGravityAccelerometerMeanY", "timeGravityAccelerometerMeanZ", "timeBodyAccelerometerJerkMeanX", "timeBodyAccelerometerJerkMeanY", "timeBodyAccelerometerJerkMeanZ", "timeBodyGyroscopeMeanX", "timeBodyGyroscopeMeanY", "timeBodyGyroscopeMeanZ", "timeBodyGyroscopeJerkMeanX", "timeBodyGyroscopeJerkMeanY", "timeBodyGyroscopeJerkMeanZ", "timeBodyAccelerometerMagnitudeMean", "timeGravityAccelerometerMagnitudeMean", "timeBodyAccelerometerJerkMagnitudeMean", "timeBodyGyroscopeMagnitudeMean", "timeBodyGyroscopeJerkMagnitudeMean", "frequencyBodyAccelerometerMeanX", "frequencyBodyAccelerometerMeanY", "frequencyBodyAccelerometerMeanZ", "frequencyBodyAccelerometerMeanFrequencyX", "frequencyBodyAccelerometerMeanFrequencyY", "frequencyBodyAccelerometerMeanFrequencyZ", "frequencyBodyAccelerometerJerkMeanX", "frequencyBodyAccelerometerJerkMeanY", "frequencyBodyAccelerometerJerkMeanZ", "frequencyBodyAccelerometerJerkMeanFrequencyX", "frequencyBodyAccelerometerJerkMeanFrequencyY", "frequencyBodyAccelerometerJerkMeanFrequencyZ", "frequencyBodyGyroscopeMeanX", "frequencyBodyGyroscopeMeanY", "frequencyBodyGyroscopeMeanZ", "frequencyBodyGyroscopeMeanFrequencyX", "frequencyBodyGyroscopeMeanFrequencyY", "frequencyBodyGyroscopeMeanFrequencyZ", "frequencyBodyAccelerometerMagnitudeMean", "frequencyBodyAccelerometerMagnitudeMeanFrequency", "frequencyBodyAccelerometerJerkMagnitudeMean", "frequencyBodyAccelerometerJerkMagnitudeMeanFrequency", "frequencyBodyGyroscopeMagnitudeMean", "frequencyBodyGyroscopeMagnitudeMeanFrequency", "frequencyBodyGyroscopeJerkMagnitudeMean", "frequencyBodyGyroscopeJerkMagnitudeMeanFrequency", "angle(timeBodyAccelerometerMean,gravity)", "angle(timeBodyAccelerometerJerkMean,gravityMean)", "angle(timeBodyGyroscopeMean,gravity)", "angle(timeBodyGyroscopeJerkMean,gravityMean)", "angle(X,gravityMean)", "angle(Y,gravityMean)", "angle(Z,gravityMean)", "timeBodyAccelerometerStdDevX", "timeBodyAccelerometerStdDevY", "timeBodyAccelerometerStdDevZ", "timeGravityAccelerometerStdDevX", "timeGravityAccelerometerStdDevY", "timeGravityAccelerometerStdDevZ", "timeBodyAccelerometerJerkStdDevX", "timeBodyAccelerometerJerkStdDevY", "timeBodyAccelerometerJerkStdDevZ", "timeBodyGyroscopeStdDevX", "timeBodyGyroscopeStdDevY", "timeBodyGyroscopeStdDevZ", "timeBodyGyroscopeJerkStdDevX", "timeBodyGyroscopeJerkStdDevY", "timeBodyGyroscopeJerkStdDevZ", "timeBodyAccelerometerMagnitudeStdDev", "timeGravityAccelerometerMagnitudeStdDev", "timeBodyAccelerometerJerkMagnitudeStdDev", "timeBodyGyroscopeMagnitudeStdDev", "timeBodyGyroscopeJerkMagnitudeStdDev", "frequencyBodyAccelerometerStdDevX", "frequencyBodyAccelerometerStdDevY", "frequencyBodyAccelerometerStdDevZ", "frequencyBodyAccelerometerJerkStdDevX", "frequencyBodyAccelerometerJerkStdDevY", "frequencyBodyAccelerometerJerkStdDevZ", "frequencyBodyGyroscopeStdDevX", "frequencyBodyGyroscopeStdDevY", "frequencyBodyGyroscopeStdDevZ", "frequencyBodyAccelerometerMagnitudeStdDev", "frequencyBodyAccelerometerJerkMagnitudeStdDev", "frequencyBodyGyroscopeMagnitudeStdDev", "frequencyBodyGyroscopeJerkMagnitudeStdDev", "activity_label")
  
  #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  table_averages <- ddply(mydata, .(subject,activity_label), function(x) colMeans(x[,3:88]))
  write.table(table_averages, "averages_output.txt", row.names = FALSE)
  
  
