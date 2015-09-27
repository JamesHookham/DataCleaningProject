###############################################################################################
############################### Preliminary operations ########################################
###############################################################################################

library(plyr)

## Reading in data

directory <- file.path(getwd(),"UCI HAR Dataset")

directory_test <- file.path(directory, "test")
     x_test <- read.table(file.path(directory_test,"X_test.txt"))
     y_test <- read.table(file.path(directory_test,"Y_test.txt"))
     subject_test <- read.table(file.path(directory_test,"subject_test.txt"))

directory_train <- file.path(directory, "train")
     x_train  <-  read.table(file.path(directory_train,"X_train.txt"))
     y_train <- read.table(file.path(directory_train,"Y_train.txt"))
     subject_train <- read.table(file.path(directory_train,"subject_train.txt"))

#Get labels 

activity_labels <- read.table(file.path(directory, "activity_labels.txt"), col.names = c("Id", "Activity"))
feature_labels <- read.table(file.path(directory, "features.txt"), colClasses = c("character"))

#################################################################################################
################################ Begin tidying data here ########################################
#################################################################################################

## 1.Merges the training and the test sets to create one data set.

train_data <- cbind(cbind(x_train, subject_train), y_train)
test_data <- cbind(cbind(x_test, subject_test), y_test)
combined_data <- rbind(train_data, test_data)

sensor_labels <- rbind(rbind(feature_labels, c(562, "Subject")), c(563, "Id"))[,2]
names(combined_data) <- sensor_labels

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_sd  <-  combined_data[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(combined_data))]

## 3. Uses descriptive activity names to name the activities in the data set.

mean_sd  <-  join(mean_sd, activity_labels, by = "Id", match = "first")
mean_sd  <-  mean_sd[,-1]

## 4. Appropriately labels the data set with descriptive names.

names(mean_sd)  <-  gsub("([()])","",names(mean_sd))
names(mean_sd)  <-  make.names(names(mean_sd))

## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject 

result <- ddply(mean_sd, c("Subject","Activity"), numcolwise(mean))
resultheaders <- names(result)
addSuffix <-  function(x, suffix) {
     if (!(x %in% c("Subject","Activity"))) {
          paste(x,suffix, sep="")
     }
     else{
          x
     }
}
resultheaders <- sapply(resultheaders, addSuffix, ".mean")
names(result) <- resultheaders

write.table(result, file = "tidy_code.txt", row.name=FALSE)