# Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This R script does the following:
directory="C:/Users/Aura/Desktop/Coursera/3_Getting and Cleaning Data/Course Project/UCI HAR Dataset"
setwd(directory)

# 1. Merges the training and the test sets to create one data set.
train1= read.table("train/X_train.txt")
test1=read.table("test/X_test.txt")
X=rbind(train1, test1)
train2=read.table("train/subject_train.txt")
test2=read.table("test/subject_test.txt")
S=rbind(train2, test2)
train3=read.table("train/y_train.txt")
test3=read.table("test/y_test.txt")
Y=rbind(train3, test3)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features=read.table("features.txt")
indices_of_good_features=grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X=X[, indices_of_good_features]
names(X)=features[indices_of_good_features, 2]
names(X)=gsub("\\(|\\)", "", names(X))
names(X)=tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set.
activities=read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y)="activity"

# 4. Appropriately labels the data set with descriptive activity names.
names(S)="subject"
cleaned=cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}

write.table(result, "data_set_with_the_averages.txt", row.name=FALSE)


