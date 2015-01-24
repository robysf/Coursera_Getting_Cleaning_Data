### GETTING AND CLEANING DATA ###
## ASSIGNMENT ##

# note that I'm a complete novice at coding, so this isn't the most elegant
# I store a lot of intermediate datasets, rather than chaining them for now b/c it's more intuitive for me
# and twice I used brute-force, non-scalable steps
# thanks for reading!

# to be implemented in the working directory unzipped from the link in the class assignment, unmodified
# with subdirectoriess called "test" and "train"

# read each dataset into R:
subject_test <- read.table("test/subject_test.txt") 
subject_train <- read.table("train/subject_train.txt") 
X_test <- read.table("test/X_test.txt") 
X_train <- read.table("train/X_train.txt") 
y_test <- read.table("test/y_test.txt") 
y_train <- read.table("train/y_train.txt") 
activity_labels <- read.table("activity_labels.txt") 
features <- read.table("features.txt") 

# rename variables in the above datasets (part of task 4)
names(subject_test) <- c("subject")
names(y_test) <- c("activity_number")
names(subject_train) <- c("subject")
names(y_train) <- c("activity_number")
var_names <- make.names(features[,2], unique = TRUE) # eliminates duplicates and corrects illegal names with "()"
names(X_test) <- var_names
names(X_train) <- var_names

# append a column for each activity and each subject, to both test and train datasets
test_table1 <- cbind(subject_test, y_test, X_test)
train_table1 <- cbind(subject_train, y_train, X_train)

# fuse test and train tables (task 1)
unified_table1 <- rbind(test_table1, train_table1)

# trim dataset to include only columns that are means or std devs (task 2)
install.packages("dplyr")
library(dplyr)
unified_tbldf1 <- tbl_df(unified_table1)
pared_columns1 <- select(unified_tbldf1, 
                         contains("subject"), 
                         contains("activity_number"), 
                         contains("mean"), 
                         contains("std"))

# replace activity numbers (from y_test and y_train) with activity names (from activity_labels) (task 3)
# I realize this is a clunky/non-scaleable way to reassign values, but I had trouble finding a better way
pared_columns1$activity_name[pared_columns1$activity_number==1] = "walking"
pared_columns1$activity_name[pared_columns1$activity_number==2] = "walking_upstairs"
pared_columns1$activity_name[pared_columns1$activity_number==3] = "walking_downstairs"
pared_columns1$activity_name[pared_columns1$activity_number==4] = "sitting"
pared_columns1$activity_name[pared_columns1$activity_number==5] = "standing"
pared_columns1$activity_name[pared_columns1$activity_number==6] = "laying"

# remove activity_number and move activity_name to the front, for simplicity
pared_columns2 <- select(pared_columns1, 
                         contains("subject"), 
                         contains("activity_name"), 
                         contains("mean"), 
                         contains("std"))

# create independent dataset grouped by subject and activity (task 5)
# I used the dplyr package for this - group_by() and summarize()
# but I couldn't figure out a way to select a range of columns using summarize(), and I didn't have time to learn another package
# so this is extremely inelegant/non-scalable/brute-force, but it seems to work

grouped_data1 <- group_by(pared_columns2, subject, activity_name)

summary_table1 <- summarize(grouped_data1, 
                            mean(tBodyAcc.mean...X), 
                            mean(tBodyAcc.mean...Y), 
                            mean(tBodyAcc.mean...Z), 
                            mean(tGravityAcc.mean...X), 
                            mean(tGravityAcc.mean...Y), 
                            mean(tGravityAcc.mean...Z), 
                            mean(tBodyAccJerk.mean...X), 
                            mean(tBodyAccJerk.mean...Y),
                            mean(tBodyAccJerk.mean...Z),
                            mean(tBodyGyro.mean...X), 
                            mean(tBodyGyro.mean...Y),
                            mean(tBodyGyro.mean...Z),
                            mean(tBodyGyroJerk.mean...X),
                            mean(tBodyGyroJerk.mean...Y),
                            mean(tBodyGyroJerk.mean...Z),
                            mean(tBodyAccMag.mean..),
                            mean(tGravityAccMag.mean..),
                            mean(tBodyAccJerkMag.mean..),
                            mean(fBodyAcc.mean...X),
                            mean(fBodyAcc.mean...Y),
                            mean(fBodyAcc.mean...Z),
                            mean(fBodyAcc.meanFreq...X),
                            mean(fBodyAcc.meanFreq...Y),
                            mean(fBodyAcc.meanFreq...Z),
                            mean(fBodyAccJerk.mean...X),
                            mean(fBodyAccJerk.mean...Y),
                            mean(fBodyAccJerk.mean...Z),
                            mean(fBodyAccJerk.meanFreq...X),
                            mean(fBodyAccJerk.meanFreq...Y),
                            mean(fBodyAccJerk.meanFreq...Z),
                            mean(fBodyGyro.mean...X),
                            mean(fBodyGyro.mean...Y),
                            mean(fBodyGyro.mean...Z),
                            mean(fBodyGyro.meanFreq...X),
                            mean(fBodyGyro.meanFreq...Y),
                            mean(fBodyGyro.meanFreq...Z),
                            mean(fBodyAccMag.mean..),
                            mean(fBodyAccMag.meanFreq..),
                            mean(fBodyBodyAccJerkMag.mean..),
                            mean(fBodyBodyAccJerkMag.meanFreq..),
                            mean(angle.tBodyAccMean.gravity.),
                            mean(angle.tBodyAccJerkMean..gravityMean.),
                            mean(angle.tBodyGyroMean.gravityMean.),
                            mean(angle.tBodyGyroJerkMean.gravityMean.),
                            mean(angle.X.gravityMean.),
                            mean(angle.Y.gravityMean.), 
                            mean(angle.Z.gravityMean.), 
                            mean(tBodyAcc.std...X), 
                            mean(tBodyAcc.std...Y), 
                            mean(tBodyAcc.std...Z),
                            mean(tGravityAcc.std...X),
                            mean(tGravityAcc.std...Y),
                            mean(tGravityAcc.std...Z),
                            mean(tBodyAccJerk.std...X), 
                            mean(tBodyAccJerk.std...Y),
                            mean(tBodyAccJerk.std...Z),
                            mean(tBodyGyro.std...X),
                            mean(tBodyGyro.std...Y),
                            mean(tBodyGyro.std...Z),
                            mean(tBodyGyroJerk.std...X),
                            mean(tBodyGyroJerk.std...Y),
                            mean(tBodyGyroJerk.std...Z),
                            mean(tBodyAccMag.std..),
                            mean(tGravityAccMag.std..),
                            mean(tBodyAccJerkMag.std..),
                            mean(tBodyGyroMag.std..),
                            mean(tBodyGyroJerkMag.std..),
                            mean(fBodyAcc.std...X),
                            mean(fBodyAcc.std...Y),
                            mean(fBodyAcc.std...Z),
                            mean(fBodyAccJerk.std...X),
                            mean(fBodyAccJerk.std...Y),
                            mean(fBodyAccJerk.std...Z),
                            mean(fBodyGyro.std...X),
                            mean(fBodyGyro.std...Y),
                            mean(fBodyGyro.std...Z),
                            mean(fBodyAccMag.std..),
                            mean(fBodyBodyAccJerkMag.std..),
                            mean(fBodyBodyGyroMag.std..),
                            mean(fBodyBodyGyroJerkMag.std..))

# output grouped data to a text file:
write.table(summary_table1, file = "tidydata.txt", row.name=FALSE)
