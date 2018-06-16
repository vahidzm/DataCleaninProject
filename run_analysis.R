# 06/011/2018
# VZM

# Read the samsung data, merge and clean the data. Extract mean and std values

## read features
features <- read.table("UCI HAR Dataset/features.txt")

# read and combine the training data
train_x <- read.table("UCI HAR Dataset/train/X_train.txt",col.names = features[,2])
train_y <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "y")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
train <- cbind(subject_train,train_x, train_y)

# read and combine the test data
test_x <- read.table("UCI HAR Dataset/test/X_test.txt",col.names = features[,2])
test_y <- read.table("UCI HAR Dataset/test/Y_test.txt",col.names = "y")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",col.names ="subject")
test <- cbind(subject_test,test_x,test_y)

# merge train and test data
all <- rbind(train,test)

# select only mean and std columns
all1 <-all[,c(1,grep("(.*)mean|std(.*)",colnames(all)),ncol(all))]

# use descriptive activity names
activities <- read.table("UCI HAR Dataset/activity_labels.txt",colClasses = "character")
all1[,"activity"] <-all1$y
for (nn in 1:6){ 
  all1$activity[which(all1$y==nn)] <- activities[nn,2]
  }


# find mean of each variable for each activity and each subject
# first split by activity and subject
alls <- split(all1, list(all$subject,all1$y))
# find averages
avgs <- sapply(alls,function(x){colMeans(x[,c(-1,-ncol(x),-ncol(x)-1)], na.rm = T)})
average <- aperm(avgs,2:1)
# add columns for activity and subject
subAct <- aperm(sapply(rownames(average),function(x){strsplit(x,"\\.")[[1]]}),2:1)
rownames(subAct) <- NULL
colnames(subAct) <- c("subject","activity")
final <- data.frame(cbind(subAct,average))
# clean up the name of the variables
colnames(final) <- gsub("\\.\\.\\.","_", colnames(final))
colnames(final) <- gsub("\\.\\.","", colnames(final))
colnames(final) <- gsub("\\.","_", colnames(final))
colnames(final) <- gsub("^t","", colnames(final))
colnames(final) <- gsub("^f","FFT", colnames(final))
 colnames(final) <- gsub("fBody","freq. body ", colnames(final))
colnames(final) <- gsub("BodyBody","Body", colnames(final))
colnames(final)[ncol(final)] <- "activity factor"

# Write data to a txt file
write.table(final,file = "final_averages.txt", row.names = F)
