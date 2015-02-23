
#This program represents R script called run_analysis.R that does the following: 

# Step 1: Merges the training and the test sets to create one data set.
# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
# Step 3: Appropriately labels the data set with descriptive variable names. 
# Step 4: Uses descriptive activity names to name the activities in the data set

# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#pre-processing step
#Set working directory and build data frames from given csv files

library(dplyr)

feature_list<-read.csv("UCI HAR Dataset/features.txt", sep="", header= FALSE)
activity_labels<-read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header= FALSE)

training_ds<-read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header= FALSE)
training_act_ds<-read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header= FALSE)
training_subject_ds<-read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header= FALSE)
initial_col=ncol(training_ds)
training_ds<-cbind(training_ds,c(training_act_ds,training_subject_ds ))


testing_ds<-read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header= FALSE)
testing_act_ds<-read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header= FALSE)
testing_subject_ds<-read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header= FALSE)
testing_ds<-cbind(testing_ds,c(testing_act_ds,testing_subject_ds ))

# Step 1: Merge the training and the test sets to create one data set.
superset<-rbind(training_ds,testing_ds)

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
column_df<-feature_list[grep("*mean*|*Mean*|*std*|*Std*|*dev*",feature_list[,2]),]
filter_list<-c(column_df[,1], initial_col+1, initial_col+2)
filter_list_label<-feature_list[column_df[,1],]
superset<-superset[,filter_list]

# Step 3: Appropriately labels the data set with descriptive variable names. 

colnames(superset)<-c(filter_list_label$V2,"activity","subject")

# Step 4: Uses descriptive activity names to name the activities in the data set

for (i in 1:nrow(activity_labels)){
    superset$activity<-gsub(i,activity_labels[i,2],superset$activity)    
}


superset$activity<-as.factor(superset$activity)
superset$subject<-as.factor(superset$subject)
clean_data<-aggregate(superset,by=list(activity=superset$activity, subject =superset$subject),mean)

clean_data[,ncol(clean_data)]=NULL
clean_data[,ncol(clean_data)-1]=NULL
write.table(clean_data,"tidy.txt",sep="\t")
