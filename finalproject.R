# week 4 project
#change directory
#1.read and download data
    setwd("C:/R project/getting and clean data/final project")
    library(dplyr)
    storefile <-"projectfile.zip"
    urlzip <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(urlzip, storefile, method="curl")
    unzip(storefile)
    features<-read.table("UCI HAR Dataset/features.txt") 
    colnames(features)<-c("num", "functions")
  
    subject_train <-read.table("UCI HAR Dataset/train/subject_train.txt")
    colnames(subject_train)<-"subject_id"
  
    subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",col.names = "subject_id"  )
  
    trainingset <-read.table("UCI HAR Dataset/train/X_train.txt", col.names =features$functions )
    traininglabel <-read.table("UCI HAR Dataset/train/y_train.txt", col.names="code")
  
    testset<-read.table("UCI HAR Dataset/test/X_test.txt",col.names =features$functions )
    testlabel <-read.table("UCI HAR Dataset/test/y_test.txt", col.names="code")
  
    activities <-read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("act_code", "activity"))
  
  
  ## 2. merge training sets and testing sets to create one data set
    merge_x<-rbind(trainingset, testset )
    merge_y <-rbind(traininglabel,testlabel )
  
    merge_subject<-rbind(subject_train, subject_test)
  
  #merge everything together subject_id, code, and functions together
    comb<-cbind(merge_subject,merge_y,merge_x)
  
  ## 3. extract only the measurement on the mean and standard deviation for each measurement
  
    selected_data<-select(comb, subject_id, code, contains("mean"), contains("std"))
  
  ## 4. Uses descriptive activity names to name the activities in the data set
    selected_data[,2]<-activities[selected_data$code, 2]
    
  ## 5. Appropriately labels the data set with descriptive variable names. 
    #prefix t is replaced by time
    # Acc is replaced by Accelerometer
    #Gyro is replaced by Gyroscope
   # prefix f is replaced by Frequency
   # Mag is replaced by Magnitude
    #BodyBody is replaced by Body
    #.mean is placed by _mean
    #.std is placed by _std
    #angle is placed by Angle
   
     #rename the column of "code" to test_activities: activities being tested
      
    # have a look at the names
      names(selected_data)
    # use function gsub(search_term, replacement_term, string_searched, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  
      names(selected_data)<-gsub("t", "Time", names(selected_data))
    
      names(selected_data)<-gsub("Acc", "Accelerometer", names(selected_data))
      names(selected_data)<-gsub("Gyro", "Gyroscope", names(selected_data))
      names(selected_data)<-gsub("f", "Frequency", names(selected_data))
      names(selected_data)<-gsub("Mag", "Magnitude", names(selected_data))
      names(selected_data)<-gsub("BodyBody", "Body", names(selected_data))
      names(selected_data)<-gsub(".mean", "_Mean", names(selected_data))
      names(selected_data)<-gsub(".std ", "_std", names(selected_data))
      names(selected_data)<-gsub("angle", "Angle", names(selected_data))
      colnames(selected_data)[2] <- "Test_activity" 
      colnames(selected_data)[1] <- "Subject_id" 
    
    #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
      group<-group_by(selected_data, Subject_id, Test_activity )
      tidy_data <-summarise_all(group, funs(mean))
      write.table(tidy_data, file = "./tidy_data.txt")
      