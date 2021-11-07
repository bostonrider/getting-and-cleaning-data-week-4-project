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
    ## use extracted for extract only mean and std
    extracted <- grepl("mean|std", features)
    
    colnames(features)<-c("num", "functions")
  
    subject_train <-read.table("UCI HAR Dataset/train/subject_train.txt")
    colnames(subject_train)<-"subject_id"
  
    subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
    colnames(subject_test)<-"subject_id"
    
    trainingset <-read.table("UCI HAR Dataset/train/X_train.txt")
    colnames(trainingset)<-features$functions 
    traininglabel <-read.table("UCI HAR Dataset/train/y_train.txt")
    colnames(traininglabel) <-"code"
  
    testset<-read.table("UCI HAR Dataset/test/X_test.txt")
    colnames(testset)<-features$functions
    testlabel <-read.table("UCI HAR Dataset/test/y_test.txt") 
    colnames(testlabel)<-"code"
  
    activities <-read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("act_code", "activity"))
  
  
  ## 2. merge training sets and testing sets to create one data set
    merge_x<-rbind(trainingset, testset )
    
  # 3. extract only the measurement on the mean and standard deviation for each measurement
    
   # merge_x<-select(merge_x, contains("mean"), contains("std"))
    
    merge_x=merge_x[,extracted]
   
   #merge training label and test label
    
    merge_y <-rbind(traininglabel,testlabel )
    #merge training and testing subject
  
    merge_subject<-rbind(subject_train, subject_test)
  
  #merge everything together subject_id, code, and functions together
    comb<-cbind(merge_subject,merge_y,merge_x)
   
   ## 4. Uses descriptive activity names to name the activities in the data set
    
    
    code_group <-factor(comb$code)
    levels(code_group) <- activities[,2]
    comb$code <-code_group
    
    
  ## 5. Appropriately labels the data set with descriptive variable names. 
   
   
     #rename the column of "code" to test_activities: activities being tested
      
    # have a look at the names
      names(comb)
    # use function gsub(search_term, replacement_term, string_searched, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  
      names(comb)<-gsub("t", "Time", names(comb))
    
      names(comb)<-gsub("Acc", "Accelerometer", names(comb))
      names(comb)<-gsub("Gyro", "Gyroscope", names(comb))
      names(comb)<-gsub("^f", "Frequency", names(comb))
      names(comb)<-gsub("Mag", "Magnitude", names(comb))
      names(comb)<-gsub("BodyBody", "Body", names(comb))
      names(comb)<-gsub("\\.mean", "_Mean", names(comb))
      names(comb)<-gsub("\\.std ", "_std", names(comb))
      names(comb)<-gsub("angle", "Angle", names(comb))
      colnames(comb)[2] <- "Test_activity" 
      colnames(comb)[1] <- "Subject_id" 
    
      
    #From the data set in step 5, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
      
     
      tidy_data <-aggregate(. ~Subject_id + Test_activity, comb, mean)
      tidy_data<-tidy_data[order(tidy_data$Subject_id, tidy_data$Test_activity),]
      write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE)
      
      
     