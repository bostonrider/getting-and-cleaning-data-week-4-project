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
    
    # (1) connect subject id with activity code and training set
          train_with_id<-cbind( subject_train, traininglabel,  trainingset)
          
     #  (2) connect subject id with activity code and testing set
          
          testing_with_id<-cbind(subject_test, testlabel,   testset)
          
      #(3) append testing set to training set 
          comb1<-rbind(train_with_id, testing_with_id )
          
        #rename subject_id and code in order to keep the columns in step 3
          colnames(comb1)[1] <- "Subject_idmean"
          
          colnames(comb1)[2] <- "codestd" 
         
   
    
  # 3. extract only the measurement on the mean and standard deviation for each measurement
    
    comb <- comb1[ , grepl("mean|std"  , colnames( comb1 ) ) ]
    
   #check the data set comb names to make sure the column names oly contain mean and std
    names(comb)
   
   ## 4. Uses descriptive activity names to name the activities in the data set
    
    
    code_group <-factor(comb$codestd)
    levels(code_group) <- activities[,2]
    comb$codestd <-code_group
    
    
  ## 5. Appropriately labels the data set with descriptive variable names. 
   
   
    
    # use function gsub(search_term, replacement_term, string_searched, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    # use function gsub to replace the following:
    # (1) prefix t with Time
    # (2) Acc with Accelerometer
    # (3) "Gyro"with  "Gyroscope"
    # (4) prefix "f" with  "Frequency"
    # (5) "Mag"with  "Magnitude"
    # (6) "BodyBody" with  "Body"
    # (7)  "angle" with "Angle"
    
      names(comb)<-gsub("^t", "Time", names(comb))
    
      names(comb)<-gsub("Acc", "Accelerometer", names(comb))
      names(comb)<-gsub("Gyro", "Gyroscope", names(comb))
      names(comb)<-gsub("^f", "Frequency", names(comb))
      names(comb)<-gsub("Mag", "Magnitude", names(comb))
      names(comb)<-gsub("BodyBody", "Body", names(comb))
     
      names(comb)<-gsub("angle", "Angle", names(comb))
      
      # rename  column1 and column2 of data comb
      colnames(comb)[2] <- "Test_activity" 
      colnames(comb)[1] <- "Subject_id" 
    
    # check the names
      
      names(comb)
    #From the data set in step 5, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
      
     
      tidy_data <-aggregate(. ~Subject_id + Test_activity, comb, mean)
      tidy_data<-tidy_data[order(tidy_data$Subject_id, tidy_data$Test_activity),]
      write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE)
      
      
     