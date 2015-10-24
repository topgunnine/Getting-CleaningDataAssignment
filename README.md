# Getting-CleaningDataAssignment
1. Some of my findings while researching the given files:
1.a. subject_test.txt file is of dimension (7351,1) 
1.b. Similarly, y_test.txt file is of dimension (2947,1)
1.c. features.txt file, i.e. dimension of (561,2) and which looks to have names of those 561 variables for datasets in train and test folders, can be used to inspire the labels in the resulting tidy data 

2. I, henceforth, decided to the following
2.a. Both X_test.txt and X_train.txt, which have dimensions of (2947,561) and (7352,561), will be row-wise merged (i.e. rbind) to give a resultant file
2.b. All 9 files in respective Inertial Signals folder (under both test and train folders, which have dimensions of (2947,128) and (7352,128) respectively) have different column dimension than feature.txt file; thus, it is not easy to reckon and pluck mean and std. dev values from these files - thus, all files in Inertial Signals folders will be ignored for this assignment
2.c. Thus, the approach that I undertake for this assignment is to rbind X_test and X_train data; thereafter cbind this data with y_test and y_train data & subject_test and subject_train data. Average out data for 30 subjects using subject_test and subject_train values and average out data for 6 activities using y_test and y_train  values. After this rbind the resultant data with features.txt file to obtain suitable variable names. From the same, take the subset that corresponds to mean() and std() variables to obtain the final tidy data 

3. When I unpacked my files, I got them stored in "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset" structure under WD. My code is written in order to fetch files from the same place

4. My code book can be found by the name codeBook.txt, which contains all the variables in the resulting dataset

5. My code is written so as to make sure that both the tidy data and code book are written into the working directory at the end of the execution of my script

run_analysis <- function() {

#This is the folder under working directory where data is stored
rootFolder <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset"

#Read X_test and X_train data
X_testfileName <- paste(getwd(),"/",rootFolder,"/","test/X_test.txt",sep="")
X_testfileData <- read.table(X_testfileName,header=FALSE,sep="")

X_trainfileName <- paste(getwd(),"/",rootFolder,"/","train/X_train.txt",sep="")
X_trainfileData <- read.table(X_trainfileName,header=FALSE,sep="")

#rbind X_test and X_train data (getting the raw measurements)
X_fileData <- rbind(X_testfileData,X_trainfileData)

#Read y_test and y_train data
y_testfileName <- paste(getwd(),"/",rootFolder,"/","test/y_test.txt",sep="")
y_testfileData <- read.table(y_testfileName,header=FALSE,sep="")

y_trainfileName <- paste(getwd(),"/",rootFolder,"/","train/y_train.txt",sep="")
y_trainfileData <- read.table(y_trainfileName,header=FALSE,sep="")

#rbind y_test and y_train data (getting the placeholders for activities)
y_fileData <- rbind(y_testfileData,y_trainfileData)

#Read subject_test and subject_train data
subject_testfileName <- paste(getwd(),"/",rootFolder,"/","test/subject_test.txt",sep="")
subject_testfileData <- read.table(subject_testfileName,header=FALSE,sep="")

subject_trainfileName <- paste(getwd(),"/",rootFolder,"/","train/subject_train.txt",sep="")
subject_trainfileData <- read.table(subject_trainfileName,header=FALSE,sep="")

#rbind subject_test and subject_train data (getting the placeholders for subjects)
subject_fileData <- rbind(subject_testfileData,subject_trainfileData)

#cbind subject_fileData, y_fileData and X_fileData
tidyData <- cbind(subject_fileData,y_fileData,X_fileData)

#Read part of header/label data from feature.txt file
headerFileName <- paste(getwd(),"/",rootFolder,"/","features.txt",sep="")
headerData <- read.table(headerFileName,header=FALSE,sep="")
headerData <- as.character(headerData[,2])

#Giving header/label names to tidy data
colnames(tidyData) <- c("Subject","Activity",headerData)

#Mean as per each subject and Activity

tidyData <- aggregate(. ~ Subject+Activity,data = tidyData,FUN=mean)


#Subset to give mean() and std() values

tidyData1 <- tidyData[, grepl("mean()", names(tidyData))]
tidyData1 <- tidyData1[ , -grep("meanFreq()",colnames(tidyData1))] #Removes meanFreq() columns
tidyData2 <- tidyData[, grepl("std()", names(tidyData))]

tidyData <- cbind(tidyData1,tidyData2)

return(tidyData)

#Write the data into a txt file in working directory
write.table(tidyData, "./tidyData.txt", sep="\t")

#Write the column names of resulting dataset into a codebook
write.table(colnames(tidyData),"./codeBook.txt",sep="\t")
}
