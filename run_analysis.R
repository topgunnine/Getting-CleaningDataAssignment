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
