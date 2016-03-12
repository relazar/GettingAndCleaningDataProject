```r
##################### STEP 1 - ASSIGN ALL LIBRARIES TO BE USED IN THE PROGRAM #########################
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)

##################### STEP 2 - DOWNLOAD FILE FROM THE INTERNET AND UNZIP #########################

## Set the url, name of the file and path of the destination folder
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "UCI HAR Dataset.zip"
destpath <- "C:/Users/relazar/Documents/Personal/CourSera/Data Science/03 - Getting and Cleaning Data/Course Project/Data"

## If the destination path doesn't exist then create it
if (!file.exists(destpath)) 
{
  dir.create(destpath)
}

## Download the zipped file from the internet into the destination folder and assign it as the filename
download.file(url, paste(destpath,"/",filename,sep=""))

## Set the current wprking directory to be the folder where the zip file has been downloaded
setwd(destpath)

## Unzip the contents of the zip file
unzip(filename)




##################### STEP 3 - UPLOAD TEXT FILES INTO INTO R #########################

## Set the current master directory to be the folder where the unzipped
## content is saved ##
setwd(paste(getwd(),"/UCI HAR Dataset",sep=""))

## Create a data frame which will contain:
# 1. The directory name from where the text file will be sourced
# 2. The name of the text file
# 3. The name of the data set in R to which this file will be uploaded and assigned to


## Create a function which given a folder name will return a vector of all text files contained in that directory
## Inputs:
# 1. type - indicating whether the first sub folder is "train" or "test"

## Outputs - a data frame containing three columns:
# 1. The full path of the text file
# 2. The name of the text file without the suffix - this will be the name of the r data set to be assigned
# 3. The type of the data set - training or test

find_file_names <- function(type)
{
  ## If the type is not train or test then leave the first sub folder as blank
  if (type %in% c("train", "test") == FALSE)
  {
    subfolder1 <- ""
  }
  else
  {
    subfolder1 <- paste(type, "/", sep="")
  }
  
 
    # Assign the path name
  pathname <- paste("./",subfolder1, sep="") 
  
  # Use the list.files function to list all files within the path name assigned
  files <- list.files(path = pathname, pattern = NULL, all.files = TRUE, full.names = TRUE)
  
  # Using the grepl function, return a list of only files which are text files
  # i.e. end with the suffix ".txt"
  # Exclude the features_info and README files as these will not be read in
  paths <- files[grepl(".txt$", files)==TRUE & 
                 grepl("features_info.txt$", files)==FALSE & 
                 grepl("README.txt$", files)==FALSE]
  
  # Get the name of each file without the .txt suffix
  rname <- rep(NA, length(paths))
  for (i in 1:length(paths))
  {
    lastslash <- regexpr("/[^/]*$", paths[i])
    lastdot <- regexpr("\\.[^\\.]*$", paths[i])
    rname[i] <- substr(paths[i],lastslash[1]+1, lastdot[1]-1)
  }
  
  
  # Create a data frame contain three columns:
  # 1. The full path of the text file
  # 2. The name of the text file without the suffix - this will be the name of the r data set to be assigned
  # 3. The type of the data set - training or test
  data.frame(pathname= paths, rdataname = rname, settype = rep(type, length(paths)))
  
}

## Upload all files from the master, training and testing folders

masterFiles <- find_file_names("")
trainFiles <- find_file_names("train")
testFiles <- find_file_names("test")

## Combine into one list of files
all_file_names <- rbind(masterFiles, trainFiles, testFiles)
all_file_names



## Now upload all the files to the respective R dat set using the assign() function
for (i in 1:nrow(all_file_names))
{
  assign(as.character(all_file_names$rdataname[i]), read.table(as.character(all_file_names$pathname[i]), sep=""))
}



dim(activity_labels)
dim(features)
dim(subject_train)
dim(X_train)
dim(y_train)
dim(subject_test)
dim(X_test)
dim(y_test)

head(activity_labels)
head(features)
head(subject_train)
head(X_train)
head(y_train)
head(subject_test)
head(X_test)
head(y_test)


##################### STEP 4 - MERGE ALL TRAINING AND TEST DATA SETS TOGETHER ALONG WITH LABELS #########################






subject <- rbind(subject_train, subject_test)
y <- rbind(y_train, y_test)
X <- rbind(X_train, X_test)

# Create a unique id to join all 3 sets together
create_id <- function(data)
{
  mutate(data, id = 1:nrow(data))
}

subject <- create_id(subject)
y <- create_id(y)
X <- create_id(X)


# For the subject and y set rename the column names
setnames(subject, "V1", "subject")
setnames(y, "V1", "activityNum")


## Join all three data sets together

datasets <- list(subject, y, X)
all_data <- join_all(datasets, by="id")

head(all_data)


## Join the activity labels as well
# Start by renaming the fields in the activity_labels and y data sets
setnames(activity_labels, c("V1","V2"), c("activityNum", "activity"))

## Join together with the previous data set created
all_data_2 <- join_all(list(activity_labels, all_data), by="activityNum")
head(all_data_2)



##################### STEP 5 - FIND THE FEATURES WITH MEAN OR STD ONLY #########################

## features
## X_train
## X_test

## In the features vector, keep only those that have mean or std in the 2nd column

# Begin by setting new names for the columns
setnames(features, names(features), c("featNum", "featName"))

# Subset only those names containing "mean or "std" using the grepl function
featmeanstd <- features[grepl("mean\\(\\)|std\\(\\)", features$featName)==TRUE,]

# Create a field name equivalent to that in the training and test sets
featmeanstd <- mutate(featmeanstd, field = paste("V",featmeanstd$featNum, sep=""))
head(featmeanstd)
# Keep a list of fieldnames we want to keep in the all_data set
names(all_data_2)

fieldnames <- c("activityNum","activity","subject", "id", featmeanstd$field)
fieldnames

## Now need to figure out how to select only the names in the above vector!
all_data_2 <- all_data_2[,names(all_data_2) %in% fieldnames]

## Rename fields
RenameField <- c("activityNum","activity","subject", "id", as.character(featmeanstd$featName))
setnames(all_data_2, names(all_data_2), RenameField)
names(all_data_2)

# Sort the data set by the subject and activity number
all_data_3 <- arrange(all_data_2, subject, activityNum)
head(all_data_3)

##################### STEP 6 - RESTRUCTURE THE DATA SET #########################
## In this step we want to split the data into several columns with the following variables for each:
## Column 1: Domain Signals {Time; Frequency}
## Column 2: Accelarator    {Body; Gravity; Not Available}
## Column 3: Instrument     {Accelerometer; Gyroscope}
## Column 4: Jerk           {Yes; No}
## Column 5: Magnitude      {Yes; No}
## Column 6: Measure        {Mean; Standard Deviation}
## Column 7: Axis           {X; Y; Z; Not Applicable}
## Column 8: Value          {[Floating Number]}

## First, melt the data set by creating a column showing the named feature of each of the feature variables
names(all_data_3)
all_data_4 <- melt(all_data_3, id.vars = c("activityNum","activity","subject","id"))

## Next, by examining the string of the variable we can create the columns as stated at beginning of STEP 5 section

## Column 1: Domain Signals {Time; Frequency}
domvec <- data.frame(t=grepl("^t",all_data_4$variable),f=grepl("^f",all_data_4$variable))
domvec$val[which(domvec$t==FALSE & domvec$f == TRUE)] <- "Frequency"
domvec$val[which(domvec$t==TRUE & domvec$f == FALSE)] <-"Time"
# Assign to a new column in the data set
all_data_4$DomainSignal <- domvec$val


## Column 2: Accelarator    {Body; Gravity; Not Available}
accvec <- data.frame(b=grepl("Body",all_data_4$variable),g=grepl("Gravity",all_data_4$variable))
accvec$val[which(accvec$b==TRUE & accvec$g==FALSE)] <- "Body"
accvec$val[which(accvec$b==FALSE & accvec$g==TRUE)] <- "Gravity"
# Assign to a new column in the data set
all_data_4$Acceleration <- accvec$val

## Column 3: Instrument     {Accelerometer; Gyroscope}
insvec <- data.frame(a=grepl("Acc",all_data_4$variable),c=grepl("Gyro",all_data_4$variable))
insvec$val[which(insvec$a==FALSE & insvec$c==TRUE)] <- "Gyroscope"
insvec$val[which(insvec$a==TRUE & insvec$c==FALSE)] <- "Accelerometer"
# Assign to a new column in the data set
all_data_4$Instrument <- insvec$val

## Column 4: Jerk           {Yes; No}
jerkvec<- data.frame(yn=grepl("Jerk",all_data_4$variable))
jerkvec$val[which(jerkvec$yn==TRUE)]<- "Yes"
jerkvec$val[which(is.na(jerkvec$val))]<- "No"
# Assign to a new column in the data set
all_data_4$Jerk <- jerkvec$val

## Column 5: Magnitude      {Yes; No}
magvec<- data.frame(yn=grepl("Mag",all_data_4$variable))
magvec$val[which(magvec$yn==TRUE)]<- "Yes"
magvec$val[which(is.na(magvec$val))]<- "No"
# Assign to a new column in the data set
all_data_4$Magnitude <- magvec$val

## Column 6: Measure        {Mean; Standard Deviation}
msdvec <- data.frame(m=grepl("mean()",all_data_4$variable),s=grepl("std()",all_data_4$variable))
msdvec$val[which(msdvec$m==FALSE & msdvec$s==TRUE)] <- "Standard Deviation"
msdvec$val[which(msdvec$m==TRUE & msdvec$s==FALSE)] <- "Mean"
# Assign to a new column in the data set
all_data_4$Measure <- msdvec$val

## Column 7: Axis           {X; Y; Z; Not Applicable}
axisvec <- data.frame(x=grepl("-X",all_data_4$variable),
                      y=grepl("-Y",all_data_4$variable),
                      z=grepl("-Z",all_data_4$variable))

axisvec$val[which(axisvec$x==FALSE & axisvec$y==FALSE & axisvec$z==FALSE)] <- "Not Applicable"
axisvec$val[which(axisvec$x==TRUE & axisvec$y==FALSE & axisvec$z==FALSE)] <- "X"
axisvec$val[which(axisvec$x==FALSE & axisvec$y==TRUE & axisvec$z==FALSE)] <- "Y"
axisvec$val[which(axisvec$x==FALSE & axisvec$y==FALSE & axisvec$z==TRUE)] <- "Z"
# Assign to a new column in the data set
all_data_4$Axis <- axisvec$val

head(all_data_4)
tail(all_data_4)




##################### STEP 7 - SUMMARISE THE DATA BY TAKING AVERAGES AND COUNTS #########################

## Group by the names by which we wish to summarise

all_data_5 <- group_by(all_data_4, subject, activity, DomainSignal, Acceleration, Instrument,
                       Jerk, Magnitude, Measure, Axis)
head(all_data_5)

## Take the mean of the value and count of occurences in each bucket
all_data_6 <- summarise(all_data_5, Average =mean(value), Count = n())
head(all_data_6)
dim(all_data_6)

## Order the data set by the subject and the activity
all_data_7 <- arrange(all_data_6, subject, activity)
head(all_data_7)
##################### STEP 8 - EXPORT THE DATA TO TEXT FILE #########################


write.table(all_data_7,"./Tidy Data Set.txt",quote=FALSE, sep="\t", row.names=FALSE)


##################### STEP 9 - CREATE A CODE BOOK ##################################


## Get the unique list of values for each variable
ActivityName <- gsub("_", " ", paste(unique(all_data_4$activity), sep="", collapse=", ")) 
DomainSignal <- paste(unique(all_data_4$DomainSignal), sep="", collapse=", ") 
Acceleration <- paste(unique(all_data_4$Acceleration), sep="", collapse=", ") 
Instrument <- paste(unique(all_data_4$Instrument), sep="", collapse=", ") 
Jerk <- paste(unique(all_data_4$Jerk), sep="", collapse=", ") 
Magnitude <- paste(unique(all_data_4$Magnitude), sep="", collapse=", ") 
Measure <- paste(unique(all_data_4$Measure), sep="", collapse=", ") 
Axis <- paste(unique(all_data_4$Axis), sep="", collapse=", ") 


# Create a description vector of each variable in the tidy data set
desc <- c("The ID of the subject who performed the activity. Values are numeric and range from 1 to 30",
          paste("The name of activity undertaken. The activities are: {", ActivityName, "}", sep=""),
          paste("The time or frequency domain signal. The singals are: {", DomainSignal, "}", sep=""),
          paste("The acceleration signal: {", Acceleration, "}", sep=""),
          paste("The measuring instrument. The instruments are: {", Instrument, "}", sep=""),
          paste("An indication of whether the Jerk signal was calculated. Possible values are: {", Jerk, "}", sep=""),
          paste("An indication of whether the magnitude of the signals was calculated. Possible values are: {", Magnitude, "}", sep=""),
          paste("The measures which were calculated from the original observations. Possible values are: {", Measure, "}", sep=""),
          paste("The axis on which the signals were calculated. Possible values are: {", Axis, "}", sep=""),
          "The average value for each bucket",
          "The count of each bucket"
          )

# The codebook is a data frame containing two columns:
# 1. The name of each variable in the tidy data set
# 2. The description of each variable in the tidy data set
codebook <- data.frame(VariableName = names(all_data_7), Description = desc)

# Export to codebook into a csv file
write.table(codebook,"./Code Book.txt",quote=FALSE, sep="\t", row.names=FALSE)


r
