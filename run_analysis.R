#read all numeric from file to list
readNumbersFromFile <- function(filename)
{
  print(paste("reading",filename,"file"))
  data <- strsplit(scan(filename, what = "", sep = "\n"),"[[:space:]]+")
  data <- lapply(data, as.numeric)
  data <- data[!is.na(data)]
  data <- lapply(data, function(e) { e[!is.na(e)]} )
}

#correct name (remove brackets, leading 'f' or 't' cymbol, capitalize words mean and std, remove '-' symbol)
correctName <- function(name)
{
  name<-sub("\\(\\)","",name)
  name<-sub("^t","Time",name)
  name<-sub("^f","Frequency",name)
  name<-sub("mean","Mean",name)
  name<-sub("std","Std",name)
  name<-gsub("-","",name)
}

#read global data from dirrectory 'type' (like "test" or "train)
readData <- function(type)
{
  setwd(type)
  
  #reading main data of 561 variables
  n <- readNumbersFromFile(paste0("X_",type,".txt"))
  #matrix from numbers
  m <- matrix(unlist(n), ncol=561,byrow = T)
  #convert matrix to list of columns
  l<-lapply(seq_len(ncol(m)), function(i) m[,i])
  #get subjects
  s <- unlist(readNumbersFromFile(paste0("subject_",type,".txt")))
  #get activity identifiers
  y <- unlist(readNumbersFromFile(paste0("y_",type,".txt")))
  
  setwd("..")
  
  #get feature names
  features <- read.csv("features.txt",sep=" ",header = F)
  #create first data frame
  f<-data.frame(s,y)
  names(f)[1] <- "Subject"
  #read activity names
  activities <- read.csv("activity_labels.txt",sep=" ",header = F)
  activities$V2 <- as.character(activities$V2)
  #and set them as a column to data frame
  f[,"Activity"] <- activities$V2[f$y]
  #remove activity identifier
  f<-select(f,-y)
  #filter only std and mean columns from entire data
  names <- as.character(features$V2)
  namefilter <- grepl("mean\\(\\)|std\\(\\)",names)
  f[names[namefilter]] <- l[(1:561)[namefilter]]
  #make column names more readable
  names(f) <- correctName(names(f))
  f
}

setwd("c:/R")
setwd("UCI HAR Dataset")
test <- readData("test")
train <- readData("train")
#concatanate 2 data frames
data <- rbind(train,test)
rm(test)
rm(train)

#new tidy data for average of activities and subjects
library(dplyr)
av_data <- group_by(data,Subject,Activity) %>% summarise_each(funs(mean)) %>% arrange(Subject,Activity)

#saving results to files
write.table(data,"dataset")
write.table(av_data,"averages")

