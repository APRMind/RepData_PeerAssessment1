#1.Code for reading in the dataset and/or processing the data

##Setting the Work Directory and Downloading Activity monitoring data File From the Internet

setwd("C:/Users/apram/Documents/RProject/RepData_PeerAssessment1/RepData_PeerAssessment1")
getwd()


##Download zip files from this location:
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip",method="curl")

##Unzip data file and read 
unzip("activity.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)

#2.Histogram of the total number of steps taken each day
library(magrittr)
library(dplyr)
databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)


##Copy my plot to a PNG file
#dev.copy(png, file = "Plot1.png", width=480, height=480)

## Don't forget to close the PNG device!
#dev.off()

#3. Calculate and report the mean and median of the total number of steps taken per day
mean(databydate$tsteps)
## [1] 10766.19
median(databydate$tsteps)
## [1] 10765


#4.Time series plot of the average number of steps taken

library(ggplot2)
databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()

#5.The 5-minute interval that, on average, contains the maximum number of steps
print(paste("Interval containing the most steps on average: ",databyinterval$interval[which.max(databyinterval$tsteps)]))
## [1] "Interval containing the most steps on average:  835"
print(paste("Average steps for that interval: ",round(max(databyinterval$tsteps),digits=2)))
## [1] "Average steps for that interval:  206.17"

#6.Code to describe and show a strategy for imputing missing data
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print(paste("The total number of rows with NA is: ",sum(is.na(stepdata$steps))))
## [1] "The total number of rows with NA is:  2304"
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)


#7.Histogram of the total number of steps taken each day after missing values are imputed

FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
##Summary of new data : mean & median
summary(FullSummedDataByDay)
##Making a histogram
hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps") #, breaks = 20)

#Calculate and report the mean and median total number of steps taken per day.

oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
# Old mean and New mean

print(paste("The total number of rows with NA is: ",round(max(oldmean),digits=2)))
print(paste("The total number of rows with NA is: ",round(max(newmean),digits=2)))

oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)

print(paste("The total number of rows with NA is: ",round(max(oldmedian),digits=2)))
print(paste("The total number of rows with NA is: ",round(max(newmedian),digits=2)))


#8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )
library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
        facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval") 


#Copy my plot to a PNG file
dev.copy(png, file = "Plot1.png", width=480, height=480)

## Don't forget to close the PNG device!
dev.off()
