# Course-5-Reproducible-Research

Activity
Loading and preprocessing the data

knitr::opts_chunk$set(echo = TRUE)
setwd("C:/Users/yinjing/Desktop/R/Coursera")
activity<-read.csv("activity.csv")
str(activity)

## 'data.frame':    17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

table(is.na(activity$steps))

## 
## FALSE  TRUE 
## 15264  2304

activity$date<-as.Date(activity$date)

What is mean total number of steps taken per day?

missing values are ignored.

echo=TRUE

steps_day<-aggregate(steps~date, data=activity,sum, na.action=na.omit)
hist(steps_day$steps,xlab="steps taken per day", main="Total number of steps taken per day")

#calcultae the mean and median
mean(steps_day$steps)

## [1] 10766.19

median(steps_day$steps)

## [1] 10765

What is the average daily activity pattern?

library(ggplot2)

## Warning: package 'ggplot2' was built under R version 3.3.3

library(dplyr)

## Warning: package 'dplyr' was built under R version 3.3.3

## 
## Attaching package: 'dplyr'

## The following objects are masked from 'package:stats':
## 
##     filter, lag

## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union

#time series plot: 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis)
steps_interval<-aggregate(steps~interval, data=activity,mean, na.action=na.omit)
plot(steps_interval$interval, steps_interval$steps, type="l")

#which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

steps_interval[steps_interval$steps==max(steps_interval$steps),1]

## [1] 835

#steps_interval<-steps_interval%>%arrange(desc(steps))
#head(steps_interval, 1)

Imputing missing values

#Number of NAs in original dataset
table(is.na(activity$steps))

## 
## FALSE  TRUE 
## 15264  2304

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Method 1:filling in NAs with the average 5-minute interval

#filling in NAs with the average 5-minute interval
activity_NAs<-activity[is.na(activity$steps),]
activity_NAs_New<-merge(activity_NAs, steps_interval, by="interval")
activity_NAs_New<-activity_NAs_New[-3]
colnames(activity_NAs_New)<-c("interval","date","steps")

#combine the two dataset, create a new dataset with the missing data filled in
activity_New<-rbind(activity_NAs_New, activity[!is.na(activity$steps),])
steps_day_New<-aggregate(steps~date, data=activity_New,sum)
hist(steps_day_New$steps, breaks=5, xlab="steps taken per day", main="Total number of steps taken per day", col="Black")
hist(steps_day$steps, breaks=5,xlab="steps taken per day", main="Total number of steps taken per day", col="grey",add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey"))

#mean and median of Steps with NA taken care of
mean(steps_day_New$steps)

## [1] 10766.19

median(steps_day_New$steps)

## [1] 10765

Method 2: filling in NAs will be to substitute the missing steps with the average 5-minute interval based on the day of the week.

steps_interval_day<-aggregate(steps~interval+weekdays(date), data=activity[!is.na(activity$steps),], mean)

activity_NAs<-activity[is.na(activity$steps),]
activity_NAs$date<-weekdays(activity_NAs$date)

activity_NAs_New_2<-merge(activity_NAs, steps_interval_day, by.x=c("interval", "date"), by.y=c("interval","weekdays(date)"))
activity_NAs_New_2<-activity_NAs_New_2[-3]
colnames(activity_NAs_New_2)<-c("interval","date","steps")


activity_New_2<-rbind(activity_NAs_New_2, activity[!is.na(activity$steps),])
steps_day_New_2<-aggregate(steps~date, data=activity_New_2,sum)

hist(steps_day_New_2$steps, breaks=5, xlab="steps taken per day", main="Total number of steps taken per day", col="Black")
hist(steps_day$steps, breaks=5,xlab="steps taken per day", main="Total number of steps taken per day", col="grey",add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey"))

#mean and median of Steps with NA taken care of
mean(steps_day_New_2$steps)

## [1] 11188.03

median(steps_day_New_2$steps)

## [1] 11162

Are there differences in activity patterns between weekdays and weekends?

#create two levels: weekday, weekend

