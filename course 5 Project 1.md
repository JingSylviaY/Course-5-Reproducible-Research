

##Loading and preprocessing the data

```r
knitr::opts_chunk$set(echo = TRUE)
setwd("C:/Users/yinjing/Desktop/R/Coursera")
activity<-read.csv("activity.csv")
str(activity)
table(is.na(activity$steps))
activity$date<-as.Date(activity$date)
```

##What is mean total number of steps taken per day?
missing values are ignored.
```{r}
echo=TRUE

steps_day<-aggregate(steps~date, data=activity,sum, na.action=na.omit)
hist(steps_day$steps,xlab="steps taken per day", main="Total number of steps taken per day")
#calcultae the mean and median
```
![plot of chunk Total number of steps taken per day](./Course-5-Reproducible-Research/Total number of steps taken per day.png)

```
mean(steps_day$steps)
median(steps_day$steps)     
```
##What is the average daily activity pattern?
```{r}
library(ggplot2)
library(dplyr)
#time series plot: 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis)
steps_interval<-aggregate(steps~interval, data=activity,mean, na.action=na.omit)
plot(steps_interval$interval, steps_interval$steps, type="l")

#which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

steps_interval[steps_interval$steps==max(steps_interval$steps),1]
#steps_interval<-steps_interval%>%arrange(desc(steps))
#head(steps_interval, 1)

```
##Imputing missing values

```{r}
#Number of NAs in original dataset
table(is.na(activity$steps))

```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#####Method 1:filling in NAs with the average 5-minute interval
```{r}
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
median(steps_day_New$steps)

```

#####Method 2: filling in NAs will be to substitute the missing steps with the average 5-minute interval based on **the day of the week**.
```{r}
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
median(steps_day_New_2$steps)
```

##Are there differences in activity patterns between weekdays and weekends?
```{r}
#create two levels: weekday, weekend

library(ggplot2)

activity_New_2<-activity_New_2%>%mutate(week_day=ifelse(activity_New_2$date %in% c("Saturday","Sunday"), "Weekend","Weekday"))

#plot
#steps_week_day<-activity_New_2%>% group_by(interval, week_day)%>% summarise(avg_steps = mean(steps))

steps_week_day<-aggregate(steps~interval+week_day, data=activity_New_2, mean)
steps_week_day$interval<-as.numeric(steps_week_day$interval)
ggplot(steps_week_day, aes(x=interval, y=steps, color=week_day ))+geom_line()+
  labs(title = "Average Steps per Interval Based on Type of Day", x = "Interval", y = "No. of Steps")+
  facet_wrap(~week_day, ncol=1, nrow=2)+theme_classic()

```
activity level folows similar pattern during weekend and weekdays.During weekdays,people is more active in the early time of the day; during weekend, people tend to be more active throughout the times, and comparatively more active later of the day than weekdays.




