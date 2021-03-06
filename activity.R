#Loading and preprocessing the data

activity <- read.csv("activity.csv")
summary(activity)
str(activity)
head(activity)

# removing NA's
noNA_activity <- na.omit(activity)
summary(noNA_activity)
str(noNA_activity)
head(noNA_activity)

#What is mean total number of steps taken per day?
date <- unique(activity$date)

#sum the number of steps for each day
dailySteps <- aggregate(cbind(noNA_activity$steps)~noNA_activity$date, FUN=sum)
TotalSteps <- dailySteps$V1
#Make a histogram of the total number of steps taken each day
par(mfrow=c(1,1)) 
png("histTotalSteps.png", width=480, height=480)
hist(TotalSteps)

#Calculate and report the mean and median total number of steps taken per day
avgSteps <- aggregate(cbind(noNA_activity$steps)~noNA_activity$date, FUN=mean)
names(avgSteps)[1] <- "Date"
names(avgSteps)[2] <- "Mean"
avgSteps
medSteps <- aggregate(cbind(noNA_activity$steps)~noNA_activity$date, FUN=median)
names(medSteps)[1] <- "Date"
names(medSteps)[2] <- "Median"
medSteps

#What is the average daily activity pattern?
intervals <- aggregate(cbind(noNA_activity$steps)~noNA_activity$interval, FUN=mean)
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
par(mfrow=c(1,1)) 
png("intervalsplot.png", width=480, height=480)
plot(intervals, type = "l")
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
names(intervals)[1] <- "FiveminInt"
names(intervals)[2] <- "Average"
intervals$FiveminInt[intervals$Average == max(intervals$Average)]

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nrow(activity)-nrow(noNA_activity)

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in. activity1 is the new dataset
activity1 <- activity
for(i in 1:length(activity1$steps)){
  if(is.na(activity1$steps[i])){
    activity1$steps[i] <- 0  ## since the median for each day is zero, I am assigning 0 to each NA value.
  }
}

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
#sum the number of steps for each day
dailySteps1 <- aggregate(cbind(noNA_activity$steps)~noNA_activity$date, FUN=sum)
TotalSteps1 <- dailySteps$V1
#Make a histogram of the total number of steps taken each day
par(mfrow=c(1,1)) 
png("TotalSteps1.png", width=480, height=480)
hist(TotalSteps1)

#Calculate and report the mean and median total number of steps taken per day
avgSteps1 <- aggregate(cbind(activity1$steps)~activity1$date, FUN=mean)
names(avgSteps1)[1] <- "Date"
names(avgSteps1)[2] <- "Mean"
avgSteps1
medSteps1 <- aggregate(cbind(activity1$steps)~activity1$date, FUN=median)
names(medSteps1)[1] <- "Date"
names(medSteps1)[2] <- "Median"
medSteps1

head(medSteps)
head(medSteps1)
## Median does not change, with NA's, ignoring NA's and changing NA's to zero the median is still zero for each day. 
head(avgSteps)
head(avgSteps1)
## Mean only changes for days that had NA values.  
head(TotalSteps)
head(TotalSteps1)
## Total daily number of steps does not change because I only added zero's into the NA value.
##If you add the mean steps instead of median into the NA's the total steps for days with NA's would increase. 

##Are there differences in activity patterns between weekdays and weekends?
##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Date1 <- as.Date(activity1$date)
weekday <- weekdays(Date1)
for(z in 1:length(weekday)){
if (weekday[z] == "Saturday" || weekday[z] == "Sunday"){
  weekday[z] <- "Weekend"
} else (weekday[z] <- "Weekday")
}
weekdf <- data.frame(activity1, weekday)


##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
##See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
weekdf1 <- subset(weekdf, weekday == "Weekend")
WeekendInt <- aggregate(cbind(weekdf1$steps)~weekdf1$interval, FUN=mean)

weekdf2 <- subset(weekdf, weekday == "Weekday")
WeekdayInt <- aggregate(cbind(weekdf2$steps)~weekdf2$interval, FUN=mean)

par(mfrow=c(2,1)) 
png("panel.png", width=480, height=480)
plot(WeekendInt, type = "l", main = "Weekend", xlab = "5-minute interval", ylab = "avg num of steps")
plot(WeekdayInt, type = "l", main = "Weekday", xlab = "5-minute interval", ylab = "avg num of steps")
