# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
Show any code that is needed to Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your 
analysis

```r
activity_data <- read.csv(unz("activity.zip", "activity.csv"))
```
## What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day

```r
total_steps <- aggregate(activity_data$steps, list(activity_data$date), sum)
colnames(total_steps) <- c("date", "totalNumOfSteps")
hist(total_steps$totalNumOfSteps,  main ="Histogram of Total Number of Steps Taken Each Day", xlab= "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 
## Calculate and report the mean and median total number of steps taken per day

```r
mean_steps <- mean(total_steps$totalNumOfSteps,na.rm=TRUE)
median_steps <- median(total_steps$totalNumOfSteps,na.rm=TRUE)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```
Mean is 10,766
Median is 10,765

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activity_data[is.na(activity_data)] <- 0
average_daily_activity <- aggregate(activity_data$steps, list(activity_data$interval), mean)
colnames(average_daily_activity) <- c("interval", "steps")
plot(average_daily_activity$interval, average_daily_activity$steps, type="l",main = "Average Number of Steps Taken per 5 Minute Interval",
     xlab = "5 Minute Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
## Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?     

```r
maxaverage_daily_activity <- average_daily_activity[which.max(average_daily_activity$steps),1]
maxaverage_daily_activity
```

```
## [1] 835
```
Max Average is 835

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
activity_data <- read.csv(unz("activity.zip", "activity.csv"))
sum(is.na(activity_data))
```

```
## [1] 2304
```
Number of rows with NAs is 2,304

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, 

I will use the mean to impute missing data.

## Create a new dataset that is equal to the original dataset but with the 
## missing data filled in.

```r
dataImputed <- activity_data
dataImputed$steps[which(is.na(dataImputed$steps))] <- mean(activity_data$steps, na.rm = T)
Imputed_total_steps <- aggregate(dataImputed$steps, list(dataImputed$date), sum)
colnames(Imputed_total_steps) <- c("date", "totalNumOfSteps")
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
hist(Imputed_total_steps$totalNumOfSteps,  main ="Histogram of Revised Total Number of Steps Taken Each Day", xlab= "Revised Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
Imputed_mean_steps <- mean(Imputed_total_steps$totalNumOfSteps,na.rm=TRUE)
Imputed_median_steps <- median(Imputed_total_steps$totalNumOfSteps,na.rm=TRUE)
Imputed_mean_steps
```

```
## [1] 10766.19
```

```r
Imputed_median_steps
```

```
## [1] 10766.19
```
New Mean is 9,354
New Median is 10,395

Do these values differ from the estimates from the first part of the 
assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Yes, the new mean and median are different from the estimates from the first part of the assignment.  They are both lower now.

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dataImputed$dayType <- ifelse(weekdays(as.Date(dataImputed$date)) %in% c("Saturday", "Sunday"),"weekend", "weekday")

Newdataset <- aggregate(dataImputed$steps, list(dataImputed$interval, dataImputed$dayType), mean)
colnames(Newdataset) <- c("interval", "day","steps")
```
The weekday activity is higher than the weekend activity.

## Make a panel plot containing a time series plot (i.e. type = "l") of the 
5-minute interval (x-axis) and the average number of steps taken, averaged 
across all weekday days or weekend days (y-axis)

```r
library(lattice)
xyplot(steps ~ interval | day, data=Newdataset,type="l", ylab="Number of steps", layout=c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


