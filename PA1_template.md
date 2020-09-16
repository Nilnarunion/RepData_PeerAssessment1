---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

1. Load the data

```r
# unzip('activity.zip')
activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(ggplot2)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
totalSteps <- aggregate(steps~date,
                        data=activity,
                        sum,
                        na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day

```r
ggplot(data= totalSteps, aes(steps)) + 
  geom_histogram(col="black",
                 binwidth = 2000,
                 aes(fill=..count..))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

* Calculate and report the **mean** and **median** total number of steps taken 
per day 


```r
summary(totalSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```
* The **mean** total number of steps taken per day is 
    1.0766189\times 10^{4} steps.
* The **median** total number of steps taken per day is 
    10765 steps.


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInterval <- aggregate(steps~interval,
                           data=activity,
                           mean,
                           na.rm=TRUE)

ggplot(data=stepsInterval, aes(x=interval, y=steps, group=1)) +
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

```r
stepsInterval[which.max(stepsInterval$steps),]$interval
```

```
## [1] 835
```

It is the **835th** interval.


## Imputing missing values  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
Total 2304 rows are missing.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I used a strategy for filing in all of the missing values with the median for that 5-minute interval. First of all, I made a function **"interval2steps"** to get the median steps for particular 5-minute interval. 

```r
median_stepsInterval <- aggregate(steps~interval,
                                  data=activity,
                                  median,
                                  na.rm=TRUE)

interval2steps <- function(interval){
    median_stepsInterval[median_stepsInterval$interval == interval,]$steps
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityFilled <- activity
count <- 0           # Count the number of data filled in
for(i in 1:nrow(activityFilled)){
    if(is.na(activityFilled[i,]$steps)){
        activityFilled[i,]$steps <- interval2steps(activityFilled[i,]$interval)
        count=count+1
    }
}
cat("Total ",count, "NA values were filled.\n\r")  
```

```
## Total  2304 NA values were filled.
## 
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
totalSteps <- aggregate(steps~date,
                        data=activityFilled,
                        sum)

ggplot(data= totalSteps, aes(steps)) + 
  geom_histogram(col="black",
                 binwidth = 2000,
                 aes(fill=..count..))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
summary(totalSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10395    9504   12811   21194
```
* The **mean** total number of steps taken per day is 
9503.8688525 steps.
* The **median** total number of steps taken per day is 
10395 steps.

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean and median values shows a little difference. A new peak has appeared on the histogram for smaller step numbers.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityFilled$day <- ifelse( as.POSIXlt(activityFilled$date)$wday%%6 == 0,
                          "weekend","weekday")

# For Sunday and Saturday : weekend, Other days : weekday 
activityFilled$day <- factor(activityFilled$day,levels=c("weekday","weekend"))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
stepsInterval <- aggregate(steps~interval+day,
                           activityFilled,
                           mean)
ggplot(data=stepsInterval, aes(x=interval, y=steps, group=1)) +
  geom_line() +
  facet_grid( rows = vars(day))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

