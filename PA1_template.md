---
title: "PA1_template"
author: "Brigitte Vogelsangs"
date: "24-1-2020"
output: 
  html_document: 
    keep_md: yes
---


```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.6.2
```

```r
knitr::opts_chunk$set(echo=TRUE)
```

# Course project 1

## Loading and preprocess the data
The code used to load the data. 
Dates are processed and summery created.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

activity <- read.csv("activity.csv")
activity$date <- as.POSIXct(activity$date, tz= "GMT", "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)
summary(activity)
```

```
##      steps             date               interval           weekday    
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   dinsdag  :2592  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   donderdag:2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   maandag  :2592  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   vrijdag  :2592  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   woensdag :2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   zaterdag :2304  
##  NA's   :2304                                           zondag   :2304
```

## What is the mean total number of steps taken per day?

Missing values in the dataset are ignored.

- Calculate the total number of steps taken per day
- Make a histogram of the total number of steps taken each day
- Calculate and report the mean and median of the total number of steps taken per day


```r
activity_steps_day <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_steps_day) <- c("date", "steps")
hist1 <- hist(activity_steps_day$steps, main = "Total number of steps taken per day", 
     xlab = "Total steps taken per day", col = "blue", ylim = c(0,20), 
     breaks = seq(0,25000, by=2500))
```

![](https://github.com/73bri/RepData_PeerAssessment1/blob/master/figure/hist1.png)<!-- -->

```r
mean(activity_steps_day$steps)
```

```
## [1] 9354.23
```

```r
median(activity_steps_day$steps)
```

```
## [1] 10395
```

## What is the daily average activity pattern?

- Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activity_day_average <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(activity_day_average) <- c("interval", "mean")
plot10 <- plot(activity_day_average$interval, activity_day_average$mean, type = "l", 
               col="blue", lwd = 2, xlab="Interval", ylab="Average number of steps", 
               main="Average number of steps per intervals")
```

![](https://github.com/73bri/RepData_PeerAssessment1/blob/master/figure/plot10.png?raw=true)<!-- -->

```r
activity_day_average[which.max(activity_day_average$mean), ]$interval
```

```
## [1] 835
```

## Imputing missing values
There are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). 
The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset.

Created a set without missing data activity new by using the mean for the day



```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
activity_replaceNA <- activity_day_average$mean[match(activity$interval, activity_day_average$interval)]
```

## Create a new dataset that is equal to the original dataset but with the missing data filled in.
- Make a histogram of the total number of steps taken each day
- Calculate and report the mean and median total number of steps taken per day. 
- Do these values differ from the estimates from the first part of the assignment? 
- What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activity_new <- transform(activity, steps = ifelse(is.na(activity$steps), yes = activity_replaceNA, no = activity$steps))
total_steps_new <- aggregate(steps ~ date, activity_new, sum)
names(total_steps_new) <- c("date", "daily_steps")
hist2 <- hist(total_steps_new$daily_steps, col = "blue", 
              xlab = "Total steps per day", ylim = c(0,30), 
              main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
```

![](https://github.com/73bri/RepData_PeerAssessment1/blob/master/figure/hist2.png?raw=true)<!-- -->

```r
mean(total_steps_new$daily_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_new$daily_steps)
```

```
## [1] 10766.19
```

There is a difference in value between the mean and the median

```r
difference1 <- mean(activity_steps_day$steps)-mean(total_steps_new$daily_steps)
difference2 <- median(activity_steps_day$steps)-median(total_steps_new$daily_steps)
difference1
```

```
## [1] -1411.959
```

```r
difference2
```

```
## [1] -371.1887
```
The impact shows a higher frequency of the total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity_new$date <- as.Date(strptime(activity_new$date, format="%Y-%m-%d"))
activity_new$datetype <- sapply(activity_new$date, function(x) {
        if (weekdays(x) == "zaterdag" | weekdays(x) =="zondag") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
str(activity_new$date)
```

```
##  Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
```

```r
str(activity_new$datetype)
```

```
##  chr [1:17568] "Weekday" "Weekday" "Weekday" "Weekday" "Weekday" "Weekday" ...
```

- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
activity_new$date <- as.Date(strptime(activity_new$date, format="%Y-%m-%d"))
activity_new$datetype <- sapply(activity_new$date, function(x) {
        if (weekdays(x) == "zaterdag" | weekdays(x) =="zondag") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
activity_time_series <- aggregate(steps~interval + datetype, activity_new, mean, na.rm = TRUE)
plot11 <- ggplot(activity_time_series, aes(x = interval , y = steps, color = datetype)) +
    geom_line() + labs(title = "Average daily steps by type of date", x = "Interval", 
                       y = "Average number of steps") + facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot11)
```

![](https://github.com/73bri/RepData_PeerAssessment1/blob/master/figure/plot11.png?raw=trueg)<!-- -->

