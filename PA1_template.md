---
title: "ReproducibleReaerchProject1"
author: "rmydeen"
date: "12/29/2019"
output: 
  html_document: 
    keep_md: yes
---
## Assignment Steps  
### Read & process data ( Step 1)
 1. Read CSV file
 2. Process & transform CSV data (Step 1)
 
### Calculate Total number of steps (Steps 2 & 3)
 3. Calculate total number of steps per day
 4. Make a histrogram of total number of steps per day (        Step 2)
 5. Calculate & report the mean & median of the total number      of steps taken per day (Step 3)
 
### Average Daily Activity Pattern (Steps 4 & 5)
 6. Calculate average daily activity pattern
 7. Make a time series plot of the 5-minute interval (x-axis)     and the average number of steps taken, averaged across all     days (y-axis) (Step 4)
 8. Find which 5-minute interval, on average across all the     days in the dataset, contains the maximum number of steps.(Step 5)
 
### Impute Missing Values ( Steps 6 & 7 & 8)
 9. Calculate and report the total number of missing values     in the dataset.
10. Devise a strategy for filling in all of the missing         values in the dataset.( Step 6)
11. Make a histogram of the total number of steps taken each     day and Calculate and report the mean and median total      number of steps taken per day.( Step 7)
12. Make a panel plot containing a time series plot of the      5-minute interval (x-axis) and the average number of        steps taken, averaged across all weekday days or weekend     days (y-axis). (Step 8)

## R Code Part

```r
#Ingore warnings for Knitr
knitr::opts_chunk$set(warning=FALSE)
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```
### Step 1 - Code to read and process data

```r
#Following steps to download data and extract
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp, mode="wb")
unzip(temp, "activity.csv",exdir = "data")
activityData <- read.csv("./data/activity.csv")
unlink(temp)

activityData$date <- as.POSIXct(activityData$date, "%Y-%m-%d")
weekday <- weekdays(activityData$date)
activityData <- cbind(activityData,weekday)
# Dim
dim(activityData)
```

```
## [1] 17568     4
```

```r
# Summary of all activity data
summary(activityData)
```

```
##      steps             date               interval           weekday    
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Friday   :2592  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Monday   :2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Saturday :2304  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Sunday   :2304  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thursday :2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Tuesday  :2592  
##  NA's   :2304                                           Wednesday:2592
```

```r
#head
head(activityData)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0  Monday
## 2    NA 2012-10-01        5  Monday
## 3    NA 2012-10-01       10  Monday
## 4    NA 2012-10-01       15  Monday
## 5    NA 2012-10-01       20  Monday
## 6    NA 2012-10-01       25  Monday
```

```r
# Names
names(activityData)
```

```
## [1] "steps"    "date"     "interval" "weekday"
```

### Step 2 - total number of steps per day

```r
totalStepsPerDay <- with(activityData, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(totalStepsPerDay) <- c("date", "steps")

hist(totalStepsPerDay$steps, xlab="Total Number of Steps per day", ylab="Number of Days", main="Total Number of Steps per day",col="darkOrange",ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Step 3 - Mean & Median

```r
#Mean of total steps taken per day
mean(totalStepsPerDay$steps)
```

```
## [1] 9354.23
```

```r
# Median of total steps taken per day
median(totalStepsPerDay$steps)
```

```
## [1] 10395
```

### Step 4 - Daily pattern

```r
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
with(stepsPerInterval,plot(interval,steps, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Step 5 - Maximum number of Steps

```r
stepsPerInterval[which.max(stepsPerInterval[,2]),1]
```

```
## [1] 835
```

### Step 6 - With Imputing missing values

```r
valuesMissing <- is.na(activityData[,1])
m<-mean(stepsPerInterval$steps)
# Impute with mean valu

activityDataImp<-activityData
activityDataImp[valuesMissing,1]<-m
head(activityDataImp)
```

```
##     steps       date interval weekday
## 1 37.3826 2012-10-01        0  Monday
## 2 37.3826 2012-10-01        5  Monday
## 3 37.3826 2012-10-01       10  Monday
## 4 37.3826 2012-10-01       15  Monday
## 5 37.3826 2012-10-01       20  Monday
## 6 37.3826 2012-10-01       25  Monday
```

```r
totalStepsPerDayImp <- with(activityDataImp, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(totalStepsPerDayImp) <- c("date", "steps")
```
### Step 7 - histogram after imputed values

```r
hist(totalStepsPerDayImp$steps, xlab="Total Number of Steps per day", ylab="Number of Days", main="Total Number of Steps per day (Imputed)",col="darkGreen",ylim = c(0,35), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#Mean of total steps taken per day
mean(totalStepsPerDayImp$steps)
```

```
## [1] 10766.19
```

```r
# Median of total steps taken per day
median(totalStepsPerDayImp$steps)
```

```
## [1] 10766.19
```

### Step 8 Weekend vs Weekdays

```r
activityData$date <- as.Date(strptime(activityData$date, format="%Y-%m-%d"))
activityData$datetype <- sapply(activityData$date, function(x) {
        if (weekdays(x) == "Satureday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
activity_data_date <- aggregate(steps~interval + datetype, activityData, mean, na.rm = TRUE)
gplot<- ggplot(activity_data_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(gplot)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


