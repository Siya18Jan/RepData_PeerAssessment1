---
title: "Course project 1"
author: "Siya He"
date: "2023/1/24"
output: html_document
---
##Loading and preprocessing the data

```r
Activity <- read.csv("./activity.csv")
Activity <- na.omit(Activity)
```
##Plotting total steps taken per day

```r
Totalstep <- tapply(Activity$steps, Activity$date, sum)
hist(Totalstep)
```

![plot of chunk scatterplot](figure/scatterplot-1.png)
## the mean

```r
mean(Totalstep)
```

```
## [1] 10766.19
```
## the median

```r
median(Totalstep)
```

```
## [1] 10765
```
##What is the average daily activity pattern?

```r
pattern <- aggregate(steps ~ interval, data = Activity, FUN = mean, na.rm = TRUE)
plot(pattern$interval, pattern$steps)
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)
##Max

```r
pattern$interval[which.max(pattern$steps)]
```

```
## [1] 835
```
##Imputing missing values

```r
Activity2 <- Activity
addmiss <- function(mystep, myinterval){
  if(is.na(mystep))
    newvalue <- pattern$steps[which(pattern$interval == myinterval)]
  else
    newvalue <- mystep
  return(newvalue)
}
Activity2$steps <- mapply(addmiss, Activity2$steps, pattern$interval)
summary(Activity2)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:15264       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```
##Make a histogram

```r
hist(tapply(Activity2$steps, Activity2$date, sum))
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)
##Differences in activity patterns between weekdays and weekends

```r
Activity2$date <- as.Date(Activity2$date)
library(ggplot2)
isweekday <- function(idate){
  if(weekdays(idate) %in%c("Sunday", "Saturday"))
    return("weekend")
  else
    return("weekdays")
}
Activity2$datetype <- mapply(isweekday, Activity2$date)
ggplot(Activity2) + geom_point(aes(x = interval, y = steps))
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)




