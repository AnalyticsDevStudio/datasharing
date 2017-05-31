# Reproducible Research: Project 1


```r
library(ggplot2)
library(plyr)
```

## Loading and preprocessing the data


```r
unzip("repdata_data_activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
SPD <- ddply(data, c("date"),summarise,TS = sum(steps,na.rm=TRUE))
```


```r
ggplot(data,aes(x=steps, col=date)) + geom_freqpoly(bins=10) + xlab("Total number of steps") + ylab("Frequency")+ ggtitle("Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day:

- Mean total number of steps taken per day = 9354.2295082 
- Median number of steps taken per day = 10395


## What is the average daily activity pattern?


```r
SP5m <- ddply(data, c("interval"),summarise, MS = mean(steps,na.rm=TRUE))
 
ggplot(SP5m,aes(x=interval,y=MS)) + geom_line() + ggtitle("Average steps for each 5-min interval") + xlab("Interval") + ylab("Mean steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

- Interval = 835 
- Mean = 206.1698113 steps.  

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

The dataset has 2304 missing values in the dataset.

```r
fillData <- aggregate(steps~interval, data=data, FUN=mean)
fillNA <- numeric()
for(i in 1:nrow(data)){
   x <- data[i, ]
   if (is.na(x$steps)){
       steps <- subset(fillData, interval==x$interval)$steps
   } else { 
       steps <- x$steps
   }
   
   fillNA <- c(fillNA, steps)
}
data$steps <- fillNA
ggplot(data,aes(x=steps, col=date)) + geom_freqpoly(bins=10) + xlab("Total number of steps") + ylab("Frequency")+ ggtitle("Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
SPD2 <- ddply(data, c("date"),summarise,TS2 = sum(steps,na.rm=TRUE))
```

Calculate and report the mean and median of the total number of steps taken per day:

- Mean total number of steps taken per day = 1.0766189\times 10^{4} 
- Median number of steps taken per day = 1.0766189\times 10^{4}


## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice)
data2 <- read.csv("activity.csv")
data2$date<- as.Date(data2$date,"%Y-%m-%d")
day <- weekdays(data2$date)

typeDay <- vector()
for (j in 1:nrow(data2)) {
  if (day[i] == "Saturday") {
    typeDay[j] <- "Weekend"
  } else if (day[j] == "Sunday") {
    typeDay[j] <- "Weekend"
  } else {
    typeDay[j] <- "Weekday"
  } 
  
}
data2$typeDay <- typeDay
data2$typeDay <- factor(data2$typeDay)
stepsByDay <- aggregate(steps ~ interval + typeDay, data = data2, mean)
names(stepsByDay) <- c("interval", "typeDay", "steps")
xyplot(steps ~ interval | typeDay, stepsByDay, type = "l",
       layout = c(1, 2),
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

According to the results, the activity is higher on the weekends.






