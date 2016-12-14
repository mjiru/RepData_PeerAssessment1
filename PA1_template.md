---
title: "Reproducible research1"
author: "Michaela Jírù"
date: "14 prosince 2016"
output: html_document
---


#Loading the data
At first, we load libraries, the working directory is set and the data is loaded into R.


```r
library(dplyr)
library(xtable)
library(ggplot2)
library(gridExtra)
library(timeDate)
 options(scipen=1, digits=2)
setwd("C:/Users/mjiru/Documents/Coursera/Reproducible research/")
data <- read.csv("activity.csv")
```

#What is mean total number of steps taken per day?

Calculate the total number of steps taken per day 


```r
steps_total <- data %>%
    group_by(date) %>%
    summarise(total_steps =sum (steps,na.rm=TRUE))
```
Display few of the results

```r
xt <- xtable(head(steps_total))
print(xt, type="html")
```

<!-- html table generated in R 3.2.5 by xtable 1.8-2 package -->
<!-- Wed Dec 14 16:14:16 2016 -->
<table border=1>
<tr> <th>  </th> <th> date </th> <th> total_steps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2012-10-01 </td> <td align="right">   0 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2012-10-02 </td> <td align="right"> 126 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2012-10-03 </td> <td align="right"> 11352 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2012-10-04 </td> <td align="right"> 12116 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2012-10-05 </td> <td align="right"> 13294 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2012-10-06 </td> <td align="right"> 15420 </td> </tr>
   </table>
Display the histogram of steps taken each day

```r
h1 <- ggplot(data= steps_total, aes(total_steps))+geom_histogram(color="black", fill="darkgreen")+
  ylab("Number of occurences")+
  xlab("Total steps taken on a day")+
  ggtitle("Histogram of total steps taken \n on a day.")+
  theme_bw()
h1
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Calculate the mean and median of the total number of steps taken per day.

```r
steps_mean <- steps_total %>%
    summarise(mean_steps =mean (total_steps,na.rm=TRUE),median_steps =median (total_steps,na.rm=TRUE) )
mean <- steps_mean$mean_steps
median <- steps_mean$median_steps
```
The mean value of total number of steps taken per day is **9354.23** and the median value is **10395**.

#What is the average daily activity pattern?
Calculate the average number of steps taken in the 5-minute interval.

```r
interval_avg <- data %>%
    group_by(interval) %>%
    summarise(mean = mean(steps, na.rm=TRUE) )
```
Display the results


```r
 a1 <- ggplot(data= interval_avg, aes( interval, mean))+geom_line(color="darkgreen", size=1.2)+
  ylab("Average steps taken")+
  xlab("Interval")+
  ggtitle("Average steps taken during \n all intervals.")+
  theme_bw()
a1
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Calculate the interval containing the maximum number of steps

```r
max <- interval_avg [interval_avg$mean==max(interval_avg$mean),]
max_int <- max$interval
max_value <- max$mean
```

The interval with the highest average number of steps is **835**, with the averge number of steps of **206.17**

#Imputing missing values

Calculate the total number of missing values

```r
cnt_missing <- nrow(data[is.na(data$steps),])
```

The total number of missing values is **2304**

Impute the missing values: Use the interval median and create a new dataset.

```r
data_imputed <- merge(data, interval_avg, by.x="interval", by.y="interval")
data_imputed$steps <- ifelse( is.na(data_imputed$steps),data_imputed$mean,data_imputed$steps   )
```

Calculate the total number of steps taken each day from new dataset

```r
steps_total_imp <- data_imputed %>%
    group_by(date) %>%
    summarise(total_steps =sum (steps))
```

Display the histogram of steps taken each day

```r
h2 <- ggplot(data= steps_total_imp, aes(total_steps))+geom_histogram(color="black", fill="darkgreen")+
  ylab("Number of occurences")+
  xlab("Total steps taken on a day")+
  ggtitle("Histogram of total steps taken \n on a day with imputed values.")+
  theme_bw()

grid.arrange(h1, h2, nrow=1, ncol=2)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

Calculate the mean and median of the total number of steps taken per day with imputed values.

```r
steps_mean_imputed <- steps_total_imp %>%
    summarise(mean_steps =mean (total_steps),median_steps =median (total_steps) )
mean_imp <- steps_mean_imputed$mean_steps
median_imp <- steps_mean_imputed$median_steps
```

The mean value of total number of steps taken per day is **10766.19** and the median value is **10766.19**.
The difference in mean of the original and imputed data is **1411.96** and the difference in median is **371.19**

#Are there differences in activity patterns between weekdays and weekends?

Create a factor variable indicating if the day is weekend or weekday.

```r
data_imputed$weekend <- isWeekday(data_imputed$date)
data_imputed$weekend<- as.factor(ifelse(data_imputed$weekend, "weekend", "weekday"))
```

Calculate the the average number of steps taken in interval averaged across weekdays or weekends.

```r
avg_week <- data_imputed %>%
    group_by(weekend, interval) %>%
    summarise(average =mean (steps))
```

Save weekends and weekdays separately


```r
avg_weekdays <-avg_week[avg_week$weekend=="weekday",]
avg_weekend <-avg_week[avg_week$weekend=="weekend",]
```

Display the histogram of steps taken each day

```r
aw1   <- ggplot(data= avg_weekend, aes( interval, average))+geom_line(color="darkgreen", size=1.2)+
  ylab("Average steps taken")+
  xlab("Interval")+
  ggtitle("Average steps taken during \n all intervals at weekends.")+
  theme_bw()

aw2   <- ggplot(data= avg_weekdays, aes( interval, average))+geom_line(color="darkblue", size=1.2)+
  ylab("Average steps taken")+
  xlab("Interval")+
  ggtitle("Average steps taken during \n all intervals at weekdays.")+
  theme_bw()

grid.arrange(aw1, aw2, nrow=1, ncol=2)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)
