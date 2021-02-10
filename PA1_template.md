---
title: "Reproducible Research - Project 1"
output: html_document
---

### 1. Loading and preprocessing the data

1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r reading, echo = TRUE}
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)
str(activity)
summary(activity)
```

### 2. What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day
Histogram Plot
```{r hist}
stepsperday <- aggregate(steps ~ date, data = activity, sum)

library(ggplot2)
ggplot(stepsperday, aes(steps)) + geom_histogram(binwidth = 2000) +
    xlab("Total number of steps taken each day") + 
    ylab("Frequency")
```
```{r Mean&Median}
mean(stepsperday$steps,na.rm = T)
median(stepsperday$steps,na.rm = T)
```

### What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r daily_pattern}
meanperinterval <- aggregate(steps ~ interval, data = activity, mean)
ggplot(data=meanperinterval, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute intervals") +
    ylab("Average number of steps taken")
```
```{r interval_with_max_steps}
max(meanperinterval$steps)
meanperinterval$interval[which.max(meanperinterval$steps)]
```

### 4.Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r filling}
sum(is.na(activity$steps))
activity2 <- activity
activity2$steps[is.na(activity2$steps)] <- mean(activity2$steps, na.rm = T)
```
```{r hist2}
stepsperday2 <- aggregate(steps ~ date, data = activity2, sum)

ggplot(stepsperday2, aes(steps)) + geom_histogram(binwidth = 2000) +
    xlab("Total number of steps taken each day") + 
    ylab("Frequency")
```

### 5. Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r timeseries}
activity3 <- activity2
activity3$date <- as.Date(activity3$date, format = '%Y-%m-%d')
activity3$day <- ifelse(weekdays(as.Date(activity3$date)) == "Saturday" | weekdays(as.Date(activity3$date)) == "Sunday", "weekend", "weekday")
activity3$day <- factor(activity3$day)

lastcall <- aggregate(steps ~ interval + day,data=activity3, FUN = mean, na.action = na.omit)
g <- ggplot(lastcall, aes(interval, steps))
g + geom_line() + facet_grid(day~.) + ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")

```



