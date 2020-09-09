# Reproducible Research: Course Project 1
========================

## Loading and Preprocessing Data

```{r, echo = TRUE}
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
              , destfile = 'activity.zip')
if (!file.exists('activity.csv')){
        unzip('activity.zip')
}

raw_data <- read.csv('activity.csv', header = TRUE)
data <- na.omit(raw_data)
str(data)
```

## What is mean total number of steps taken per day?

```{r, echo = TRUE}
library(ggplot2)
steps_day <- tapply(data$steps, data$date, FUN = sum)
qplot(steps_day, bins = 30, xlab = 'Steps per day'
      , main = 'Total number of steps taken each day')
mean_steps <- mean(steps_day)
median_steps <- median(steps_day)
```

## What is the average daily activity pattern?
```{r , echo = TRUE}
average_day <- aggregate(list(steps = data$steps), by = list(interval = data$interval)
                         , FUN = mean)
ggplot(data = average_day, aes(x = interval, y = steps))+geom_line()+
        xlab('5-min interval')+ylab('avg. number of steps taken')
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo = TRUE}
average_day[which.max(average_day$steps),]
```

## Imputing missing values
Total number of missing values:
```{r, echo = TRUE}
missing <- length(which(is.na(raw_data$steps)))
```

All of the missing values being replaced with the 5 minute mean value.
```{r, echo = TRUE}
fill <- function(steps, interval){
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else filled <- (average_day[average_day$interval == interval, 'steps'])
        return(filled)
}
fill_data <- raw_data
fill_data$steps <- mapply(fill, fill_data$steps, fill_data$interval)
```

```{r, echo = TRUE}
fill_steps_day <- tapply(fill_data$steps, fill_data$date, FUN = sum)
qplot(steps_day, bins = 30, xlab = 'Steps per day',
      main = 'Total number of steps taken each day ')

mean_fill_steps <- mean(fill_steps_day)
median_fill_steps <- median(fill_steps_day)
```

## Are there differences in activity patterns between weekdays and weekends?
```{r, echo = TRUE}
fill_data$date <- as.Date(fill_data$date)
fill_data$weekday <- weekdays(fill_data$date)
fill_data$day_type <- ifelse(fill_data$weekday == 'Saturday' | fill_data$weekday == 'Sunday',
                             'Weekend', 'Weekday')
```

```{r, echo = TRUE}
fill_data$day_type <- factor(fill_data$day_type)
day_type_data <- aggregate(steps ~ interval + day_type, data = fill_data, mean)
ggplot(day_type_data, aes(interval, steps))+geom_line()+facet_grid(day_type ~ .)+
        xlab('5-min interval')+ylab('Average number of steps taken')+
        ggtitle('Weekdays and weekends activity patterns')
```