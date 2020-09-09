#download file
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
              , destfile = 'activity.zip')
if (!file.exists('activity.csv')){
        unzip('activity.zip')
}

raw_data <- read.csv('activity.csv', header = TRUE)
data <- na.omit(raw_data)
str(data)

#Calculate total number of steps taken each day
library(ggplot2)
steps_day <- tapply(data$steps, data$date, FUN = sum)
qplot(steps_day, bins = 30, xlab = 'Steps per day'
      , main = 'Total number of steps taken each day')
mean_steps <- mean(steps_day)
median_steps <- median(steps_day)

#Average daily activity pattern

average_day <- aggregate(list(steps = data$steps), by = list(interval = data$interval)
                         , FUN = mean)
ggplot(data = average_day, aes(x = interval, y = steps))+geom_line()+
        xlab('5-min interval')+ylab('avg. number of steps taken')

average_day[which.max(average_day$steps),]

#Missing values
missing <- length(which(is.na(raw_data$steps)))
fill <- function(steps, interval){
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else filled <- (average_day[average_day$interval == interval, 'steps'])
        return(filled)
}
fill_data <- raw_data
fill_data$steps <- mapply(fill, fill_data$steps, fill_data$interval)

fill_steps_day <- tapply(fill_data$steps, fill_data$date, FUN = sum)
qplot(steps_day, bins = 30, xlab = 'Steps per day',
      main = 'Total number of steps taken each day ')

mean_fill_steps <- mean(fill_steps_day)
median_fill_steps <- median(fill_steps_day)

#Diff in activity patterns between weekdays and weekends
fill_data$date <- as.Date(fill_data$date)
fill_data$weekday <- weekdays(fill_data$date)
fill_data$day_type <- ifelse(fill_data$weekday == 'Saturday' | fill_data$weekday == 'Sunday',
                             'Weekend', 'Weekday')
fill_data$day_type <- factor(fill_data$day_type)
day_type_data <- aggregate(steps ~ interval + day_type, data = fill_data, mean)
ggplot(day_type_data, aes(interval, steps))+geom_line()+facet_grid(day_type ~ .)+
        xlab('5-min interval')+ylab('Average number of steps taken')+
        ggtitle('Weekdays and weekends activity patterns')
