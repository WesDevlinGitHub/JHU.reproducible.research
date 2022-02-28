library(dplyr)
library(tidyr)
activity <- read.csv("~/repos/R_Homework/rep_res/repdata_data_activity/activity.csv")
head(activity)

activity1<- activity %>% drop_na()
# Create a new dataframe with the sum of steps per day.
activity2 <- aggregate(steps ~ date, activity1, sum)
summary(activity2)

# a Histogram of steps per day
hist(activity2$steps, xlab = 'Steps per Day',
     main = 'Histogram of Total Number of Steps per Day', breaks = 15)
# Mean and median total steps taken per day 
mean(activity2$steps)
median(activity2$steps)
# Plot total daily steps 
ggplot(activity2, aes(as.Date(date), y = as.numeric(steps), color = 'Daily Steps')) +
        geom_point()+
        geom_line() +
        geom_hline(aes(yintercept = mean(steps), color="Mean")) +
        scale_color_manual(values = c('black', 'blue')) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = 'Total Steps per Day',
             x = 'Date',
             y = 'Steps',
             color = NULL) +
        theme(legend.title = element_text(face = "bold"))
# Average number of steps by interval of day
activity3 <- aggregate(steps ~ interval, activity1, mean)
# Create a plot of the average steps acros all days in 5-Minute intervals
ggplot(activity3, aes(as.numeric(interval), y = steps, color = 'Max Steps/5 Minutes')) +
        geom_line(show.legend = TRUE) +
        geom_segment(aes(x = 835, y = 0, xend = 835, yend = max(steps)),
                     linetype="dashed", color = 'red') +
        scale_color_manual(values = c('black', 'red')) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = 'Average Steps Across All Days in 5-Minute Intervals',
             x = 'Interval',
             y = 'Average Steps per 5-Minute Interval',
             color = NULL) +
        theme(legend.title = element_text(face = "bold"))

# Here you can see that at interval 835 the max number of steps are taken.
# Calculate the total number of NA values
sum(is.na(activity$steps))
# Create a new data set and replace NA Values with interval average.
natomins <- activity
for (i in 1:nrow(natomins)) {
        if(is.na(natomins$steps[i])) {
                val <- activity3$steps[which(activity3$interval == natomins$interval[i])]
                natomins$steps[i] <- val 
        }
}
# Create a new histogram including new replaced values.

newhistdata <- aggregate(steps ~ date, natomins, sum)
hist(newhistdata$steps, xlab = 'Steps per Day',
     main = 'Histogram of Total Number of Steps per Day', breaks = 15)

# Transform Date column by weekday
natomins1 <- natomins
natomins1$date <- as.Date(natomins1$date , format = "%Y-%m-%d")
natomins1$date <-  ifelse(as.POSIXlt(natomins1$date)$wday %in% c(0,6),
                          'weekend', 'weekday')
# Make new data frame with the mean of each interval with weekend or weekdays.
avginterval <- aggregate(steps ~ interval + date, natomins1, mean)

# Create a plot which shows the 5-minute intervals per day on weekends and weekdays.
ggplot(avginterval, aes(interval, steps)) +
        geom_line() +
        facet_grid(date ~ .) +
        scale_color_manual(values = 'black') +
        scale_y_continuous(labels = scales::comma) +
        labs(title = 'Average Weekday & Weekend Steps per 5-Minute Interval ',
             x = 'Daily 5-Minute Intervals',
             y = 'Steps',
             color = NULL) +
        theme(legend.title = element_text(face = "bold"))













