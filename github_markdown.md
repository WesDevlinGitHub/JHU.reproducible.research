Daily Steps Recorded on 5-Minute Intervals for 2 Months
================
Wes Devlin
2022-02-28

## R Markdown

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

-   steps: Number of steps taking in a 5-minute interval (missing values
    are coded as NA)

-   date: The date on which the measurement was taken in YYYY-MM-DD
    format

-   interval: Identifier for the 5-minute interval in which measurement
    was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

Loading and preprocessing the data;

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(ggplot2)
activity <- read.csv("~/repos/R_Homework/rep_res/repdata_data_activity/activity.csv")
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?


``` r
activity1<- activity %>% drop_na()
# Create a new dataframe with the sum of steps per day.
activity2 <- aggregate(steps ~ date, activity1, sum)
summary(activity2)
```

    ##      date               steps      
    ##  Length:53          Min.   :   41  
    ##  Class :character   1st Qu.: 8841  
    ##  Mode  :character   Median :10765  
    ##                     Mean   :10766  
    ##                     3rd Qu.:13294  
    ##                     Max.   :21194

``` r
# a Histogram of steps per day
hist(activity2$steps, xlab = 'Steps per Day',
     main = 'Histogram of Total Number of Steps per Day', breaks = 15)
```

![plot3-1](https://user-images.githubusercontent.com/98646602/155910167-bbd7cda6-aa9c-459c-bfae-bc67cb3a6a32.png)


``` r
# Mean and median total steps taken per day 
mean(activity2$steps)
```

    ## [1] 10766.19

``` r
median(activity2$steps)
```

    ## [1] 10765

Create to gain a better understanding of the overall steps taken per
day.

``` r
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
```

![plot-1](https://user-images.githubusercontent.com/98646602/155910178-54bff6d1-0512-4f32-b0b4-d7a7b7e2b84a.png)


What is the average daily activity pattern?

``` r
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
```

![plot2-1](https://user-images.githubusercontent.com/98646602/155910190-018e86be-d2eb-4381-88e2-f9949e8a1e6f.png)


Imputing missing

-   Note that there are a number of days/intervals where there are
    missing values (coded as NA). The presence of missing days may
    introduce bias into some calculations or summaries of the data.

``` r
# Here you can see that at interval 835 the max number of steps are taken.
# Calculate the total number of NA values
sum(is.na(activity$steps))
```

    ## [1] 2304

``` r
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
```

![plot3-1](https://user-images.githubusercontent.com/98646602/155910204-ead2dacb-dbd6-492d-a93c-bc4cdf763351.png)


Are there differences in activity patterns between weekdays and
weekends?

``` r
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
```

![plot4-1](https://user-images.githubusercontent.com/98646602/155910219-5fc4f7ad-9a7a-41cc-9b3f-45e19d2c43ae.png)

