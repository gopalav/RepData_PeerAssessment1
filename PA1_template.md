# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzipData <- unz('activity.zip', 'activity.csv')
activityData <- read.table(unzipData, header = TRUE, sep = ',')
```


## What is mean total number of steps taken per day?




## What is the average daily activity pattern?




## Imputing missing values




## Are there differences in activity patterns between weekdays and weekends?

