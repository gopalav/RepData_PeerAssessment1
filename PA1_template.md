# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# load the required data table library
library (data.table)
# unzip the data file and read the data
unzipData <- unz('activity.zip', 'activity.csv')
activityData <- read.table(unzipData, header = TRUE, sep = ',')
# convert the data into data table
activityData <- data.table(activityData)
# process the data to create date frame with the total steps group by date
dataSummary <- as.data.frame(activityData[, j=list(total = sum(steps, na.rm = TRUE)),by = date])
```


## What is mean total number of steps taken per day?

```r
#calculate the mean and median of the total steps per day
meanSteps <- mean(dataSummary$total)
medianSteps <- median(dataSummary$total)
# histogram of total steps per day
hist(dataSummary$total, xlab='Total Steps', ylab='Frequency', main='Histogram of total steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

- **Mean total number of steps taken per day = 9354.2295082**
- **Median total number of steps taken per day = 10395**

## What is the average daily activity pattern?

```r
# process the original data to create date frame with the total steps group by interval and the mean
dataSummary1 <- as.data.frame(activityData[, j=list(total = sum(steps, na.rm = TRUE), mean = mean(steps, na.rm = TRUE)),by = interval])

# Calculate the interval which has the maximum number of steps
maxMean <-(max(dataSummary1$mean, na.rm=TRUE))
maxInterval <-with(dataSummary1, interval[mean == maxMean])
maxInterval <- sprintf("%04d", maxInterval)
maxInterval <- format(strptime(maxInterval, format="%H%M"), format = "%H:%M")

# plot the time series for the interval and mean
plot(dataSummary1$interval, dataSummary1$mean, type='l', xlab = 'Interval', ylab = 'Average steps', main = 'Daily Activity Pattern')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

- **Interval which has the maximum number of steps = 08:35**

## Inputing missing values




## Are there differences in activity patterns between weekdays and weekends?

