Reproducible Research: Peer Assessment 1
================

Loading and preprocessing the data
----------------------------------

Load the data (i.e. read.csv()).

The data is stored in a single csv file zipped up in "activity.zip".

``` r
mydata <- read.csv(unz("activity.zip","activity.csv"),header=T, quote="\"", sep=",")
head(mydata,5)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20

Process/transform the data (if necessary) into a format suitable for your analysis. Convert the date variable into dates.

``` r
mydata$date <- as.Date(mydata$date)
```

What is mean total number of steps taken per day?
-------------------------------------------------

Calculate the total number of steps taken per day. Save the output into a table named mydata\_bydate so that we can use it to plot a histogram in the next step.

``` r
mydata_bydate <- aggregate(mydata$steps, by=list(mydata$date), sum, na.rm = TRUE)
colnames(mydata_bydate) <- c("date","tot.nbr.of.steps")
mydata_bydate
```

    ##          date tot.nbr.of.steps
    ## 1  2012-10-01                0
    ## 2  2012-10-02              126
    ## 3  2012-10-03            11352
    ## 4  2012-10-04            12116
    ## 5  2012-10-05            13294
    ## 6  2012-10-06            15420
    ## 7  2012-10-07            11015
    ## 8  2012-10-08                0
    ## 9  2012-10-09            12811
    ## 10 2012-10-10             9900
    ## 11 2012-10-11            10304
    ## 12 2012-10-12            17382
    ## 13 2012-10-13            12426
    ## 14 2012-10-14            15098
    ## 15 2012-10-15            10139
    ## 16 2012-10-16            15084
    ## 17 2012-10-17            13452
    ## 18 2012-10-18            10056
    ## 19 2012-10-19            11829
    ## 20 2012-10-20            10395
    ## 21 2012-10-21             8821
    ## 22 2012-10-22            13460
    ## 23 2012-10-23             8918
    ## 24 2012-10-24             8355
    ## 25 2012-10-25             2492
    ## 26 2012-10-26             6778
    ## 27 2012-10-27            10119
    ## 28 2012-10-28            11458
    ## 29 2012-10-29             5018
    ## 30 2012-10-30             9819
    ## 31 2012-10-31            15414
    ## 32 2012-11-01                0
    ## 33 2012-11-02            10600
    ## 34 2012-11-03            10571
    ## 35 2012-11-04                0
    ## 36 2012-11-05            10439
    ## 37 2012-11-06             8334
    ## 38 2012-11-07            12883
    ## 39 2012-11-08             3219
    ## 40 2012-11-09                0
    ## 41 2012-11-10                0
    ## 42 2012-11-11            12608
    ## 43 2012-11-12            10765
    ## 44 2012-11-13             7336
    ## 45 2012-11-14                0
    ## 46 2012-11-15               41
    ## 47 2012-11-16             5441
    ## 48 2012-11-17            14339
    ## 49 2012-11-18            15110
    ## 50 2012-11-19             8841
    ## 51 2012-11-20             4472
    ## 52 2012-11-21            12787
    ## 53 2012-11-22            20427
    ## 54 2012-11-23            21194
    ## 55 2012-11-24            14478
    ## 56 2012-11-25            11834
    ## 57 2012-11-26            11162
    ## 58 2012-11-27            13646
    ## 59 2012-11-28            10183
    ## 60 2012-11-29             7047
    ## 61 2012-11-30                0

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

``` r
hist(mydata_bydate$tot.nbr.of.steps, main="Histogram of Total Number of Steps Taken Each Day", xlab="Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/Plot%20histogram-1.png)

Calculate and report the mean and median of the total number of steps taken per day

``` r
mymean <- mean(mydata_bydate$tot.nbr.of.steps, na.rm = TRUE)
mymean
```

    ## [1] 9354.23

``` r
mymedian <- median(mydata_bydate$tot.nbr.of.steps, na.rm = TRUE)
mymedian
```

    ## [1] 10395

The mean number of steps taken per day is `R mymean`. The median number of steps taken per day is `R mymedian`.

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
mydata_byinterval <- aggregate(mydata$steps, by=list(mydata$interval), mean, na.rm = TRUE)
colnames(mydata_byinterval) <- c("interval","avg.nbr.of.steps")

plot(mydata_byinterval, type = "l", main="Time Series of Average Number of Steps Taken", xlab="5-minute interval", ylab = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/Time%20Series%20plot-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
mymax <- max(mydata_byinterval$avg.nbr.of.steps, na.rm = TRUE)
mymax
```

    ## [1] 206.1698

``` r
myinterval <- mydata_byinterval[mydata_byinterval$avg.nbr.of.steps == mymax, 1]
myinterval
```

    ## [1] 835

Interval `R myinterval`, on average, contains the maximum number of steps at `R mymax` steps.

Imputing missing values
-----------------------

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
myna <- sum(is.na(mydata$steps))
myna
```

    ## [1] 2304

There are `R myna` missing values in the dataset.

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Use the mean for the day to replace NA values. If the mean for the day is not available, use 0. This new dataset is named mydata\_noNA.

``` r
##  First create a summary of the mean number of steps by date
mydata_means <- aggregate(mydata$steps, by=list(mydata$date), mean, na.rm = TRUE)
colnames(mydata_means) <- c("date","avg.nbr.of.steps")

## Fill in zero if mean is not available
mydata_means[is.na(mydata_means$avg.nbr.of.steps),"avg.nbr.of.steps"] <- 0

## Merge the average back into the main dataset
mydata_noNA <- merge(mydata, mydata_means, by = "date")

## Fill in the NAs
mynas <- is.na(mydata_noNA$steps)
mydata_noNA[mynas,"steps"] <- mydata_noNA[mynas,"avg.nbr.of.steps"]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

``` r
mydata_noNA_bydate <- aggregate(mydata_noNA$steps, by=list(mydata$date), sum)
colnames(mydata_noNA_bydate) <- c("date","tot.nbr.of.steps")

hist(mydata_noNA_bydate$tot.nbr.of.steps, main="Histogram of Total Number of Steps Taken Each Day", xlab="Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/Plot%20histogram%20again-1.png)

``` r
mymean_noNA <- mean(mydata_noNA_bydate$tot.nbr.of.steps, na.rm = TRUE)
mymean_noNA
```

    ## [1] 9354.23

``` r
mymedian_noNA <- median(mydata_noNA_bydate$tot.nbr.of.steps, na.rm = TRUE)
mymedian_noNA
```

    ## [1] 10395

The new mean number of steps taken per day is `R mymean_noNA`. The new median number of steps taken per day is `R mymedian_noNA`.

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

No, because I used the mean value by date to replace the NAs. These new values are same as before. There is no impact to the estimates using this method of imputing missing data.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

``` r
is.weekend <- weekdays(mydata$date) %in% c("Saturday","Sunday")
mydata$dayofweek <- factor(c("weekday"), levels = c("weekday","weekend"))
mydata[is.weekend,"dayofweek"] <- factor(c("weekend"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
mydata_byinterval_plot <- aggregate(steps ~ interval + dayofweek, mydata, mean, na.rm = TRUE)

library(lattice)

xyplot(mydata_byinterval_plot$steps ~ mydata_byinterval_plot$interval | mydata_byinterval_plot$dayofweek, type = "l", layout = c(1,2), main="Time Series of Average Number of Steps Taken", xlab="5-minute interval", ylab = "Average Steps Taken")
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/Time%20Series%20plots-1.png)
