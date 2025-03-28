---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


``` r
if (!requireNamespace("readr", quietly = TRUE)) {
    install.packages("readr")
}

library(readr)

unzip("activity.zip")

data <- read_csv("activity.csv")
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


## What is mean total number of steps taken per day?

#### Calculate the total number of steps taken per day?
##### This will result to an array with total sum of steps taken per day

``` r
steps_per_day <- with(data, tapply(steps, date, sum))
barplot(unname(steps_per_day),     names.arg = names(steps_per_day) )
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Make a histogram of the total number of steps taken each day


``` r
hist(steps_per_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### Calculate and report the mean and median of the total number of steps taken per day

``` r
summary(steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

``` r
#mean_steps_per_day <- with(data, tapply(steps, date, mean))
#barplot(unname(mean_steps_per_day),  names.arg = names(mean_steps_per_day) )
```

## What is the average daily activity pattern?
#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


``` r
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
}

library(ggplot2)

mean_by_interval <- aggregate(steps ~ interval, data = data, FUN = mean)

ggplot(mean_by_interval, aes(x = interval, y = steps, group = 1)) +
geom_line() +
labs(title = "Mean Value by Interval", x = "Interval", y = "Mean Value") +
theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
### Answer is interval number 835, with maximum average steps of 206.17 



``` r
## Answer is interval number 835, with maximum average steps of 206.17
subset(mean_by_interval,steps == max(mean_by_interval$steps) )
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with  NAs)
### There are 2304 null values

``` r
sum(is.na(data$steps))
```

```
## [1] 2304
```

#### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


``` r
if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
}

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
## We use the mean of interval to fill the value of nulls 
data_imputed <- data %>%
   group_by(interval) %>%
   mutate(steps  = ifelse(is.na(steps ), mean(steps , na.rm = TRUE), steps ))

 
head(data_imputed )
```

```
## # A tibble: 6 × 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <date>        <dbl>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

#### Create a new dataset that is equal to the original dataset but with the missing data filled in


``` r
### Same code as on top
```


#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

###### this is the report of mean and median and other report with nulls

``` r
summary(steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

``` r
hist(steps_per_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

###### this is the report of mean and median and other report IMPUTED

``` r
steps_per_day_imputed <- with(data_imputed, tapply(steps, date, sum))
summary(steps_per_day_imputed)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

``` r
hist(steps_per_day_imputed)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

``` r
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
#Answers: They are about the same
```
## Are there differences in activity patterns between weekdays and weekends? NOT REALLY
#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


``` r
data_imputed$day_type <- ifelse(weekdays(data_imputed$date) %in% c("Saturday", "Sunday"), "Weekends", "Weekdays")
```

#### Make a panel plot containing a time series plot(i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using

``` r
head(data_imputed)
```

```
## # A tibble: 6 × 4
## # Groups:   interval [6]
##    steps date       interval day_type
##    <dbl> <date>        <dbl> <chr>   
## 1 1.72   2012-10-01        0 Weekdays
## 2 0.340  2012-10-01        5 Weekdays
## 3 0.132  2012-10-01       10 Weekdays
## 4 0.151  2012-10-01       15 Weekdays
## 5 0.0755 2012-10-01       20 Weekdays
## 6 2.09   2012-10-01       25 Weekdays
```






``` r
data_summary <- data_imputed %>%
    group_by(day_type, interval) %>%
    summarise(mean_value = mean(steps))
```

```
## `summarise()` has grouped output by 'day_type'. You can override using the
## `.groups` argument.
```

``` r
ggplot(data_summary, aes(x = interval, y = mean_value, color = day_type)) +
facet_wrap(~ day_type, scales = "free_y") +
ylim(0, 210) +
geom_line() +
labs(title = "Average Steps by 5 mins Interval by day type",
        x = "Interval",
        y = "Mean Value") +
theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
