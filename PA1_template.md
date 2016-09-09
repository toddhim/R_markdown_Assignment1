#Personal Activity Monitoring
Todd Himple
Date: September 5, 2016


#Inroduction
The following markdown is assignment #1 of the "Data Science" course "Reproducible Research"
from Coursera. The assignment is to look at personal data (step count) from activity 
monitoring devices. The devices collect data in 5 min. intervals during the months of October
and November 2012.  The assignment instructs the students to write a single R markdown document
processed by knitr and transformed into an HTML file.



##Downloading and cleaning dataset
The following code performs the following functions:

 * Loads the knitr package
 * Downloads dataset from source and unzips
 * Captures download date
 * Cleans data by assigning the date field to Date class


```r
library(knitr)
##opts_knit$set(base.dir = 'docs') # Change the base dir where to save figures
##knit2html("./docs/report.Rmd", "./docs/report.html")


fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
download.file(fileUrl, destfile = "Dataset.zip") 
dateDownloaded <- date() #capturing download date
dateDownloaded
```

```
## [1] "Tue Sep 06 22:18:55 2016"
```

```r
##unzipping dataset and reading data into R 
unzip("Dataset.zip") 

Act_mon_data <- read.csv("activity.csv")

##Convert the date into date class
Act_mon_data$date<-as.Date(Act_mon_data$date, "%Y-%m-%d")
```



##Determining mean number of steps each day
The following code Calculates total # of steps/day; showing visual in a histogram


```r
daily_steps <- aggregate(steps~date, Act_mon_data, sum, na.rm = TRUE)

barplot(daily_steps$steps, names.arg = daily_steps$date, 
        main = "Total number of steps per day", xlab = "Date", ylab = "Steps/day")
```

![plot of chunk graphing the mean steps each day](figure/graphing the mean steps each day-1.png)

The following returns the mean & median of the total # of steps taken per day

Mean:

```r
mean_steps <- mean(daily_steps$steps, na.rm = TRUE)
print(mean_steps)
```

```
## [1] 10766.19
```

Median:

```r
median_steps <- median(daily_steps$steps, na.rm = TRUE)
print(median_steps)
```

```
## [1] 10765
```



##Determining the average daily activity pattern
The following code produces:

 * A time series plot of the 5-minute interval & average number of steps taken across all days
 * A calculation of the 5-min intervial that averages the the max # of steps



```r
interval_steps <- aggregate(steps~interval, data = Act_mon_data, FUN = mean)
plot(interval_steps , type = "l",
     main = "Ave. number of steps per day",
     xlab="5-min. intervals",
     ylab="Ave. steps")
abline(v=interval_steps$interval[which.max(interval_steps$steps)], untf = TRUE)
```

![plot of chunk average daily activity](figure/average daily activity-1.png)

```r
interval_steps$interval[which.max(interval_steps$steps)]
```

```
## [1] 835
```



##Calculating and imputing missing values to see if change to the daily activity pattern
The code below produces the following:

 * Count of missing values
 * A new dataset with missing values imputed (taking the average of the 5-min interval)
 * Histogram of the total # of steps taken/day and Calculate &  the mean/median steps.



```r
sum(is.na(Act_mon_data))
```

```
## [1] 2304
```

```r
#[1] 2304

Act_mon_data2 <- Act_mon_data #making copy of dataset
NAs <- is.na(Act_mon_data2$steps)
avg_interval <- tapply(Act_mon_data2$steps, Act_mon_data2$interval, mean, na.rm=TRUE, simplify=TRUE) #average by interval
Act_mon_data2$steps[NAs] <- avg_interval[as.character(Act_mon_data2$interval[NAs])]

daily_steps_noNA <- aggregate(steps~date, Act_mon_data2, sum)

barplot(daily_steps_noNA$steps, names.arg = daily_steps_noNA$date, 
        main = "Total steps/day; missing values averaged", xlab = "Date", ylab = "Steps/day")
```

![plot of chunk missing values](figure/missing values-1.png)

The following returns the revised mean & median with missing values imputed

Mean:

```r
mean_steps_noNA <- mean(daily_steps$steps)
print(mean_steps_noNA)
```

```
## [1] 10766.19
```

Median:

```r
median_steps_noNA <- median(daily_steps$steps)
print(median_steps_noNA)
```

```
## [1] 10765
```

**Conclusion is that there is no difference in the mean and median values**



##Comparing weekdays to weekends
The following seeks to determine if differences in patterns between weekdays and weekends

        * Creating factor variable w/ two levels - "weekday" & "weekend"
        * Panel plot to see any differences between weekday and weekend activities


```r
daytype_split <- function(date) 
{
        if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {"weekend"} 
        else {"weekday"}
}

Act_mon_data$daytype <- as.factor(sapply(Act_mon_data$date, daytype_split))

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
        steps.type <- aggregate(steps ~ interval, data = Act_mon_data, subset = Act_mon_data$daytype == type, 
                                FUN = mean, na.rm=TRUE)
        plot(steps.type, type = "l", 
             ylim = c(0, 200),
             main = type,
             xlab="5-min. intervals",
             ylab="Ave. steps")
}
```

![plot of chunk weekends/weekdays](figure/weekends/weekdays-1.png)

**When comparing activity on weekends vs weekdays, it appears to be a greater spike in the early hours on**
**weekdays, and then tapers off.  However, there's more activity throughout the rest of the day on weekends**
