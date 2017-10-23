---
title: "Assignment week 2"
author: "Laurent Dreveton-Amzalac"
date: "23 octobre 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Assignment week 2, Reproducible Research from JHU on Coursera
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### 1. Code for reading in the dataset and/or processing the data
The data for this assignment can be downloaded from the course web site: <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.ziphttp://rmarkdown.rstudio.com>[link](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.ziphttp://rmarkdown.rstudio.com).

```{r setoptions,echo=TRUE}
activity <- read.csv("activity.csv")
dim(activity)
```

create new dataset without na values
```{r}
nactivity <- na.omit(activity)
meanTotalSteps <- aggregate(nactivity$steps, list(nactivity$date), sum)
names(meanTotalSteps) <- c("date", "steps")
meanTotalSteps
```

### 2. Histogram of the total number of steps taken each day, mean line in magenta, median line in blue, median=mean in our example
```{r}
lmts <- nrow(meanTotalSteps)
hist(meanTotalSteps$steps, breaks = lmts)
rug(meanTotalSteps$steps)
abline(v = mean(meanTotalSteps$steps), col = "magenta", lwd = 4)
abline(v = median(meanTotalSteps$steps), col = "blue", lwd = 2)
```

### 3. Mean and median number of steps taken each day

```{r}
mean(meanTotalSteps$steps)
```
```{r}
median(meanTotalSteps$steps)
```

### 4. Time series plot of the average number of steps taken

```{r}
library(plyr)
#calculate average steps for each of 5-minute inteval during a 24-hour period
int.mean.steps <- ddply(nactivity, ~interval, summarise, mean = mean(steps))
```
```{r tidy=FALSE}
library(ggplot2)
ggplot(data = int.mean.steps, aes(x=interval, y=mean)) +
    geom_line() + 
    labs(title = "Average Number of Steps Across All Days", 
         subtitle = "Covering only five states and five diagnosis", 
         x="5 minutes inteval", y="Number of Step Count")
```

### 5. The 5-minute interval that, on average, contains the maximum number of steps

```{r}
int.mean.steps[which.max(int.mean.steps$mean),]
```

**Observations**

Based on steps taken pattern, the person's daily activity peaks around 8:35am

### 6. Code to describe and show a strategy for imputing missing data

Calculate and report the total number of missing values in the dataset

```{r}
library(sqldf)
```

```{r tidy=FALSE}
tNA <- sqldf('SELECT d.* 
        FROM "activity" as d
        WHERE d.steps IS NULL 
        ORDER BY d.date, d.interval')
```

```{r}
nrow(tNA)
```
Filling the missing values

```{r tidy=FALSE}
t1 <- sqldf('
        SELECT d.*, i.mean 
        FROM "int.mean.steps" as i 
        JOIN "activity" as d 
        ON d.interval = i.interval 
        ORDER BY d.date, d.interval ')

t1$steps[is.na(t1$steps)] <- t1$mean[is.na(t1$steps)]
```

### 7. Histogram of the total number of steps taken each day after missing values are imputed

```{r tidy=FALSE}
t1.total.steps <- as.integer( sqldf('
        SELECT sum(steps) 
        FROM t1') )
        
t1.total.steps.by.date <- sqldf('
        SELECT date, sum(steps) as "t1.total.steps.by.date" 
        FROM t1 GROUP BY date 
        ORDER BY date')
        
daily.61.steps <- sqldf('
        SELECT date, "t1.total.steps.by.date" as "steps" 
        FROM "t1.total.steps.by.date" 
        ORDER BY date')
```

Make an histogram of the total number of steps every day

```{r tidy=FALSE}
hist(daily.61.steps$steps,
        main=" ", 
        breaks=10, 
        xlab="After Imputate NA -Total Number Of Steps Taken Daily")
```

Calculate and report the mean and median total number of steps taken per day

```{r}
t1.mean.steps.per.day <- as.integer(t1.total.steps / NROW(t1.total.steps.by.date))
t1.mean.steps.per.day
```

```{r}
t1.median.steps.per.day <- median(t1.total.steps.by.date$t1.total.steps.by.date)
t1.median.steps.per.day
```
**Observation**

Do these values (mean and median) differ from the estimates from the first part of the assignment? Not really

What is the impact of imputing missing value on the estimates of the total daily number of steps? The shape of the histogram remains the same as the histogram from removed missing values. However, the frequency counts increased as expected.

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r tidy=FALSE}
t1$date <- as.Date(t1$date)
t1$weektime <- as.factor(ifelse(weekdays(t1$date) %in%
                    c("samedi", "dimanche"), "weekend", "weekday"))
        
t5 <- sqldf('
        SELECT interval, avg(steps) as "mean.steps", weektime             
        FROM t1 
        GROUP BY weektime, interval 
        ORDER BY interval')
```

```{r plot1, tidy=FALSE}
library(dplyr)
lines <- t5 %>%
    group_by(weektime) %>%
    summarise(
        x = mean(mean.steps),
        ymin = min(interval),
        ymax = max(interval),
        
        #2
        y = mean(interval),
        ymin2 = min(mean.steps),
        ymax2 = max(mean.steps)
    )

ggplot(data = t5, 
       aes(x = interval, y = mean.steps, colour = factor(weektime)))  + 
    geom_line(alpha = 0.8) + 
    labs(title = "Panel plot comparing the average number of steps taken per 5-minute interval", 
         subtitle = "across weekdays and weekends") + 
    geom_linerange(aes(x=y, y=NULL, ymin=ymin2, ymax=ymax2), 
                   data=lines, linetype="dashed") + 
    facet_grid(factor(weektime) ~ .)
```

```{r plot2}
ggplot(data = t5, 
       aes(x = mean.steps, y = interval, colour = factor(weektime)))  + 
    geom_line(alpha = 0.8) + 
    labs(title = "Panel plot comparing the average number of steps taken per 5-minute interval", 
         subtitle = "across weekdays and weekends") + 
    geom_hline(aes(yintercept = mean(interval))) +
    geom_linerange(aes(x=x, y=NULL, ymin=ymin, ymax=ymax), 
                   data=lines, linetype="dashed")
```

```{r plot3}
ggplot(data = t5, 
       aes(x = interval, y = mean.steps, colour = factor(weektime)))  + 
    geom_line(alpha = 0.8) + 
    labs(title = "Panel plot comparing the average number of steps taken per 5-minute interval", 
         subtitle = "across weekdays and weekends") + 
    geom_hline(aes(yintercept = mean(mean.steps))) +
    geom_linerange(aes(x=y, y=NULL, ymin=ymin2, ymax=ymax2), 
                   data=lines, linetype="dashed")


```


