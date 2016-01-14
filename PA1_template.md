---
title: "Reproducible Research: Course Project 1"
output: html_document
---


## Loading libraries
```{r libs, message=FALSE}
library(readr)
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data
```{r}
activity.data <- read_csv(unz("activity.zip", "activity.csv"), col_type = "iDi")
str(activity.data)
```

## What is mean total number of steps taken per day?

```{r}
steps_per_day <-
  activity.data %>% 
  group_by(date) %>%
  summarise(
    steps_sum = sum(steps, na.rm = TRUE)
  )

steps_per_day_mean <- mean(steps_per_day$steps_sum)
steps_per_day_median <- median(steps_per_day$steps_sum)

ggplot(steps_per_day, aes(x = steps_sum)) +
  geom_histogram(binwidth = 2000) +
  geom_vline(aes(xintercept = steps_per_day_mean), color = "red", linetype = "dotdash") +
  geom_vline(aes(xintercept = steps_per_day_median), color = "lightgreen", linetype = "dotdash")
```

Report the mean and median of the total number of steps taken per day:
```{r}
steps_per_day_mean
steps_per_day_median
```


## What is the average daily activity pattern?

```{r}
steps_by_interval <-
  group_by(activity.data, interval) %>%
  summarise(
    steps_mean = mean(steps, na.rm = TRUE)
  )

interval_max <- filter(steps_by_interval, steps_mean == max(steps_mean))

ggplot(steps_by_interval, aes(x = interval, y = steps_mean)) +
  geom_line() +
  geom_text(aes(x = interval_max$interval+100, y = interval_max$steps_mean, 
                label = interval_max$interval), size = 4) +
  geom_vline(aes(xintercept = interval_max$interval), color = "red", linetype = "dotdash") +
  labs(x = "Intervals", y = "Mean steps") 
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
interval_max$interval
```


## Imputing missing values

Calculate and report the total number of missing values in the dataset:
```{r}
nrow(filter(activity.data, is.na(steps)))
```

Create a new dataset that is equal to the original dataset but with the missing data filled in:
```{r}
activity.data2 <- left_join(activity.data, steps_by_interval, by = "interval")

activity.data2 <-
  activity.data2 %>%
  mutate(steps = ifelse(is.na(steps), steps_mean, steps)) %>%
  select(-steps_mean)

steps_per_day_new <-
  activity.data2 %>%
  group_by(date) %>%
  summarise(
    steps_sum = sum(steps, na.rm = TRUE)
  )

steps_per_day_mean2 <- mean(steps_per_day_new$steps_sum)
steps_per_day_median2 <- median(steps_per_day_new$steps_sum)

ggplot(steps_per_day_new, aes(x = steps_sum)) +
  geom_histogram(binwidth = 2000) +
  geom_vline(aes(xintercept = steps_per_day_mean2), color = "red", linetype = "dotdash") +
  geom_vline(aes(xintercept = steps_per_day_median2), color = "lightgreen", linetype = "dotdash")
```

The impact of imputation: 
```{r}
steps_per_day_mean2
steps_per_day_median2
```


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:
```{r}
activity.data2 <- 
  activity.data2 %>%
  mutate(day = ifelse(is.element(weekdays(date), c("Saturday", "Sunday")), "weekend", "weekday"))
```

Make a panel plot containing a time series plot:
```{r}

steps_by_interval_new <-
  activity.data2 %>% 
  group_by(day, interval) %>%
  summarise(
    steps_mean = mean(steps)
  )


ggplot(steps_by_interval_new, aes(x = interval, y = steps_mean)) +
  geom_line() +
  facet_wrap( ~ day, ncol = 1) +
  labs(x = "Intervals", y = "Mean steps") 

```
title: "Untitled"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
