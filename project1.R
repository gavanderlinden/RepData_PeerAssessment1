# load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# load data
df <- read.csv("activity.csv", stringsAsFactors = FALSE)
df$hours <- as.character(df$interval %/% 100)
df$minutes <- as.character(df$interval %% 100)
df <- df %>%
    mutate(dateTime = ymd_hm(paste(date, hours, minutes, sep=" ")))


# group steps by days
dfDays <- df %>%
    na.omit() %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

# plot a histogram of total number of steps per day
qplot(dfDays$total_steps, geom="histogram")

# calculate mean and median of total number of steps per day
meanSteps = mean(dfDays$total_steps)
medianSteps = median(dfDays$total_steps)

# group by interval
dfInterval <- df %>%
    na.omit() %>%
    group_by(interval) %>%
    summarise(total_steps = sum(steps))

# time plot of total number of steps per interval
qplot(interval, total_steps, data=dfInterval, geom="line")

# max interval
mostActiveInterval <- dfInterval[which.max(dfInterval$total_steps),]

# number of missing values
numMissingValues <- sum(is.na(df$steps))

# calculate averages
stepsNoNA <- df$steps
stepsNoNA[is.na(stepsNoNA)] <- 0
dfNoNA <- transform(
    df, meanStepsInterval = ave(
        stepsNoNA, interval, FUN = mean, na.rm=T
    )
)
dfNoNA$steps[is.na(dfNoNA$steps)] <- dfNoNA$meanSteps[is.na(dfNoNA$steps)]

# group steps by days
dfDaysNoNA <- dfNoNA %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

# plot a histogram of total number of steps per day
qplot(dfDaysNoNA$total_steps, geom="histogram")

# calculate mean and median of total number of steps per day
meanStepsNoNA = mean(dfDaysNoNA$total_steps)
medianStepsNoNA = median(dfDaysNoNA$total_steps)

# calculate if date is a weekday
dfNoNA <- dfNoNA %>%
    mutate(dayType = as.factor(ifelse(wday(dfNoNA$dateTime, label=T) %in% c("Sat", "Sun"), "weekday", "weekend")))

# group by interval
dfIntervalNoNA <- dfNoNA %>%
  group_by(interval, dayType) %>%
  summarise(total_steps = sum(steps))

# time plot of total number of steps per interval
qplot(interval, total_steps, data=dfIntervalNoNA, facets = .~dayType, geom="line")
