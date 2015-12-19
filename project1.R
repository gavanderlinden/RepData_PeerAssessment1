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

# replace na with zeros
df[is.na(df)] <- 0

# time series plot
qplot(dateTime, steps, data=df, geom="line")
