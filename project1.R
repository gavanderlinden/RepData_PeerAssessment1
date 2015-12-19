# load libraries
library(dplyr)

# load data
df <- read.csv("activity.csv")

dfDays <- df %>%
    na.omit() %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))
