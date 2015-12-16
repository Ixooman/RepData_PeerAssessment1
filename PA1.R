# Code for preparation and analysis of data
# =========== DRAFT FOR REPORT ============

library(dplyr)
library(ggplot2)

df <- read.csv("activity.csv")
df$date <- as.Date(df$date)

tmp <- summarize(group_by(df, date), s=sum(steps, na.rm = TRUE))
ggp <- ggplot(tmp, aes(x = date, y = s))
ggp <- ggp + geom_bar(fill="gray", color=NA, stat = "identity")
ggp <- ggp + theme_bw()
ggp <- ggp + labs(title="Total number of steps taken per day", x="Date", y="Steps")
ggp

steps.mean <- mean(tmp$s)
steps.median <- median(tmp$s)
