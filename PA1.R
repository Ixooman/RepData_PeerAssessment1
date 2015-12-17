# Code for preparation and analysis of the data

# ===== Loading and preprocessing the data =====

library(dplyr)
library(ggplot2)

df <- read.csv("activity.csv")
df$date <- as.Date(df$date)
df <- tbl_df(df)

# ===== Mean and median total number of steps per day =====

tmp <- summarize(group_by(df, date), s=sum(steps, na.rm = TRUE))
days <- length(tmp$date)
ggp <- ggplot(tmp, aes(x = date, y = s))
ggp <- ggp + geom_bar(fill="gray", color=NA, stat = "identity")
ggp <- ggp + theme_bw()
ggp <- ggp + labs(title="Total number of steps taken per day", x="Date", y="Steps")
ggp

steps.mean <- mean(tmp$s)
steps.median <- median(tmp$s)

# ===== Average daily activity pattern =====

tmp <- summarize(group_by(df, interval), avg=mean(steps, na.rm = TRUE))
ggp <- ggplot(tmp, aes(x = interval, y = avg))
ggp <- ggp + geom_line(color="blue")
ggp <- ggp + theme_bw()
ggp <- ggp + labs(title="The average daily activity pattern", x="Intervals", y="Steps (average value)")
ggp

int.max <- tmp$interval[which.max(tmp$avg)]

# ===== Imputing missing values =====

missing.rows <- is.na(df$steps)
missing.count <- sum(missing.rows)
missing.rows <- which(missing.rows)
tmp <- summarize(group_by(df, interval), med=median(steps, na.rm = TRUE))
df.imp <- cbind(df, rep(tmp$med, days))
names(df.imp) <- c(names(df), "med")
df.imp$steps[missing.rows] <- df.imp$med[missing.rows]
df.imp <- select(df.imp, -med)

tmp <- summarize(group_by(df.imp, date), s=sum(steps, na.rm = TRUE))
ggp <- ggplot(tmp, aes(x = date, y = s/1000))
ggp <- ggp + geom_histogram(fill="gray", color=NA, stat = "identity")
ggp <- ggp + theme_bw()
ggp <- ggp + labs(title="Total number of steps taken per day (with imputed values)", x="Date", y="Steps (thousands)")
ggp

steps.mean.imp <- mean(tmp$s)
steps.median.imp <- median(tmp$s)

# ===== Differences in activity patterns =====
we <- c('Sunday', 'Saturday')
days <- factor(c('weekday', 'weekend')[(weekdays(df.imp$date) %in% we)+1L])
days.palette <- c('blue','red')
df.imp <- cbind(df.imp, days)

tmp <- summarize(group_by(df.imp, days, interval), avg=mean(steps, na.rm = TRUE))
ggp <- ggplot(tmp, aes(x = interval, y = avg, colour = days))
ggp <- ggp + geom_line()
ggp <- ggp + scale_colour_manual(values=days.palette) 
ggp <- ggp + theme_bw()
ggp <- ggp + labs(title="The average daily activity patterns", x="Intervals", y="Steps (average value)")
ggp <- ggp + facet_wrap( ~ days, nrow = 2, ncol = 1)
ggp

