#Statistical prediction/modeling
setwd("DataScience/Reproducible Research/RepData_PeerAssessment1/")
# Load the data (Loading and preprocessing the data)
#library(timeDate)
library(dplyr)
library(ggplot2)
library(timeDate)
unzip("activity.zip")
data <- read.csv("activity.csv", header=TRUE)
# Show the first six observations
head(data)
# Total steps taken per day 
sum_per_day <- aggregate(steps~date, data=data, FUN=sum)
# Histogram of the steps per day
hist(sum_per_day$steps, breaks=20, col="red", main="Histogram of total steps", xlab="Steps", ylab="Count")

# Mean  & median of  the total number of steps taken
steps_median <- median(sum_per_day$steps)
cat("Median (steps/day) = ", steps_median)

steps_mean<-mean(sum_per_day$steps)
cat("Mean (steps/day) = ", steps_mean)
# Total steps in 5-minute intervals
interval_total <- aggregate(steps~interval, data=data, FUN=mean)
plot(ts(interval_total$steps), ylab="Mean number of steps", main="Interval (5 min) average")
interval_total[which.max(interval_total$steps), ]
# Imputing Missing values
# Number of missing values
cat("Total missing values: ",sum(is.na(data$steps)))
# Compute the mean/interval average and create a second (equal) dataset with the missing values replaced. 
data2 <- data  %>% group_by(interval) %>% mutate(steps=replace(steps, is.na(steps), mean(steps, na.rm=TRUE))) %>% data.frame()
# Compute the total number of steps taken each day.
filled_in <- aggregate(steps~date, data=data2, FUN=sum)
# Histogram of the total number of steps taken each day
hist(filled_in$steps, breaks=20, col="red", main="Total steps with average/NA replacement", xlab="Total Steps")
data2 <- data2 %>% mutate(wd=ifelse(isWeekday(x=data2$date), "weekday", "weekend")) %>% data.frame()
filled_in_total <- aggregate(steps~interval + wd, data=data2, FUN=mean)
ggplot(filled_in_total, aes(x=interval, y=steps)) + geom_line(nrow=2, ncol=0) + facet_grid(wd ~ .) + xlab("5-minute Interval") + ylab("Number of steps") + ggtitle("Time Series plot of Weekday/Weekend steps/interval")
version

