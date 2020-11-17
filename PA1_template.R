#Read data from computer
activity <- read.csv('D:\\Coursera\\Reproducible_Research\\A1/activity.csv')

#check a part of data
head(activity)

# Calculate the total number of steps taken per day
total_steps_perday <- tapply(activity$steps, activity$date, sum)
head(total_steps_perday)

#drawing the histogram
hist(total_steps_perday)

#Calculate and report the mean and median of the total number of steps taken per day
mean_steps <-mean(total_steps_perday, na.rm=T)
median_steps <- median(total_steps_perday, na.rm=T)

# Calculate the average number of steps taken per interval
avg_steps_interval <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)

# Plot
plot(steps~interval, data=avg_steps_interval, type="l")

#Find which 5-minute interval contains the maximum number of steps
max_steps_interval <- avg_steps_interval$interval[which.max(avg_steps_interval$steps)]

#Calculate and report the total number of missing values in the dataset
na_num <- sum(is.na(activity$steps))

#remove missing values with the mean for that 5-minute interval
for (i in 1:length(activity$steps)){
  if (is.na(activity$steps[i])==1){
    index <- which(avg_steps_interval$interval==activity$interval[i])
    activity$steps[i] <- avg_steps_interval$steps[index]
  }
}

#check the total number of missing values in the dataset
sum(is.na(activity$steps))

#After NA removal process, calculating the total number of steps taken per day
narm_total_steps_perday <- tapply(activity$steps, activity$date, sum)

#drawing the histogram
hist(narm_total_steps_perday)

#Calculate and report the mean and median of the total number of steps taken per day after removing NAs
new_mean_steps <-mean(narm_total_steps_perday, na.rm=T)
new_median_steps <- median(narm_total_steps_perday, na.rm=T)

#Creating an new column to indicate weekday and weekend
activity$weekday <- ifelse(weekdays(as.Date(activity$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")

#Make a panel plot
library(lattice)
new_avg_steps_interval <- aggregate(steps~interval+weekday, data=activity, mean, na.rm=TRUE)
xyplot(steps~interval|weekday,data=new_avg_steps_interval,type = "l",layout=1:2, ylab = 'Number of steps')
