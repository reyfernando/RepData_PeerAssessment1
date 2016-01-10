
#This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
#The data for this assignment can be downloaded from the course web site:
  
#taset: Activity monitoring data [52K]
#The variables included in this dataset are:
  
#steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
#date: The date on which the measurement was taken in YYYY-MM-DD format
#interval: Identifier for the 5-minute interval in which measurement was taken

#The####
#Reproducible Research: Peer Assessment 1
####

P1: CodeLoading and preprocessing data  w#We unzip de date and label is db_activity
library(ggplot2)
file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/Factivity.zip")
unzip("./data/Factivity.zip", exdir="./data")
db_activity <- read.csv("./data/activity.csv")
name
#get familiar w the source
s(db_activity)
head(db_activity)
str(db_activity)
summary(db_activity)


#STEP2: Histogram of the total number of steps taken each day
#S
?hist
hi?aggregate
#aggregates steps by date, instead of interval
Steps_per_Date <- aggregate(steps ~ date,data=db_activity, sum, na.rm = TRUE)
Steps_per_Date

hist(Steps_per_Date$steps, breaks = 30,
     main="Total Steps per Day", 
     xlab="Steps",
     ylab="Frequency")

#The plot highlights the range between 10k and 13k as most frequent. 
#We also observe two outliers 1) lazy: with low activy below 2k and 2) Go_getters with an above average activity beyond 20k per day.

#STEP3: Mean and median number of steps taken each daygmean_steps <-mean(Steps_per_Date$steps, na.rm=TRUE)amedian_steps<-median(Steps_per_Date$steps, na.rm = TRUE)

mean_data
median_data

P4: Time series plot of the average number of steps taken
#STE#This provides the average number of steps per interval regardless of date

Data_mean<- aggregate(steps ~ interval, data= db_activity, mean, na.rm = TRUE)
Data_mean

##normal R ploting
plot(Data_mean$steps ~ Data_mean$interval
     , type="l"
     , main = "Average number of steps per interval"
     , xlab = "Interval"
     , ylab = "Avg. Steps" 
     )



P5: The 5-minute interval that, on average, contains the maximum number of steps
#STE
head(Data_mean)
## provides which row
which.max(Data_mean$steps)
## outputs row
Data_mean[which.max(Data_mean$steps),]


P6: Code to describe and show a strategy for imputing missing data
#STE#Since there are some na values across the 
#data, this may result in a bias analysis. 
## This may well be also a reason why the delta between mean and median is so small for instance. 

#gets all missing values
na_data <- sum(is.na(db_activity$steps))
print(paste("Number of missing values in the source", na_data, "rows."))


#one can replace na for zero, for mean or median.
# median seems more accurate as it takes skewness into consideration.
fixed_data<- db_activity

#STEP7: Histogram of the total number of steps taken each day after missing values are imputed
fixed_data$steps[is.na(fixed_data$steps)] <- median(db_activity$steps, na.rm=TRUE)

fixed_data_per_day <- aggregate(steps ~ date, data=fixed_data, sum, na.rm=TRUE)

hist(fixed_data_per_day$steps, breaks= 30, main = "Steps per day w/ Adjustment excluding NA" , xlab="Steps" )

#STEP8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
fixed_data$date <- as.Date(fixed_data$date)
str(fixed_data$date)E
fixed_data$datename <- weekdays(fixed_data$date)
fixed_data
##creates factors for weekend
##gets weekdays and weekends


fixed_data$weekend_flag <- as.factor(ifelse(fixed_data$datename =="Saturday" | fixed_data$datename =="Sunday","weekend","weekday" ))
interval_summary <- aggregate(steps ~ interval + weekend_flag, fixed_data, mean)
head(interval_summary)
plot <- ggplot(interval_summary, aes(x=interval, y=steps, color = weekend_flag)) +
            geom_line() + facet_wrap(~weekend_flag, ncol = 1, nrow=2)
print(plot)

P9: All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


