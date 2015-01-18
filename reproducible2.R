func<-function() {
  
  #load the data
  data<-read.csv("activity.csv")
  
  #Process/transform the data (if necessary) into a format suitable 
  #for your analysis
  #converst date field from factor to date class
  data$date<-as.Date(as.character(data$date))
  
  #Make a histogram of the total number 
  #of steps taken each day
  sumdaysteps<-tapply(data$steps,data$date,sum)
  par(mfrow=c(1,1))#,mar=c(2,2,2,2))
  #hist(sumdaysteps,xlab="steps per day",col="blue",lwd=2,breaks=10,
  #     main="Number of steps per day")
  #Calculate and report the mean and median total 
  #number of steps taken per day
  mean_steps_per_day<-mean(sumdaysteps,na.rm=T)
  print(mean_steps_per_day)
  median_steps_per_day<-median(sumdaysteps,na.rm=T)
  print(median_steps_per_day)
  
  #Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
  #number of steps taken, averaged across all days (y-axis)
  stepsininterval<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
  #plot(stepsininterval,type="l",col="red")
  max_interval<-as.numeric(names(which.max(stepsininterval)))
  print(max_interval)
  #Calculate and report the total number of missing values in the dataset 
  #(i.e. the total number of rows with NAs)
  missing_values_number<-sum(is.na(data$steps))
  print(missing_values_number)

  #Devise a strategy for filling in all of the 
  #missing values in the dataset.
  #For filling NA values use mean for that 5-minute interval
  #Create a new dataset that is equal to the original dataset 
  #but with the missing data filled in.
  data1<-data
    for (i in 1:nrow(data1)) {
    if (is.na(data1$steps[i])) {
      data1$steps[i]<-stepsininterval[as.character(data$interval[i])]
    }
  }

  #Make a histogram of the total number 
  #of steps taken each day for the new set with imputing values
  sumdaysteps_update<-tapply(data1$steps,data1$date,sum)
  #hist(sumdaysteps_update,xlab="steps per day updated",col="blue",
  #     lwd=2,breaks=10,main="Number of steps per day")
  mean_steps_per_day_update<-mean(sumdaysteps_update)
  print(mean_steps_per_day_update)
  median_steps_per_day_update<-median(sumdaysteps_update,na.rm=T)
  print(median_steps_per_day_update)
  
  #Are there differences in activity patterns between weekdays and weekends?
  #save current system's locale
  locale <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale("LC_TIME", "English")
  wdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
  data1$day<-weekdays(data1$date)
  #Create a new factor variable in the dataset with two levels – "eekday” and
  #“weekend” indicating whether a given date is a weekday or weekend day.
  data1$dayweek<-as.factor(ifelse(data1$day %in% wdays,"Weekday","Weekend"))
  
  #Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) 
  #and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
  stepsininterval_by_day<-with(data1,aggregate(steps,
                                    by=list(interval,dayweek),mean))
  names(stepsininterval_by_day)<-c("interval","dayweek","steps")
  library(lattice)
  xyplot(steps ~ interval | dayweek, stepsininterval_by_day, type = "l", layout = c(1, 2), 
         ylab = "Average number of steps")
  
  Sys.setlocale("LC_TIME", locale)
  stepsininterval_by_day
}