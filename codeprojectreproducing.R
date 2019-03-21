
library(xtable)          ####package needed
library(latexpdf)
library(knitr)
library(lubridate)
library(dplyr)



rawstepdata <- read.csv("activity.csv")

###getting the flavor of the raw data
#head(rawstepdata)
#str(rawstepdata)
#summary(rawstepdata)

tidydata <- rawstepdata
tidydata$date <- as.Date.character(rawstepdata$date,format = '%Y-%m-%d')
str(tidydata)


## What is mean total number of steps taken per day?

sumperday <- with(tidydata, tapply(steps, FUN = sum, date))  ### We apply on the data set tidy data, the function sum on the number of set by factoring on the date)

png("plot1.png")
hist(sumperday,
     breaks = 10,
     main = "Histogram for the total number of step per day") ### we took 10 as a break line to have a better understanding on how is split our result, and see a littlebit the tail effect. 
abline(v = mean(sumperday,na.rm = TRUE), col="green") ###let us remove the NA value for now in order to get a result
abline(v=median(sumperday, na.rm=TRUE), col="red") ###let us remove the NA value for now in order to get a resul
dev.off()

meanday<- as.numeric(mean(sumperday,na.rm = TRUE))  ###let us remove the NA value for now in order to get a result
medianday<-as.numeric(median(sumperday,na.rm = TRUE)) ###let us remove the NA value for now in order to get a result

## What is the average daily activity pattern?

intervalaverage <- with(tidydata, tapply(steps, FUN = mean, interval,na.rm=TRUE))  ### We apply on the data set tidy data, the function mean on the number of step by factoring on the interval


png("plot2.png")
plot(intervalaverage, 
     type='l',
     main= "Average number of steps per interval within October-November",
     xlab = "5-minute interval",
     ylab= " average number of step")
dev.off()

intervalaverage[intervalaverage == max(intervalaverage)]


## Imputing missing values

totalNA <- sum(is.na(rawstepdata$steps))
print(totalNA)

MissingValue <- tidydata[is.na(tidydata$steps),]    #### extracting all the rows where we have missing values
intervalaverageNew <- as.data.frame(intervalaverage,col.names = FALSE) #### preparing the mean value to replace the missing one
intervalaverageNew$interval <- row.names(intervalaverageNew) ##converting the names of the row in a real column

NewValue <- merge(MissingValue,intervalaverageNew, by = "interval", all.x=TRUE) ###we add an extra column with the replacign value

NewValue$steps <- NULL   ###removing previosu steps NA column
names(NewValue)[3] <- "steps"  ###rename the column of our replacing value
NewValue <- NewValue[order(NewValue$date,NewValue$interval),] ### we reorder to have the date and interval lexical order
NewStepData <- tidydata
NewStepData[is.na(NewStepData$steps),1] <- NewValue$steps ### now we replace in our new data set the missing value

sumperdaywithoutNA <- with(NewStepData, tapply(steps, FUN = sum, date))  ### We apply on the data set tidy data, the function sum on the number of set by factoring on the date)


png("plot3.png")
hist(sumperdaywithoutNA,
breaks = 10,
main = "Histogram for the total number of step per day without missing value") ### we took 10 as a break line to have a better understanding on how is split our result, and see a littlebit the tail effect. 
abline(v = mean(sumperdaywithoutNA), col="green") ###let us remove the NA value for now in order to get a result
abline(v=median(sumperdaywithoutNA), col="red") ###let us remove the NA value for now in order to get a result

dev.off()



meandaywithoutNA<- as.numeric(mean(sumperdaywithoutNA))  ### we get a number because no NA anymore
mediandaywithoutNA<-as.numeric(median(sumperdaywithoutNA)) ### we get a number because no NA

## Are there differences in activity patterns between weekdays and weekends?

WeekFactorStepData <- NewStepData   ###start from our previous data set which is tidy and withou NA value
WeekFactorStepData$nameday <- weekdays(WeekFactorStepData$date) ### let us determine which are weq

##let us put an indicator telling us if it is sunday or saturday aka the week end
WeekFactorStepData$week_ind <- as.numeric((WeekFactorStepData$nameday == "Sunday" | WeekFactorStepData$nameday == "Saturday")) ###bolean to spot the saturday and sunday
WeekFactorStepData$week_ind[WeekFactorStepData$week_ind==0] <- "weekday"  ##changin the 0 and 1 in appropriate terminology
WeekFactorStepData$week_ind[WeekFactorStepData$week_ind==1] <- "weekend"

##Split the data base in two, one for weekday and the other one for weekend using our indicator
WeekFactorStepDataWeek <- WeekFactorStepData[WeekFactorStepData$week_ind =="weekday",]

WeekFactorStepDataWeekend <- WeekFactorStepData[WeekFactorStepData$week_ind =="weekend",]


##now let us compute our vector of mean for each interval and our both data set
intervalaverageweekday <- with(WeekFactorStepDataWeek, tapply(steps, FUN = mean, interval))  ### We apply on the data set tidy data, the function mean on the number of step by factoring on the interval
intervalaverageweekend <- with(WeekFactorStepDataWeekend, tapply(steps, FUN = mean, interval)) 

##Finally let us plot oour time series


png("plot4.png")
plot(intervalaverageweekday,   ##average for the day of the week
type='l',
main= "Average number of steps per interval within October-November weekday and weekend",  ##title
xlab = "5-minute interval",
ylab= "average number of step",
col="blue")
lines(intervalaverageweekend,   ##average for the day of the weekend. We want it on the same graphe
col="red")
legend(1, 95, legend=c("Weekday", "Weekend"),
col=c("blue", "red"), lty=1:2, cex=0.8)
abline(h=mean(intervalaverageweekday), col="blue")  ###we had horizontal bar to get the overal mean
abline(h=mean(intervalaverageweekend), col="red")

dev.off()

