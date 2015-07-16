library(dplyr)
library(ggplot2)



setwd("Z:\\OMAR\\RCoursera\\RepData\\RepData_PeerAssessment1\\")

#file.choose()

###

activity <- read.csv("Z:\\OMAR\\RCoursera\\RepData\\RepData_PeerAssessment1\\activity.csv")

activity$date <- as.Date(activity$date)


#View(activity)
#str(activity)



###### Number of steps by day ####


steps_bydate <- activity %>%
                group_by(date) %>%
                summarise(steps_sum = sum(steps),
                          steps_mean = mean(steps, na.rm = T))
                           



#head(steps_bydate)
#tail(steps_bydate)


###########



ggplot(data=steps_bydate, aes(steps_bydate$steps_sum)) + geom_histogram()

#######

#mean(steps_bydate$steps_sum, na.rm = T)
#median(steps_bydate$steps_sum, na.rm = T)
#sum(steps_bydate$steps_sum, na.rm = T)




ggplot(data=steps_bydate, aes(steps_bydate$steps_sum)) + geom_histogram()


##### What is the average daily activity pattern? ###


activity_pattern <- activity %>%
    group_by(interval) %>%
    summarise(steps_sum = sum(steps, na.rm = T),
              steps_mean = steps_sum/61)

# Divide steps_sum / 61. There are 61 intervals for each value.
# There are 61 "0" intervals, 61 "5" intervals, and so on...

#table(activity$interval) Con esto se ve la frecuencia de las entradas.


ggplot(data=activity_pattern, aes(x = activity_pattern$interval, y = activity_pattern$steps_mean)) + geom_line()

# Need the number of rows per interval...

#head(activity_pattern)
#tail(activity_pattern)
#View(activity_pattern)





## Rango con max numero de Steps.

activity_pattern[activity_pattern$steps_sum == max(activity_pattern$steps_sum),]

activity_pattern[activity_pattern$steps_mean == max(activity_pattern$steps_mean),]



## Parte 4
## Imputing missing values:


# Total number of NAs in dataset.
sum(is.na(activity))

## 

activity_complete <- activity

activity_complete$steps <- activity_pattern$steps_mean[match(any(activity_complete$interval), activity_pattern$interval)]

#activity_complete$steps[is.na(activity_complete$steps)] <- "omar"


if (any(activity_complete$steps) == T){
    activity_complete$steps <- activity_pattern$steps_mean[match(activity_complete$interval, activity_pattern$interval)]
}





dput(activity_complete, "df1")
dput(activity_pattern, "df2")


## Parte 5 ##

activity$days <- weekdays(activity$date)



    
activity <- mutate(activity,
                   Weekday = NA,
                   Weekday = ifelse(grepl("lunes", days)
                                    | grepl("martes", days) | grepl("miércoles", days)
                                    | grepl("jueves", days) | grepl("viernes", days),
                                    "Weekday", Weekday),
                             
                   Weekday = ifelse(grepl("sábado", days)
                                   | grepl("domingo", days),
                                   "Weekend", Weekday))




