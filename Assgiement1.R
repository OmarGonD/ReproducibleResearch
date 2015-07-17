library(dplyr)
library(ggplot2)


##########################################
###  PARTE 1  ############################
#### Loading and procesing the data  #####
##########################################


setwd("Z:\\OMAR\\RCoursera\\RepData\\RepData_PeerAssessment1\\")


activity <- read.csv("Z:\\OMAR\\RCoursera\\RepData\\RepData_PeerAssessment1\\activity.csv")

activity$date <- as.Date(activity$date)




#########################################################
### Part 2 ##############################################
### What is mean total number of steps taken per day? ###
#########################################################


### 2.1 Calculate the total number of
### steps taken per day


steps_bydate <- activity %>%
                group_by(date) %>%
                summarise(steps_sum = sum(steps, na.rm = T),
                          steps_mean = mean(steps, na.rm = T))
                           



###########

### 2.2 Make a histogram of
### the total number of steps taken each day

ggplot(data=steps_bydate, aes(steps_bydate$steps_sum)) + geom_histogram(binwidth = 4000)



###########

### 2.3 Calculate and report the mean and median of
### the total number of steps taken per day

mean(steps_bydate$steps_sum, na.rm = T)
median(steps_bydate$steps_sum, na.rm = T)
sum(steps_bydate$steps_sum, na.rm = T)



### Parte 3 ###
### What is the average daily activity pattern? ###

### 3.1 Make a time series plot (i.e. type = "l") 
### of the 5-minute interval (x-axis) and 
### the average number of steps taken, averaged across
### all days (y-axis)


activity_pattern <- activity %>%
    group_by(interval) %>%
    summarise(steps_sum = sum(steps, na.rm = T),
              steps_mean = round(steps_sum/61, 0),
              setps_mean2 = steps_sum/61)


ggplot(data=activity_pattern,
       aes(x = activity_pattern$interval, y = activity_pattern$steps_mean)) +
    geom_line()


# Divide steps_sum / 61. There are 61 intervals for each value.
# There are 61 "0" intervals, 61 "5" intervals, and so on...

#table(activity$interval) Con esto se ve la frecuencia de las entradas.


### 3.2 Which 5-minute interval, on average across all the days in the dataset,
### contains the maximum number of steps?


max.steps.sum <- max(activity_pattern$steps_sum)

activity_pattern[activity_pattern$steps_sum == max.steps.sum,]






### Parte 4 ###
### Imputing missing values ###


### 4.1 Calculate and report the total number
### of missing values in the dataset (i.e. the 
### total number of rows with NAs)


sum(!complete.cases(activity))

### 4.2 Devise a strategy for filling in all of the 
### missing values in the dataset. The strategy does 
### not need to be sophisticated. For example, you could 
### use the mean/median for that day, or the mean for that 5-minute interval, etc.

activity_complete <- activity


#Nuevo intento. Basado en:
#http://stackoverflow.com/questions/24847299/
#using-ifelse-to-replace-nas-in-one-
#data-frame-by-referencing-another-data-fram


### 4.3 Create a new dataset that is equal to the original
### dataset but with the missing data filled in.

activity_complete$steps <- ifelse(is.na(activity_complete$steps) == TRUE,
                                  activity_pattern$steps_mean[activity_pattern$interval %in% activity_complete$interval], activity_complete$steps)



#Funciona utilizando match()

#http://stackoverflow.com/questions/24847299/using-ifelse-to-replace-nas-in-one-data-frame-by-referencing-another-data-fram

#activity_complete$steps[is.na(activity_complete$steps)] <- activity_pattern$steps_mean[match(activity_complete$interval[is.na(df1$B)],df2$A)]


#df1$B[is.na(df1$B)] <- df2$B[match(df1$A[is.na(df1$B)],df2$A)]

# Lo que YO HICE, pero esta mal

#df1$steps <- df2$steps_mean[match(df1$interval, df2$interval)]
############


#activity_complete$steps <- activity_pattern$steps_mean[match(any(activity_complete$interval), activity_pattern$interval)]

#activity_complete$steps[is.na(activity_complete$steps)] <- "omar"









### Parte 5 ###

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




