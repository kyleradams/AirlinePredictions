# Kyler Adams

# Clear workspace
rm(list=ls(all=TRUE))
# required libraries
library(tidyverse)
library(zoo)
library(e1071)


## TEST CODE PLEASE SCROLL DOWN TO FUNCTION ##
###### TEST CODE IS NOT FULLY COMMENTED ######
##############################################
#############        START      ##############
##############################################

# Read in csv files
val.data <- read_csv("airline_booking_validationData.csv")
train.data <- read_csv("airline_booking_trainingData.csv")

# Convert departure_date and booking_date to date format
train.data$departure_date <- as.Date(train.data$departure_date, "%m/%d/%Y")
train.data$booking_date <- as.Date(train.data$booking_date, "%m/%d/%Y")

# Create days_to_departure variable
train.data$days_to_departure <- train.data$departure_date - train.data$booking_date

# Create a lag variable cum_bookings_lag1day that gives the cumulative bookings for the previous day
train.data <- train.data %>% group_by(departure_date) %>% arrange(departure_date, booking_date) %>% 
    mutate(cum_bookings_lag1day = ifelse(days_to_departure+1 == dplyr::lag(days_to_departure), 
                                         dplyr::lag(cum_bookings), NA))

final.demand <- train.data %>% group_by(departure_date) %>% 
    filter(days_to_departure == 0) %>% 
    summarise(final_demand = cum_bookings)
train.data <- merge(train.data, final.demand, by = "departure_date")

train.data$departure_DOW <- factor(weekdays(train.data$departure_date), levels = 
                                       c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
train.data$two_weeks_out <- ifelse(train.data$days_to_departure <= 14, 1, 0)



DOW <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
all_models <- c()
for(i in seq_along(DOW)){
    
    d <- DOW[i]
    dd <- subset(train.data, train.data$departure_DOW == d)
    mod <- svm(final_demand ~ days_to_departure + cum_bookings +
                  cum_bookings_lag1day, data = dd, na.action = na.omit)
    all_models <- c(all_models, list(mod))
}

filtered.data <- train.data[!is.na(train.data$cum_bookings_lag1day),]
filtered.data <- filtered.data %>% 
        group_by(departure_date) %>% 
        mutate(cx = cumsum(cum_bookings_lag1day))


ddd <- data.frame()
for(i in seq_along(DOW)){
    
    d <- DOW[i]
    dd <- subset(filtered.data, filtered.data$departure_DOW == d)
    dd$predict <- predict(all_models[[i]], newdata = dd)
    ddd <- rbind(ddd,as.data.frame(dd))
}
# class(dd)
# View(ddd) 

# class(train.data)
# apply to test data
# Convert departure_date and booking_date to date format
val.data$departure_date <- as.Date(val.data$departure_date, "%m/%d/%Y")
val.data$booking_date <- as.Date(val.data$booking_date, "%m/%d/%Y")

# Create days_to_departure variable
val.data$days_to_departure <- val.data$departure_date - val.data$booking_date

# Create a lag variable cum_bookings_lag1day that gives the cumulative bookings for the previous day
val.data <- val.data %>% group_by(departure_date) %>% arrange(departure_date, booking_date) %>% 
    mutate(cum_bookings_lag1day = ifelse(days_to_departure+1 == dplyr::lag(days_to_departure), 
                                         dplyr::lag(cum_bookings), NA))
val.data <- data.frame(val.data)
val.data$departure_DOW <- factor(weekdays(val.data$departure_date), levels = 
                                       c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

val.filtered.data <- val.data[!is.na(val.data$cum_bookings_lag1day),]

ddd <- data.frame()

for(i in seq_along(DOW)){
    
    d <- DOW[i]
    dd <- subset(val.filtered.data, val.filtered.data$departure_DOW == d)
    dd$predict <- predict(all_models[[i]], newdata = dd)
    ddd <- rbind(ddd,dd)
}

val.numerator <- ddd %>% 
    filter(days_to_departure <= 7 & days_to_departure > 0) %>% 
    summarise(totalnum = sum(abs(final_demand - predict))) %>% 
    summarise(totalnum = sum(totalnum))
val.denominator <- sum(abs(val.filtered.data$final_demand - val.filtered.data$naive_forecast), na.rm = T)
# View(filtered.val.data) 

val.MASE = val.numerator / val.denominator
val.MASE

hist(train.data$final_demand)

train.data %>% 
    group_by(departure_DOW) %>% 
    summarise(sum = sum(final_demand))

library(psych)
pairs.panels(train.data)



mod.ts <- lm(final_demand ~ departure_DOW, data = train.data)
summary(mod.ts)
val.data$predict <- predict(mod.ts, newdata = val.data)

############## END OF TEST CODE ##############
##############################################
#############        END        ##############
##############################################



