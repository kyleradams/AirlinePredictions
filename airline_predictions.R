# Kyler Adams & Joseph Carl
# IS 5201
# Forecasting project

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






######################
### FUNCTION START ###
demand_forecast = function(train, test) {
    val.data <- read_csv(test)
    train.data <- read_csv(train)
    
    # Convert departure_date and booking_date to date format
    # Need date format in order to do calculations on dates as numbers
    # This step is required for both training and validation dataset
    train.data$departure_date <- as.Date(train.data$departure_date, "%m/%d/%Y")
    train.data$booking_date <- as.Date(train.data$booking_date, "%m/%d/%Y")
    val.data$departure_date <- as.Date(val.data$departure_date, "%m/%d/%Y")
    val.data$booking_date <- as.Date(val.data$booking_date, "%m/%d/%Y")
    
    # Create days_to_departure variable
    # This variable will be used in regression
    # This step is required for both training and validation dataset
    train.data$days_to_departure <- train.data$departure_date - train.data$booking_date
    val.data$days_to_departure <- val.data$departure_date - val.data$booking_date
    
    # Create a lag variable cum_bookings_lag1day that gives the cumulative bookings for the previous day
    # This variable will be used in regression
    # This step is required for both training and validation dataset
    train.data <- train.data %>% group_by(departure_date) %>% arrange(departure_date, booking_date) %>% 
        mutate(cum_bookings_lag1day = ifelse(days_to_departure+1 == dplyr::lag(days_to_departure), 
                                             dplyr::lag(cum_bookings), NA))
    val.data <- val.data %>% group_by(departure_date) %>% arrange(departure_date, booking_date) %>% 
        mutate(cum_bookings_lag1day = ifelse(days_to_departure+1 == dplyr::lag(days_to_departure), 
                                             dplyr::lag(cum_bookings), NA))
    val.data <- data.frame(val.data)
    
    # Add final demand column to use as dependent variable for regression
    final.demand <- train.data %>% group_by(departure_date) %>% 
        filter(days_to_departure == 0) %>% 
        summarise(final_demand = cum_bookings)
    # Merge the final demand new column into the training data
    train.data <- merge(train.data, final.demand, by = "departure_date")
    
    # Create days of the week variable to subset data, and create different models
    #   depending on the day of the week
    # This step is required for both training and validation dataset
    train.data$departure_DOW <- factor(weekdays(train.data$departure_date), levels = 
                                           c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    val.data$departure_DOW <- factor(weekdays(val.data$departure_date), levels = 
                                         c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    # Create vector of days of the week which will be used for looping
    DOW <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
    
    
    ########## FIRST LOOP #############
    # For each day of the week, grab all values from training data which match
    #   that day of the week.  Then create a support vector machine regression
    #   (SVM regression) model for each.  Store all models into the list "all_models"
    all_models <- c()
    for(i in seq_along(DOW)){
        
        d <- DOW[i]
        dd <- subset(train.data, train.data$departure_DOW == d)
        mod <- svm(final_demand ~ days_to_departure + cum_bookings +
                       cum_bookings_lag1day, data = dd, na.action = na.omit)
        all_models <- c(all_models, list(mod))
    }
    
    # Remove NA values (the first day bookings open will have NA for 
    #   lag1day because there is no prior day)
    # This step is required for both training and validation dataset
    filtered.data <- train.data[!is.na(train.data$cum_bookings_lag1day),]
    val.filtered.data <- val.data[!is.na(val.data$cum_bookings_lag1day),]
    
    # Initialize data frame for storing predictions
    pred.by.day <- data.frame()
    
    
    ########## SECOND LOOP #############
    # For each day of the week, grab all values from training data which match
    #   that day of the week.  Then, using SVM regression, predict final demand
    #   based on the model which corresponds to the day of the week.
    #
    #   NOTE: This loop is not necessary for the validation data-set, but is for 
    #   testing purposes only.
    for(i in seq_along(DOW)){
        
        d <- DOW[i]
        dd <- subset(filtered.data, filtered.data$departure_DOW == d)
        dd$predict <- predict(all_models[[i]], newdata = dd)
        pred.by.day <- rbind(pred.by.day,as.data.frame(dd))
    }

    
    
    # Initialize data frame for storing predictions
    validation.pred.by.day <- data.frame()
    
    ########## FINAL LOOP #############
    # For each day of the week, grab all values from validation data which match
    #   that day of the week.  Then, using SVM regression, predict final demand
    #   based on the model which corresponds to the day of the week.
    for(i in seq_along(DOW)){
        
        d <- DOW[i]
        dd <- subset(val.filtered.data, val.filtered.data$departure_DOW == d)
        dd$predict <- predict(all_models[[i]], newdata = dd)
        validation.pred.by.day <- rbind(validation.pred.by.day,dd)
    }
    
    
    # Calculate mean absolute scaled error (MASE)
    # MASE allows us to compare prediction models.
    # In this case, we are comparing the SVM regression model to the "naive model"
    # We are only comparing predictions for the final week.
    val.numerator <- validation.pred.by.day %>% 
        filter(days_to_departure <= 7 & days_to_departure > 0) %>% 
        summarise(totalnum = sum(abs(final_demand - predict))) %>% 
        summarise(totalnum = sum(totalnum))
    val.denominator <- sum(abs(val.filtered.data$final_demand - val.filtered.data$naive_forecast), na.rm = T)
    val.MASE = val.numerator / val.denominator
    
    # This data frame will be returned by the function, all prediction values
    return_forecasts <- validation.pred.by.day %>% 
        select(departure_date, booking_date, predict)
    
    # END #
    return(list(MASE = paste("MASE:", round(val.MASE, 6), sep = " "), 
                forecasts = return_forecasts))
    
}



