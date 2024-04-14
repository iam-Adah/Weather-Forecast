## Installing libraries
install.packages("tidyverse") 
install.packages("dplyr") 
install.packages("zoo")  
install.packages("forecast") 
install.packages("tseries")
install.packages("DescTools")


## Loading libraries
library(tidyverse)
library(dplyr)
library(zoo)
library(forecast)
library(tseries)
library(DescTools)

##getting and setting file path    
getwd()
setwd("C:/Users/Chidera/Documents/Tutorial")

## Getting the raw dataset for me to get the heading
data_head <- read_csv("WRFdata_May2018.csv", n_max = 2, 
                        col_names = FALSE)

## Converting the X. in the datetime row to NA

data_head[1,][grep("^X\\.", data_head[1,])] <- NA

View(data_head)

## TO create the datetime from TSK COLUMN TO SMOIS COLUMN
for (i in 4:ncol(data_head)) {
  prev <- data_head[, i-1]
  this <- data_head[, i]
  missing <- is.na(this)
  this[missing, ] <- prev[missing, ]
  data_head[, i] <- this
}


# Remove the leading "X" character from the datetime strings
date_new <- as.POSIXct(sub("X", "", data_head[1,]), format = "%d.%m.%Y.%H.%M")
print(date_new)
str(date_new)
View(date_new)

# Remove the leading "X" character from the datetime strings
#datetime_strings <- gsub("X", "", data_head[1,])

# Convert the datetime strings to POSIXct objects
#datetime_objects <- as.POSIXct(datetime_strings, format = "%d.%m.%Y.%H.%M")

# Print the datetime objects
#print(datetime_objects)

#data_head[nrow(data_head) + 1,] <- data.frame(as.POSIXct(datetime_strings, format = "%d.%m.%Y.%H.%M"))

data_head =  rbind(data_head,format(date_new, "%d/%m/%Y %H:%M"))
head(data_head)
View(data_head)





##Importing the selected 300 rows of dataset in CSV
data <- read.csv("dera.csv", all=TRUE)
class(data)

#view  the datasett
head(data)

#structure if the dataset
str(data)

#summary statistics
summary(data)

#check the missing values
sum(is.na(data))
ncol(data)
 
### Interpolating to remove NA from the 300 records 
 data[2:2482] <- na.approx(data[2:2482])
 dfs <- na.approx(data, rule = 2)
 sum(is.na(dfs))

 
 class(dfs)
 col_name <- colnames(dfs)
 col_name
 
 dfs <- as.data.frame(dfs)
 
 
 ### create detect outlier function
 
 outlier_detect <- function(x) {
   
   #Outlier detection using Z-Score method for 'Ozone'
   data <- x
   z_scores <- scale(data)
   threshold <- 2
   outliers <- data[abs(z_scores) > threshold]
 }
 
 
 ### create check outlier function
 check_outlier <- function(dataframe,
                            columns=names(dataframe)) {
   
   # for loop to traverse in columns vector
   for (col in columns) {
     
     # remove observation if it satisfies outlier function
     dataframe <- dataframe[outlier_detect(dataframe[[col]]), ]
     
   }
   
   print(dataframe)
 }
 
 check_outlier(dfs, c(col_name))
 
 
 
 # Handling outliers using Winsorization
 
 
 outlier_remover <- function(x) {
   
   #Outlier detection using Z-Score method for 'Ozone'
   data <- x
   winsorized_data <- Winsorize(data, probs = c(0.05, 0.95),na.rm=TRUE)
 }
 
 # create remove outlier function
 remove_outlier <- function(dataframe,
                            columns=names(dataframe)) {
   
   # for loop to traverse in columns vector
   for (col in columns) {
     
     # remove observation if it satisfies outlier function
     dataframe <- dataframe[outlier_remover(dataframe[[col]]), ]
   }
   
   # return dataframe
   print("Remove outliers")
   print(dataframe)
 }
 
remove_outlier(dfs, c(col_name))
 
dfs




## Subseting to get pick the lag and long
dfs <- as.matrix(dfs)
df_finals <- dfs[150,]
View(df_finals)
is.null(df_finals)
names(df_finals) <- NULL
is.null(df_finals)
#df_finals <- df_finals[,-1]

## Transpose the dataset

data_head <- t(data_head)
df_finals <- t(df_finals)

## combining the dataset
df_body = cbind(data_head, setNames(df_finals, rep("", ncol(df_finals))))

# Delete the first two rows
df_body<- df_body[-c(1:2),]

# Delete the first column
df_body <- df_body[, -1]

# Insert a new header name
colnames(df_body) <- c("Weather", "Datetime", "Number")


View(df_body)

# Convert the matrix to a data frame
df_last <- as.data.frame(df_body)

sum(is.na(df_last))

# Use gsub() to remove the quotes around "ad"
df_last$Weather <- gsub('"', '', df_last$Weather)


df_last[duplicated(df_last), ]
# Remove duplicate rows
df_last <- unique(df_last)
df_last[duplicated(df_last), ]

df_last$Number <- as.double(df_last$Number)
df_last$Datetime <- as.POSIXct(df_last$Datetime, format="%d/%m/%Y %H:%M")
df_last$Weather <- as.factor(df_last$Weather)

# View the updated data frame
#df_last[duplicated(df_last), ]
#df_last

# Using pivoted
weather_pivoted <- df_last %>%
  pivot_wider(names_from = Weather, values_from = Number,)

# Selecting the final column required for the analysis
df_final <- weather_pivoted %>% 
  select(Datetime, TSLB)


# Splitting the datatime column into  Year,Month, Day and Time
df_final$Year <- as.numeric(format(df_final$Datetime, "%Y"))
df_final$Month <- as.numeric(format(df_final$Datetime, "%m"))
df_final$Day <- as.numeric(format(df_final$Datetime, "%d"))
df_final$Hour <- as.numeric(format(df_final$Datetime, "%H"))
View(df_final)

# Removing the date column
df_final <- df_final %>% select(-Datetime)
View(df_final)

class(df_final$TSLB)

# TRAIN AND TEST DATASET

# Determine the number of hours in the dataser
n_hours <- nrow(df_final)

# to convert the dataset to  time series object
df_final_ts <- ts(df_final$TSLB, start = c(2001,1), frequency = 24 * 365.25)

# Determining the index of the last observation in the training set
train_end_idx <- which(df_final$Year == 2028 & 
                         df_final$Month == 5 &
                         df_final$Day == 20 &
                         df_final$Hour == 18)[1]
View(train_end_idx)

# In a case where train_end_idx is not found, it can be calculated using total number of hours
if (is.na(train_end_idx)){
  train_end_idx <- as.integer((2028 -2001 + 1) * 365.25*24)
}

# splitting the dataset into traina and test dataset

train_data <- df_final_ts[1:train_end_idx]
train_data <- unlist(train_data)
View(train_data)

test_data <- df_final_ts[(train_end_idx + 1):n_hours]
test_data <- unlist(test_data)
View(test_data)



#ARIMA MODEL

# fitting the arima model
arima_model <- auto.arima(train_data,seasonal = TRUE, stepwise = TRUE)
# Forcasting using  arima model
arima_forecast <- forecast(arima_model, h = length(test_data))


# STANDARD REGRESSION MODEL

# create a linear regression dataset

regression_data <- data.frame(TSLB = as.numeric(df_final$TSLB),
                              Year = df_final$Year,
                              Month = df_final$Month,
                              Day = df_final$Day,
                              Hour = df_final$Hour)
view(regression_data)


#Split the regression dataset into train and test set
train_data_regression <- regression_data %>% filter(Year < 2029)
test_data_regression <- regression_data %>% filter(Year >= 2029)

# fit the standard regression model
regression_model <- lm(TSLB ~ Year + Month + Day + Hour, data = train_data_regression)

# forecast using the standard regression model
regression_forecast <- predict(regression_model, test_data_regression)

#EVALUATING THE BEST MODELS

# Calculating the Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) metrics of ARIMA Model
mae_arima <- mean(abs(test_data - arima_forecast$mean))
rmse_arima <- sqrt(mean((test_data - arima_forecast$mean)^2))

# Calculating the Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) metrics of Standard Regression Model
mae_regression <- mean(abs(test_data - regression_forecast))
rmse_regression <- sqrt(mean((test_data - regression_forecast)^2))

# Displaying the performance matrics
cat("ARIMA Model: MAE =", mae_arima, ", RMSE =", rmse_arima, "\n")
cat("Standard Regression Model: MAE =", mae_regression, ", RMSE =", rmse_regression)

