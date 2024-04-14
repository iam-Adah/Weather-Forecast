# INSTALL pACKAGES
install.packages("forecast")
install.packages("tseries")
install.packages("tidyverse")
install.packages("zoo")
install.packages("DescTools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubricate")
install.packages("ggplot2")
install.packages("imputeTS")
library(forecast)
library(tseries)
library(tidyverse)
library(zoo)
library(DescTools)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(imputeTS)


# SET WORKING DIRECTORY
setwd("C:\\Users\\USER\\Documents\\R_Projects")
getwd()

#LOAD FIRST 2 ROWS OF DATASET
Headers <- read_csv("WRFdata_May2018.csv", n_max = 2, 
                    col_names = FALSE)

#ASSIGN NA VAVLUES TO ALL THE X VALUES IN THE FIRST ROW OF "HEADERS"
Headers[1,][grep("^X\\.", Headers[1,])] <- NA

#FILL ALL COLUMNS WITH THE CORRESPONDING DATE-TIME VALUE
for (i in 4:ncol(Headers)) {
  past <- Headers[, i-1]
  curr <- Headers[, i]
  missing <- is.na(curr)
  curr[missing, ] <- past[missing, ]
  Headers[, i] <- curr
}
View(Headers)  

#REMOVE "X" FROM THE DATE_TIME VALUE
date_time <- as.POSIXct(sub("X", "", Headers[1,]), format = "%d.%m.%Y.%H.%M")
date_time

#BIND DATE_TIME
Headers =  rbind(Headers,format(date_time, "%d-%m-%Y %H:%M"))
head(Headers)
View(Headers)

#LOAD THE SELECTED 300 ROWS FROM DATASET
data <- read.csv("300_data.csv", header = FALSE)

#STRUCTURE OF DATASET
str(data)

#SUMMARY OF DATA
summary(data)

#check the missing values
sum(is.na(data))
ncol(data)

### Interpolating to remove NA from the 300 records 
data[2:2482] <- na.approx(data[2:2482])
df_300 <- na.approx(data, rule = 2)
sum(is.na(df_300))

class(df_300)
col_name <- colnames(df_300)
col_name

df_300 <- as.data.frame(df_300)

# create detect outlier function

outlier_detect <- function(x) {
  
  #Outlier detection using Z-Score method for 'Ozone'
  data <- x
  z_scores <- scale(data)
  threshold <- 2
  outliers <- data[abs(z_scores) > threshold]
}

# create check outlier function
check_outlier <- function(dataframe,
                          columns=names(dataframe)) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[outlier_detect(dataframe[[col]]), ]
    
  }
  
  print(dataframe)
}

check_outlier(df_300, c(col_name))


# Handling outliers using Winsorization

outlier_remover <- function(x) {
  
  #Outlier detection using Z-Score method
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

remove_outlier(df_300, c(col_name))

df_300

## Subseting to get pick the lag and long
df_300 <- as.matrix(df_300)
df_selected <- df_300[152,]
View(df_selected)
is.null(df_selected)
names(df_selected) <- NULL
is.null(df_selected)

## Transpose the dataset

Headers <- t(Headers)
df_selected <- t(df_selected)

#TRANSPOSE df_selected
df_selected <- t(df_selected)

## combining the dataset
df = cbind(Headers, setNames(df_selected, rep("", ncol(df_selected))))

#DELETE THE FIRST COLUMN
df <- df[, -1]

# Delete the first two rows
df <- df[-c(1:2),]

# Insert a new header name
colnames(df) <- c("W_Cond", "Datetime", "Value")

# Convert the matrix to a data frame
df <- as.data.frame(df)

sum(is.na(df))

# Use gsub() to remove the quotes around "ad"
df$W_Cond <- gsub('"', '', df$W_Cond)

df$Value <- as.double(df$Value)
df$Datetime <- as.POSIXct(df$Datetime, format="%d-%m-%Y %H:%M")
df$W_Cond <- as.factor(df$W_Cond)

# Using pivoted
W_Cond_pivoted <- df %>%
  pivot_wider(names_from = W_Cond, values_from = Value,)

# Splitting the datatime column into  Year,Month, Day and Time
W_Cond_pivoted$Year <- as.numeric(format(W_Cond_pivoted$Datetime, "%Y"))
W_Cond_pivoted$Month <- as.numeric(format(W_Cond_pivoted$Datetime, "%m"))
W_Cond_pivoted$Day <- as.numeric(format(W_Cond_pivoted$Datetime, "%d"))
W_Cond_pivoted$Hour <- as.numeric(format(W_Cond_pivoted$Datetime, "%H"))
View(W_Cond_pivoted)

#Calculate for Wind Speed
W_Cond_pivoted$wind_speed <- sqrt(W_Cond_pivoted$U10^2 + W_Cond_pivoted$V10^2)

##Visualizations for wind_seed over time line plot
ggplot(W_Cond_pivoted, aes(x = Datetime, y = wind_speed)) +
  geom_line() +
  labs(title = "Wind_speed Over Time",
       x = "Date and Time",
       y = "Wind_speed") +
  theme_minimal()
##scatter plot
ggplot(W_Cond_pivoted, aes(x = Datetime, y = wind_speed)) +
  geom_point() +
  labs(title = "Wind_speed Over Time",
       x = "Date and Time",
       y = "Wind_speed (TSK)") +
  theme_minimal()
#time series with smoothen
ggplot(W_Cond_pivoted, aes(x = Datetime, y = wind_speed)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, color = "blue") +
  labs(title = "Wind_speed Over Time",
       x = "Date and Time",
       y = "Wind_speed (TSK)") +
  theme_minimal()

# Selecting the final column required for the analysis
df1 <- W_Cond_pivoted %>% 
  select(Year, Month, Day, Hour, wind_speed)

#Determine the number of hours in the dataset
n_hours <- nrow(df1)

#Convert the dataset to a time series object
df1_ts <- ts(df1$wind_speed, start = c(2001,1), frequency = 24 * 365.25)
View(df1_ts)

# Determining the index of the last observation in the training set
train_end_idx <- which(df1$Year == 2018 & 
                         df1$Month == 5 &
                         df1$Day == 20 &
                         df1$Hour == 18)[1]
View(train_end_idx)

# In a case where train_end_idx is not found, it can be calculated using total number of hours
if (is.na(train_end_idx)){
  train_end_idx <- as.integer((2018 -2001 + 1) * 365.25*24)
}

# splitting the dataset into traina and test dataset

train_data <- df1_ts[1:train_end_idx]
train_data <- unlist(train_data)
View(train_data)

test_data <- df1_ts[(train_end_idx + 1):n_hours]
test_data <- unlist(test_data)
View(test_data)

#Develop The ARIMA model
#Fit the ARIMA model
arima_model <- auto.arima(train_data, seasonal = TRUE, stepwise = TRUE)

#Forecast using ARIMA model
arima_forecast <- forecast(arima_model, h = length(test_data))
print(arima_model)

# Calculating the Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) metrics of ARIMA Model
mae_arima <- mean(abs(test_data - arima_forecast$mean))
rmse_arima <- sqrt(mean((test_data - arima_forecast$mean)^2))
cat("ARIMA Model: MAE =", mae_arima)
cat("ARIMA MODEL: RMSE =", rmse_arima)

#Model Diagnostic for ARIMA model
checkresiduals(arima_model)

#Plot histogram of residuals
hist(arima_model$residuals, main = "Histogram of ARIMA Model Residuals",
     xlab = "Residuals", col = "lightblue")

#Perform Ljung-Box test
Box.test(arima_model$residuals, lag = 10, type = "Ljung-Box",
         fitdf = arima_model$p + arima_model$q)

#MACHINE LEARNING MODELS
#Load Packages
install.packages("gridExtra")
library(gridExtra)

# Preprocess dataset
df2 <- W_Cond_pivoted %>% 
  select(Datetime, wind_speed)
View(df2)

# Display the first few rows of the dataset
head(data)

#Summary Statistics
summary(df2$wind_speed)

# Time series plot
ggplot(df2, aes(x = Datetime, y = wind_speed)) +
  geom_line(color = "blue") +
  labs(title = "Wind_Speed Concentration Over Time",
       x = "Date and Time",
       y = "Wind_Speed Concentration") +
  theme_minimal()

# Create a variable to represent time
df2 <- df2 %>%
  mutate(time = as.numeric(difftime(Datetime, min(Datetime), units = "hours")))
View(df2)

# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(df2), 0.8 * nrow(df2))
train_data <- df2[train_indices, ]
test_data <- df2[-train_indices, ]
str(train_data)
str(test_data)

#Linear Regression
# Fit the model on the training set
train_model_LR <- lm(wind_speed ~ time, data = train_data)
summary(train_model_LR)

# Predict wind_speed values for the test set
predictions <- predict(train_model_LR, newdata = test_data)

# Calculate the root mean squared error (RMSE)
rmse_LR <- sqrt(mean((test_data$wind_speed - predictions)^2))
cat("RMSE_LR:", rmse_LR)

#Calculate the mean absolute error (MAE)
LR_MAE <- mean(abs(test_data$wind_speed - predictions))
cat("MAE LINEAR:", LR_MAE)

#Plot of Actual vs Predicted values
ggplot() +
  geom_point(data = test_data, aes(x = wind_speed, y = predictions), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs. Predicted Wind_Speed Concentration",
       x = "Actual Wind_Speed Concentration",
       y = "Predicted Wind_Speed Concentration") +
  theme_minimal()

#SUPORT VECTOR REGRESSION

install.packages("e1071")
library(e1071)

# Fit an SVR model on the training set radial
svr_model_rad <- svm(wind_speed ~ time, data = train_data, kernel = "radial")
# Display the SVR model summary
summary(svr_model_rad)

# Predict wind_speed values for the test set using the SVR model
svr_predictions <- predict(svr_model_rad, newdata = test_data)


# Calculate the mean absolute error (MAE)
svr_mae_rad <- mean(abs(test_data$wind_speed - svr_predictions))
cat("SVR_RADIAL MAE:", svr_mae_rad)

# Fit an SVR model on the training set poly
svr_model_poly <- svm(wind_speed ~ time, data = train_data, kernel = "poly")
# Display the SVR model summary
summary(svr_model_poly)

# Predict tsk values for the test set using the SVR model
svr_predictions <- predict(svr_model_poly, newdata = test_data)


# Calculate the mean absolute error (MAE)
svr_mae_poly <- mean(abs(test_data$wind_speed - svr_predictions))
cat("SVR_POLY MAE:", svr_mae_poly)

# Fit an SVR model on the training set linear
svr_model_lin <- svm(wind_speed ~ time, data = train_data, kernel = "linear")
# Display the SVR model summary
summary(svr_model_lin)

# Predict tsk values for the test set using the SVR model
svr_predictions <- predict(svr_model_lin, newdata = test_data)


# Calculate the mean absolute error (MAE)
svr_mae_lin <- mean(abs(test_data$wind_speed - svr_predictions))
cat("SVR_LINEAR MAE:", svr_mae_lin)

## compare the different SVR MODELS(RADIAL,POLY,LINEAR)
mae_df <- data.frame(
  Model = c("Linear", "Poly", "Radial"),
  MAE = c(svr_mae_lin, svr_mae_poly, svr_mae_rad)
)

library(ggplot2)

ggplot(mae_df, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Mean Absolute Error for SVR Models",
       x = "SVR Model",
       y = "Mean Absolute Error") +
  theme_minimal() +
  scale_fill_manual(values = c("Linear" = "black", "Poly" = "purple", "Radial" = "orange")) +
  theme(legend.position = "none")
## SECOND VISUALIZATION FOR SVR

ggplot(mae_df, aes(x = MAE, y = Model, color = Model)) +
  geom_point(size = 4) +
  labs(title = "Mean Absolute Error for SVR Models",
       x = "Mean Absolute Error",
       y = "SVR Model") +
  theme_minimal() +
  scale_color_manual(values = c("Linear" = "black", "Poly" = "purple", "Radial" = "orange")) +
  theme(legend.position = "none")

## RANDOM FOREST
install.packages("randomForest")
library(randomForest)

# Fit a Random Forest model on the training set ntree =100
rf_model_100 <- randomForest(wind_speed ~ time, data = train_data, ntree = 100)

# Display the Random Forest model summary
summary(rf_model_100)

# Predict wind_speed values for the test set using the Random Forest model
rf_predictions_100 <- predict(rf_model_100, newdata = test_data)


# Calculate the mean absolute error (MAE)
rf_mae_100 <- mean(abs(test_data$wind_speed - rf_predictions_100))
cat("Random Forest MAE:", rf_mae_100)

# Fit a Random Forest model on the training set ntree =200
rf_model_200 <- randomForest(wind_speed ~ time, data = train_data, ntree = 200)
# Display the Random Forest model summary
summary(rf_model_200)
# Predict wind_speed values for the test set using the Random Forest model
rf_predictions_200 <- predict(rf_model_200, newdata = test_data)


# Calculate the mean absolute error (MAE)
rf_mae_200 <- mean(abs(test_data$wind_speed - rf_predictions_200))
cat("Random Forest MAE:", rf_mae_200)

# Fit a Random Forest model on the training set ntree =500
rf_model_500 <- randomForest(wind_speed ~ time, data = train_data, ntree = 500)

# Display the Random Forest model summary
summary(rf_model_500)

# Predict wind_speed values for the test set using the Random Forest model
rf_predictions_500 <- predict(rf_model_500, newdata = test_data)


# Calculate the mean absolute error (MAE)
rf_mae_500 <- mean(abs(test_data$wind_speed - rf_predictions_500))
cat("Random Forest MAE:", rf_mae_500)

##COMPARE DIFFERENT RANDOM FOREST MODELS (NTREE=100.200,500) AND VISUALIZE
mae_rf_df <- data.frame(
  Model = c("ntree = 100", "ntree = 200", "ntree = 500"),
  MAE = c(rf_mae_100, rf_mae_200, rf_mae_500)
)

#Visualization to compare RF models
ggplot(mae_rf_df, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Mean Absolute Error for Random Forest Models",
       x = "Random Forest Model (ntree)",
       y = "Mean Absolute Error") +
  theme_minimal() +
  scale_fill_manual(values = c("ntree = 100" = "black", "ntree = 200" = "purple", "ntree = 500" = "orange")) +
  theme(legend.position = "none")

## SECOND VISUALIZTION FOR RANDOM FOREST PLOT
ggplot(mae_rf_df, aes(x = MAE, y = Model, color = Model)) +
  geom_point(size = 4) +
  labs(title = "Mean Absolute Error for Random Forest Models",
       x = "Mean Absolute Error",
       y = "Random Forest Model (ntree)") +
  theme_minimal() +
  scale_color_manual(values = c("ntree = 100" = "black", "ntree = 200" = "purple", "ntree = 500" = "orange")) +
  theme(legend.position = "none")

####################
# Create a dataframe with the MAE values for ARIMA, Linear Regression, SVR Poly, and Random Forest (ntree=500)
mae_comparison_df <- data.frame(
  Model = c("ARIMA", "LR","SVR RAD", "SVR LIN", "SVR Poly", "RF(100)","RF(200)", "RF(500)"),
  MAE = c(mae_arima, LINEAR_REGRESSION_MAE, svr_mae_rad, svr_mae_lin, svr_mae_poly, rf_mae_100, rf_mae_200, rf_mae_500)
)

# Bar chart to visualize the comparison
ggplot(mae_comparison_df, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Mean Absolute Error for Selected Models",
       x = "Model",
       y = "Mean Absolute Error") +
  theme_minimal() +
  scale_fill_manual(values = c("ARIMA" = "blue", "LR" = "green","SVR RAD" = "brown", "SVR LIN" = "black", "SVR Poly" = "red",
                               "RF(100)" = "pink", "RF(200)" = "yellow", "RF(500)" = "purple")) +
  theme(legend.position = "none")

# Point chart to visualize the comparison
ggplot(mae_comparison_df, aes(x = MAE, y = Model, color = Model)) +
  geom_point(size = 4) +
  labs(title = "Mean Absolute Error for Selected Models",
       x = "Mean Absolute Error",
       y = "Model") +
  theme_minimal() +
  scale_fill_manual(values = c("ARIMA" = "blue", "LR" = "green","SVR RAD" = "brown", "SVR LIN" = "black", "SVR Poly" = "red",
                               "RF(100)" = "pink", "RF(200)" = "yellow", "RF(500)" = "purple")) +
  theme(legend.position = "none")
##########################

# Bar chart with legend
ggplot(mae_comparison_df, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Mean Absolute Error for Selected Models",
       x = "Model",
       y = "Mean Absolute Error") +
  theme_minimal() +
  scale_fill_manual(name = "Models",
                    values = c("ARIMA" = "blue", "LR" = "green","SVR RAD" = "brown", "SVR LIN" = "black", "SVR Poly" = "red",
                               "RF(100)" = "pink", "RF(200)" = "yellow", "RF(500)" = "purple"),
                    labels =c("ARIMA", "LR","SVR RAD", "SVR LIN", "SVR Poly",
                              "RF(100)", "RF(200)", "RF(500)")) +
  theme(legend.position = "bottom")

# Point chart with legend
ggplot(mae_comparison_df, aes(x = MAE, y = Model, color = Model)) +
  geom_point(size = 4) +
  labs(title = "Mean Absolute Error for Selected Models",
       x = "Mean Absolute Error",
       y = "Model") +
  theme_minimal() +
  scale_color_manual(name = "Models",
                     values = c("ARIMA" = "blue", "LR" = "green","SVR RAD" = "brown", "SVR LIN" = "black", "SVR Poly" = "red",
                                "RF(100)" = "pink", "RF(200)" = "yellow", "RF(500)" = "purple"),
                     labels =c("ARIMA", "LR","SVR RAD", "SVR LIN", "SVR Poly",
                               "RF(100)", "RF(200)", "RF(500)")) +
  theme(legend.position = "bottom")
