## Installing libraries
install.packages("tidyverse") 
install.packages("dplyr") 
install.packages("zoo")    

## Loading libraries
library(tidyverse)
library(dplyr)
library(zoo)

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

data_head =  rbind(data_head,format(date_new, "%d-%m-%Y %H:%M"))
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
 
## Exporting the dataset
 write.csv(dfs,"C:\\Users\\Chidera\\Documents\\Tutorial\\dfs.csv")
 
## Importing it back
 dfs <- read.csv("dfs.csv", all=TRUE)
 dfs
 sum(is.na(dfs))
 
 ##dfs <- as.numeric(unlist(dfs))
 
 class(dfs)
 col_name <- colnames(dfs)
 col_name
 
 ### create detect outlier function
 
 detect_outlier <- function(x) {
   
   # calculate first quantile
   Quantile1 <- quantile(x, probs=.25)
   
   # calculate third quantile
   Quantile3 <- quantile(x, probs=.75)
   
   # calculate inter quartile range
   IQR = Quantile3-Quantile1
   
   # return true or false
   x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
 }
 
 ### create check outlier function
 check_outlier <- function(dataframe,
                            columns=names(dataframe)) {
   
   # for loop to traverse in columns vector
   for (col in columns) {
     
     # remove observation if it satisfies outlier function
     dataframe <- dataframe[detect_outlier(dataframe[[col]]), ]
     
   }
   
   print(dataframe)
 }
 
 check_outlier(dfs, c(col_name))

 
 # create remove outlier function
 remove_outlier <- function(dataframe,
                            columns=names(dataframe)) {
   
   # for loop to traverse in columns vector
   for (col in columns) {
     
     # remove observation if it satisfies outlier function
     dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
   }
   
   # return dataframe
   print("Remove outliers")
   print(dataframe)
 }
 
remove_outlier(dfs, c(col_name))
 
dfs


## Subseting to get pick the lag and long
df_finals <- subset(dfs, X == 150 )
names(df_finals) <- NULL
df_finals <- df_finals[,-1]

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
colnames(df_body) <- c("weather", "date", "number")


View(df_body)

# Convert the matrix to a data frame
df_last <- as.data.frame(df_body)

sum(is.na(df_last))

# Use gsub() to remove the quotes around "ad"
df_last$weather <- gsub('"', '', df_last$weather)


df_last[duplicated(df_last), ]
# Remove duplicate rows
df_last <- unique(df_last)
df_last[duplicated(df_last), ]

df_last$number <- as.double(df_last$number)
df_last$date <- as_datetime(df_last$date)
df_last$weather <- as.factor(df_last$weather)

# View the updated data frame
df_last[duplicated(df_last), ]
df_last

# Using pivoted
weather_pivoted <- df_last %>%
  dplyr::group_by(date, weather) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)
  pivot_wider(names_from = weather, values_from = number,)
weather_pivoted <- pivot_wider(df_last, names_from = "date", values_from = "number", id_cols ="weather")

data_star <- t(weather_pivoted)

new_dataset <- data_star[,c("weather","TSK")]
new_dataset <- data_star %>% select(1)
write.csv(new_dataset,"C:\\Users\\Chidera\\Documents\\Tutorial\\new_dataset.csv")

df_last %>%
  pivot_wider(
    names_from = weather,
    values_from = number
  )
 

## Using spread
# Use the spread() function to pivot the data
weather_spread <- spread(df_last, date, number)

# View the resulting dataset
weather_spread
df_last %>%
  spread(weather, number)

spread(df_last, key=weather, value=number, fill = NA)

View(df_last)
