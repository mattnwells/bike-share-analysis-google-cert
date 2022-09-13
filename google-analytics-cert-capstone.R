#--------------- INSTALL PACKAGES AND LOAD LIBRARIES ---------------

#install packages
install.packages("tidyverse", dependencies = TRUE) 
install.packages("lubridate")
install.packages("hms")
install.packages("data.table")

#load libraries
library(tidyverse) #data cleaning
library(lubridate) #date manipulation
library(hms) #time manipulation
library(data.table) #exporting data

#--------------- DATA PREPARATION ----------------

#----------load data ----------

#load 12 months data
data_2021_06 <- read.csv("202106-divvy-tripdata.csv")
data_2021_07 <- read.csv("202107-divvy-tripdata.csv")
data_2021_08 <- read.csv("202108-divvy-tripdata.csv")
data_2021_09 <- read.csv("202109-divvy-tripdata.csv")
data_2021_10 <- read.csv("202110-divvy-tripdata.csv")
data_2021_11 <- read.csv("202111-divvy-tripdata.csv")
data_2021_12 <- read.csv("202112-divvy-tripdata.csv")
data_2022_01 <- read.csv("202201-divvy-tripdata.csv")
data_2022_02 <- read.csv("202202-divvy-tripdata.csv")
data_2022_03 <- read.csv("202203-divvy-tripdata.csv")
data_2022_04 <- read.csv("202204-divvy-tripdata.csv")
data_2022_05 <- read.csv("202205-divvy-tripdata.csv")

#merge files into a single df 
data_12_months <- rbind(data_2021_06, data_2021_07, data_2021_08, data_2021_09, data_2021_10, data_2021_11, data_2021_12, data_2022_01, data_2022_02, data_2022_03, data_2022_04, data_2022_05) 

#drop monthly files from the environment
remove(data_2021_06, data_2021_07, data_2021_08, data_2021_09, data_2021_10, data_2021_11, data_2021_12, data_2022_01, data_2022_02, data_2022_03, data_2022_04, data_2022_05)

#copy df
data_12_months_2 <- data_12_months  

#--------------- DATA PROCESSING ---------------

#create column ride_length in minutes
data_12_months_2$ride_length <- difftime(data_12_months_2$ended_at, data_12_months_2$started_at, units = "mins")

#create column day_of_week in text
data_12_months_2$day_of_week <- wday(data_12_months_2$started_at)

#create numeric columns for the day of week, date, month, day, year
data_12_months_2$date <- as.Date(data_12_months_2$started_at)
data_12_months_2$day_of_week <- format(as.Date(data_12_months_2$date), "%A")
data_12_months_2$month <- format(as.Date(data_12_months_2$date), "%m") 
data_12_months_2$day <- format(as.Date(data_12_months_2$date), "%d")
data_12_months_2$year <- format(as.Date(data_12_months_2$date), "%Y")

#create column for ride season classification
data_12_months_2 <- data_12_months_2 %>% mutate(season = 
  case_when(month == "03" ~ "Spring",
            month == "04" ~ "Spring",
            month == "05" ~ "Spring",
            month == "06" ~ "Summer",
            month == "07" ~ "Summer",
            month == "08" ~ "Summer",
            month == "09" ~ "Fall",
            month == "10" ~ "Fall",
            month == "11" ~ "Fall",
            month == "12" ~ "Winter",
            month == "01" ~ "Winter",
            month == "02" ~ "Winter")
)

#---------- data cleaning ----------

#remove duplicate rows
data_12_months_2 <- distinct(data_12_months_2) 

#remove rows with null values
data_12_months_2 <- na.omit(data_12_months_2)

#remove rows where ride_length <= 0
data_12_months_2 <- data_12_months_2[!(data_12_months_2$ride_length <=0),] 

#remove unneeded columns
data_12_months_2 <- data_12_months_2 %>%  
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

#rename member_casual to member_type
data_12_months_2 <- data_12_months_2 %>% rename(member_type = member_casual)

#convert ride_length to numeric
data_12_months_2$ride_length <- as.numeric(as.character(data_12_months_2$ride_length))
is.numeric(data_12_months_2$ride_length)

#view final df
View(data_12_months_2)

#---------- data analysis ----------

#total number of rides
nrow(data_12_months_2)

#total number of rides rounded
round(nrow(data_12_months_2), digits = -5)

#count member type
data_12_months_2 %>% count(member_type)

#total rides by bike type
data_12_months_2 %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#total rides by bike type by member type 
data_12_months_2 %>%
  group_by(member_type, rideable_type) %>% 
  count(rideable_type)

#min, max, median, mean length of ride
summary(data_12_months_2$ride_length)

#---------- data visualization ----------

#converts values from scientific notation 
options(scipen = 999)

#plot bicycle type by number of rides
data_12_months_2 %>%
  group_by(rideable_type, member_type) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x=rideable_type, y=count_trips, fill=member_casual, color=member_type)) +
  geom_bar(stat='identity', position='dodge') +
  theme_bw()+
  labs(title="Number of Trips by Bicycle Type", x="Bicycle Type", y="Number of Rides")

#arrange days of week in order
data_12_months_2$day_of_week <- ordered(data_12_months_2$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#plot number of rides by day of week
data_12_months_2 %>%
  group_by(member_type,day_of_week) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= day_of_week, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Number of Rides by Day of Week", x = "Day of Week", y = "Number of Rides")

#arranges months in order
data_12_months_2$month <- ordered(data_12_months_2$month, levels=c("01", "02", "03", "04", "05", "06", "07","08","09","10","11","12"))

#plot number of rides per month
data_12_months_2 %>%
  group_by(member_type, month) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= month, y=count_trips, fill=member_type, color=member_type)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw() +
  labs(title ="Number of Rides per Month", x = "Month", y = "Number of Trips")

#Bike use by hour, by day of week
ggplot(data = data_12_months_2) +
  aes(x = started_at, fill = member_type) +
  facet_wrap(~day_of_week) +
  geom_bar() +
  labs(x = 'Start Hour', y = 'Number of Rides', fill = 'Member Type', title = 'Bike Use By Hour, By Day') +
  theme(axis.text = element_text(size = 5))

#Find popular start station for casual riders
data_12_months_2 %>%
  group_by(member_type, start_station_name) %>%
  dplyr::summarise(number_of_ride = n()) %>%
  filter(start_station_name != "", "casual"== member_type) %>%
  arrange(-number_of_ride) %>%
  head(n=5) %>%
  select(-member_type)

#Find popular start station for member riders
data_12_months_2 %>%
  group_by(member_type,start_station_name) %>%
  dplyr::summarise(number_of_ride = n()) %>%
  filter(start_station_name != "", "member" == member_type) %>%
  arrange(-number_of_ride) %>%
  head(n=5) %>%
  select(-member_type)

#---------- Seasonal Use ----------

#total rides by member type
data_12_months_2 %>%
  group_by(season, member_type) %>% 
  count(season)

#total rides
data_12_months_2 %>%
  group_by(season) %>% 
  count(season)
