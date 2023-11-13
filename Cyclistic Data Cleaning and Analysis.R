# DATA IMPORTING


# Load libraries necessary for all steps in the analysis proccess of Cyclistic's historical data

library(tidyverse)
library(lubridate)
library(geosphere)
library(ggmap)

# Import 12 most recent monthly datasets (August 2022 to July 2023) into RStudio using read.csv function

CyclisticData202307 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202307-divvy-tripdata.csv")
CyclisticData202306 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202306-divvy-tripdata.csv")
CyclisticData202305 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202305-divvy-tripdata.csv")
CyclisticData202304 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202304-divvy-tripdata.csv")
CyclisticData202303 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202303-divvy-tripdata.csv")
CyclisticData202302 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202302-divvy-tripdata.csv")
CyclisticData202301 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202301-divvy-tripdata.csv")
CyclisticData202212 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202212-divvy-tripdata.csv")
CyclisticData202211 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202211-divvy-tripdata.csv")
CyclisticData202210 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202210-divvy-tripdata.csv")
CyclisticData202209 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202209-divvy-publictripdata.csv")
CyclisticData202208 <- read_csv("/kaggle/input/divvy-trip-data-august-2022-july-2023/202208-divvy-tripdata.csv")

# Merge these 12 together into a new, single data frame

CyclisticMerged <- rbind(CyclisticData202307, CyclisticData202306, CyclisticData202305,
                         CyclisticData202304, CyclisticData202303, CyclisticData202302,
                         CyclisticData202301, CyclisticData202212, CyclisticData202211,
                         CyclisticData202210, CyclisticData202209, CyclisticData202208)

# View and explore merged data frame of 1 year of historical data of Cyclistic rides

head(CyclisticMerged)
glimpse(CyclisticMerged)
summary(CyclisticMerged)


# DATA CLEANING AND WRANGLING


# ride_id

# Cleaning - Remove duplicates to ensure each ride has one unique, corresponding observation/row

CyclisticMergedNoDup <- CyclisticMerged %>% 
  distinct(ride_id, .keep_all = TRUE)

print(paste("Removed", nrow(CyclisticMerged) - nrow(CyclisticMergedNoDup),
            "duplicate rows corresponding to ride_id"))

# Cleaning - check for and remove any missing values (NA)

print(paste("ride_id has", sum(is.na(CyclisticMergedNoDup$ride_id)), "missing values" ))


# rideable_type

# Transformation - convert data type from character to factor for an easier analysis later on

CyclisticMergedNoDup$rideable_type <- as.factor(CyclisticMergedNoDup$rideable_type)

class(CyclisticMergedNoDup$rideable_type)
nlevels(CyclisticMergedNoDup$rideable_type)
summary(CyclisticMergedNoDup$rideable_type)

# Change values of "docked_bike" to "classic_bike"

CyclisticMergedNoDup$rideable_type[CyclisticMergedNoDup$rideable_type == "docked_bike"] <- "classic_bike"

summary(CyclisticMergedNoDup$rideable_type)


# started_at, ended_at

# Transformation - create new column of calculated ride time by using the difftime function to find the difference
# between end time and start time of a ride

CyclisticMergedNoDup$ride_time_minutes <- (as.double(difftime(CyclisticMerged$ended_at, CyclisticMerged$started_at))) /60

summary(CyclisticMergedNoDup$ride_time_minutes)

# Cleaning - removing outliers

# Seperate ride_time_minutes into quantiles to see how ride time is distrubuted and then how to filter values

quantile_ride_time = quantile(CyclisticMergedNoDup$ride_time_minutes, seq(0, 1, by=0.01))
quantile_ride_time

CyclisticMergedNoDup <- CyclisticMergedNoDup %>% 
  filter(ride_time_minutes >=  1) %>% # negative ride times and rides times under 60 seconds
  filter(ride_time_minutes <= 180) # 3 hours, exlude large ride times to not skew mean

summary(CyclisticMergedNoDup$ride_time_minutes)

# Parsing - Extracting the month, weekday, and hour of each ride from started_at into their own respective variables,
# using strftime and floor_date functions from lubridate package

CyclisticMergedNoDup <- CyclisticMergedNoDup %>% 
  mutate(month = strftime(CyclisticMergedNoDup$started_at, "%B"),
         weekday = strftime(CyclisticMergedNoDup$started_at, "%A"))

CyclisticMergedNoDup <- CyclisticMergedNoDup %>% 
  mutate(hour_started = floor_date(started_at, unit = "hour")) 
CyclisticMergedNoDup <- CyclisticMergedNoDup %>% 
  mutate(hour_started = strftime(CyclisticMergedNoDup$hour_started,"%R"))

head(CyclisticMergedNoDup)

# Transformation - Change data type of new columns from character to factor in order for an easier analysis later on

CyclisticMergedNoDup$month <- as.factor(CyclisticMergedNoDup$month)                          
CyclisticMergedNoDup$weekday <- as.factor(CyclisticMergedNoDup$weekday)                          
CyclisticMergedNoDup$hour_started <- as.factor(CyclisticMergedNoDup$hour_started)

class(CyclisticMergedNoDup$month)
nlevels(CyclisticMergedNoDup$month)
summary(CyclisticMergedNoDup$month)
class(CyclisticMergedNoDup$weekday)
nlevels(CyclisticMergedNoDup$weekday)
summary(CyclisticMergedNoDup$weekday)
class(CyclisticMergedNoDup$hour_started)
nlevels(CyclisticMergedNoDup$hour_started)
summary(CyclisticMergedNoDup$hour_started)


# start_station_name, end_station_name, start_station_id, end_station_id

# Transformation - Change data types of columns from character to factor

CyclisticMergedNoDup$start_station_name <- as.factor(CyclisticMergedNoDup$start_station_name)                          
CyclisticMergedNoDup$end_station_name <- as.factor(CyclisticMergedNoDup$end_station_name)                          
CyclisticMergedNoDup$start_station_id <- as.factor(CyclisticMergedNoDup$start_station_id)                          
CyclisticMergedNoDup$end_station_id <- as.factor(CyclisticMergedNoDup$end_station_id)

class(CyclisticMergedNoDup$start_station_name)
nlevels(CyclisticMergedNoDup$start_station_name)
class(CyclisticMergedNoDup$end_station_name)
nlevels(CyclisticMergedNoDup$end_station_name)
class(CyclisticMergedNoDup$start_station_id)
nlevels(CyclisticMergedNoDup$start_station_id)
class(CyclisticMergedNoDup$end_station_id)
nlevels(CyclisticMergedNoDup$end_station_id)


# start_lat, start_lng, end_lat, end_lng

# Transformation - Create column of calculated distance between start and end points with the coordinate values 
# contained in these 4 variables, use distGeo function

CyclisticMergedNoDup$distance_between_stations_km <- 
  distGeo(matrix(c(CyclisticMergedNoDup$start_lng,CyclisticMergedNoDup$start_lat), ncol = 2),
          matrix(c(CyclisticMergedNoDup$end_lng,CyclisticMergedNoDup$end_lat), ncol = 2)) / 1000

summary(CyclisticMergedNoDup$distance_between_stations_km)

# Cleaning - outliers, NA's

CyclisticMergedNoDup <- CyclisticMergedNoDup %>%
  arrange(desc(distance_between_stations_km))

head(CyclisticMergedNoDup$distance_between_stations_km, n = 10)

# After arranging the data frame by distance, sorted from largest to smallest values, 8 outliers with illogical 
# values have been identified to be removed

CyclisticMergedNoDup <- CyclisticMergedNoDup %>% 
  slice(-(1:8))

head(CyclisticMergedNoDup$distance_between_stations_km, n = 10)
summary(CyclisticMergedNoDup$distance_between_stations_km)


# member_casual

# Transformation - Change data type from character to factor

CyclisticMergedNoDup$member_casual <- as.factor(CyclisticMergedNoDup$member_casual)     

class(CyclisticMergedNoDup$member_casual)
nlevels(CyclisticMergedNoDup$member_casual)
summary(CyclisticMergedNoDup$member_casual)


# Final Check Before Analysis

# Look through the summary of cleaned data frame before performing analysis
# Check missing values for each variable, explore further, decide to keep/remove/replace and when

summary(CyclisticMergedNoDup)

print(paste("ride_id has", sum(is.na(CyclisticMergedNoDup$ride_id)), "missing values"))
print(paste("rideable_type has", sum(is.na(CyclisticMergedNoDup$rideable_type)), "missing values"))
print(paste("started_at has", sum(is.na(CyclisticMergedNoDup$started_at)), "missing values"))
print(paste("ended_at has", sum(is.na(CyclisticMergedNoDup$ended_at)),"missing values"))
print(paste("start_station_name has", sum(is.na(CyclisticMergedNoDup$start_station_name)),"missing values"))
print(paste("start_station_id has", sum(is.na(CyclisticMergedNoDup$start_station_id)),"missing values"))
print(paste("end_station_name has", sum(is.na(CyclisticMergedNoDup$end_station_name)),"missing values"))
print(paste("end_station_id has", sum(is.na(CyclisticMergedNoDup$end_station_id)),"missing values"))
print(paste("start_lat has", sum(is.na(CyclisticMergedNoDup$start_lat)),"missing values"))
print(paste("start_lng has", sum(is.na(CyclisticMergedNoDup$start_lng)),"missing values"))
print(paste("end_lat has", sum(is.na(CyclisticMergedNoDup$end_lat)),"missing values"))
print(paste("end_lng has", sum(is.na(CyclisticMergedNoDup$end_lng)),"missing values"))
print(paste("member_casual has", sum(is.na(CyclisticMergedNoDup$member_casual)),"missing values"))
print(paste("ride_time_minutes has", sum(is.na(CyclisticMergedNoDup$ride_time_minutes)),"missing values"))
print(paste("month has", sum(is.na(CyclisticMergedNoDup$month)),"missing values"))
print(paste("weekday has", sum(is.na(CyclisticMergedNoDup$weekday)),"missing values"))
print(paste("hour_started has", sum(is.na(CyclisticMergedNoDup$hour_started)),"missing values"))
print(paste("distance_between_stations_km has", sum(is.na(CyclisticMergedNoDup$distance_between_stations_km)),"missing values"))

# Variables containing station data have significant amount of missing values.
# Subset into seperate data set to explore further.

NAinSSN <- subset(CyclisticMergedNoDup, is.na(CyclisticMergedNoDup$start_station_name))
NAinSSI <- subset(CyclisticMergedNoDup, is.na(CyclisticMergedNoDup$start_station_id))
NAinESN <- subset(CyclisticMergedNoDup, is.na(CyclisticMergedNoDup$end_station_name))
NAinESI <- subset(CyclisticMergedNoDup, is.na(CyclisticMergedNoDup$end_station_id))

head(NAinSSN)
head(NAinSSI)
head(NAinESN)
head(NAinESI)

summary(NAinSSN$rideable_type)
summary(NAinSSI$rideable_type)
summary(NAinESN$rideable_type)
summary(NAinESI$rideable_type)

# Drop columns that will not be used in your analysis and visualization

CyclisticClean <- CyclisticMergedNoDup %>% 
  select(-c(started_at, ended_at))

head(CyclisticClean)
summary(CyclisticClean)


# DATA ANALYSIS AND VISUALIZATION


# summarise the data by the number of rides taken by each User type to see how they compare.

member_casual_summary <- CyclisticClean %>% 
  group_by(member_casual) %>% 
  summarise(count = n(),  percentage = round(length(ride_id)/nrow(CyclisticClean)*100,2), 
  .groups = "drop")

options(scipen = 999, repr.plot.width = 11, repr.plot.height = 8)

ggplot(member_casual_summary, aes(x = " " , y = count, fill = member_casual)) + 
  geom_col() + labs(title = "Number of Rides Taken by User Type (Aug 2022 - July 2023)") + 
  geom_text(aes(label = paste(count," --- ", percentage ,"%")), position = position_stack(vjust = 0.5), 
  size = 6) + theme_void(base_size = 19) + scale_fill_discrete(name = "User Type") + coord_polar(theta = "y")

# look into the differences in ride count by grouping the counts by the Month that the rides took place

CyclisticClean$month <- ordered(CyclisticClean$month, levels = 
                                  c("August", "September", "October", "November", "December", "January",
                                    "February", "March", "April", "May", "June", "July"))

month_member_casual_summary <- CyclisticClean %>% 
  group_by(month, member_casual) %>%
  summarise(count = n(), .groups = "drop")%>%
  arrange(month, member_casual)

options(repr.plot.width = 16, repr.plot.height = 8)

ggplot(data = month_member_casual_summary, aes(x = month, y = count, fill = member_casual),
  show.legend = FALSE) + geom_col(position = "dodge", width = 0.6) +
  labs(title = "Monthly Ride Count by User Type (Aug 2022 - July 2023)", x = "Month", y = "# of Rides") + 
  scale_fill_discrete(name="User Type") + theme_gray(base_size = 19)

# group ride counts by the days of the week

CyclisticClean$weekday <- ordered(CyclisticClean$weekday, levels = 
                                    c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                      "Friday", "Saturday", "Sunday"))

weekday_member_casual_summary <- CyclisticClean %>% 
  group_by(weekday, member_casual) %>%
  summarise(count = n(), .groups = "drop")%>%
  arrange(weekday, member_casual)

ggplot(data = weekday_member_casual_summary, aes(x = weekday, y = count, fill = member_casual),
  show.legend = FALSE) + geom_col(position = "dodge", width = 0.8) +
  labs(title = "Ride Count by Day of the Week (Aug 2022 - July 2023)", x = "Day", y = "# of Rides") + 
  scale_fill_discrete(name="User Type") + theme_gray(base_size = 19) + geom_text(aes(label = count),
  size = 5, vjust = -0.25, position = position_dodge(width = 0.8))

# time of day these rides are taking place for both the workweek and weekend

hour_member_casual_summary <- CyclisticClean %>% 
  filter(weekday == "Monday" | weekday == "Tuesday"| weekday == "Wednesday" |
         weekday == "Thursday" |weekday == "Friday") %>% 
  group_by(hour_started, member_casual) %>%
  summarise(count = n(), .groups = "drop")%>%
  arrange(hour_started, member_casual)

ggplot(data = hour_member_casual_summary, aes(x = hour_started, y = count, group = member_casual, 
  color = member_casual)) + geom_point() + geom_line() +
  labs(title = "Ride Count by Time of Day on Weekdays (Aug 2022 - July 2023)", x = "Hour", y = "# of Rides") + 
  guides(color = guide_legend(title = "User Type")) + 
  theme(text = element_text(size = 19),axis.text.x = element_text(angle = 90))

hour_member_casual_summary1 <- CyclisticClean %>% 
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(hour_started, member_casual) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(hour_started, member_casual)

ggplot(data = hour_member_casual_summary1, aes(x = hour_started, y = count, group = member_casual,
  color = member_casual)) + geom_point() + geom_line() +
  labs(title = "Ride Count by Time of Day on Weekends (Aug 2022 - July 2023)", x = "Hour",
  y = "# of Rides") + guides(color = guide_legend(title = "User Type")) + 
  theme(text = element_text(size = 19),axis.text.x = element_text(angle = 90))

# ride time of our users

ride_time_summary <- CyclisticClean %>%
  group_by(member_casual) %>%
  summarise(mean = round(mean(ride_time_minutes),1), median = round(median(ride_time_minutes),1), .groups = "drop")

options(repr.plot.width = 10, repr.plot.height = 8)

ggplot(data = ride_time_summary) + 
  geom_col(mapping = aes(x = member_casual , y = mean, fill = member_casual), show.legend = FALSE) +
  labs(title = "Mean Ride Time By User Type (Aug 2022 - July 2023)", x = "User Type", 
  y = "Mean Ride Time (minutes)") + geom_text(aes(x = member_casual, y = mean, label = mean), 
  vjust = -0.25, size = 5) + theme(text = element_text(size = 19))

# grouping the ride time by time variables: month, weekday, hour_started

weekday_member_casual_summary3 <- CyclisticClean %>%
  group_by(month, member_casual) %>%
  summarise(mean = round(mean(ride_time_minutes),1), median = round(median(ride_time_minutes),1), .groups = "drop")

options(repr.plot.width = 16, repr.plot.height = 8)

ggplot(data = weekday_member_casual_summary3, aes(x = month , y = mean, fill = member_casual), show.legend = FALSE) + 
  geom_col(position = "dodge", width = 0.7) + 
  labs(title = "Mean Ride Time by Month (Aug 2022 - July 2023)", x = "Month",y = "Mean Ride Time (minutes)") + 
  scale_fill_discrete(name="User Type") + theme_gray(base_size = 19) + geom_text(aes(label = mean),
  size = 5, vjust = -0.25, position = position_dodge(width = 0.7))

weekday_member_casual_summary2 <- CyclisticClean %>%
  group_by(weekday, member_casual) %>%
  summarise(mean = round(mean(ride_time_minutes),1), median = round(median(ride_time_minutes),1), .groups = "drop")

ggplot(data = weekday_member_casual_summary2, aes(x = weekday , y = mean, fill = member_casual), 
  show.legend = FALSE) + geom_col(position = "dodge", width = 0.8) + 
  labs(title = "Mean Ride Time by Day of the Week (Aug 2022 - July 2023)", 
  x = "Day",y = "Mean Ride Time (minutes)") + scale_fill_discrete(name="User Type") + 
  theme_gray(base_size = 19) + geom_text(aes(label = mean), size = 5, vjust = -0.25, 
  position = position_dodge(width = 0.8))

ride_time_summary_week <- CyclisticClean %>%
  filter(weekday == "Monday" | weekday == "Tuesday" | weekday == "Wednesday" |
  weekday == "Thursday" | weekday == "Friday") %>% 
  group_by(hour_started, member_casual) %>%
  summarise(mean = round(mean(ride_time_minutes),1), median = round(median(ride_time_minutes),1), .groups = "drop")

ride_time_summary_weekend <- CyclisticClean %>%
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(hour_started, member_casual) %>%
  summarise(mean = round(mean(ride_time_minutes),1), median = round(median(ride_time_minutes),1), .groups = "drop")

ggplot(data = ride_time_summary_week, aes(x = hour_started , y = mean, group = member_casual, 
  color = member_casual)) + geom_line() + geom_point() + 
  labs(title = "Mean Ride Time by Time of Day (Weekdays, Aug 2022 - July 2023)",
  x = "Hour", y = "Mean Ride Time (minutes)") + theme(axis.text.x = element_text(angle = 90), 
  text = element_text(size = 19)) + guides(color = guide_legend(title = "User Type")) + 
  geom_text(aes(x = hour_started, y = mean, label = mean), vjust = -1, size = 4)

ggplot(data = ride_time_summary_weekend, aes(x = hour_started , y = mean, group = member_casual,
  color = member_casual)) + geom_line() +  geom_point() +
  labs(title = "Mean Ride Time by Time of Day (Weekends, Aug 2022 - July 2023)", x = "Hour", 
  y = "Mean Ride Time (minutes)") + theme(axis.text.x = element_text(angle = 90), 
  text = element_text(size = 19)) + guides(color = guide_legend(title = "User Type")) + 
  geom_text(aes(x = hour_started, y = mean, label = mean), vjust = -1, size = 4)

# dive into the Cyclistics bike types and summarize how frequently they're being used by Cyclistic riders

CyclisticClean$rideable_type <- ordered(CyclisticClean$rideable_type, levels = 
                                          c("electric_bike", "classic_bike"))

rideable_type_summary <- CyclisticClean %>%
  group_by(member_casual, rideable_type) %>%
  summarise(count = n(), .groups = "drop") 

options(repr.plot.width = 16, repr.plot.height = 8)

ggplot(data = rideable_type_summary, aes(x = member_casual, y = count, fill = rideable_type)) + 
  geom_col(position = "stack") + labs(title = "Ride Count by Bike Type (Aug 2022 - Jul 2023)", 
  x = "User Type", y = "# of Rides") +  scale_fill_discrete(name="Bike Type") + 
  theme_gray(base_size = 19) + coord_flip() + geom_text(aes(label = count), size = 5, 
  position = position_stack(vjust = 0.5))

# perform some multivariate analysis on rideable_type to get a clearer picture on how each User group uses each bike

rideable_type_summary3 <- CyclisticClean %>%
  filter(rideable_type == "electric_bike" | rideable_type == "classic_bike") %>%
  group_by(month, rideable_type, member_casual) %>%
  summarise(count = n(), .groups = "drop")

options(repr.plot.width = 16, repr.plot.height = 16)

ggplot(data = rideable_type_summary3, aes(x = month, y = count, fill = rideable_type)) +
  geom_col(position = "stack") + 
  labs(title = "Ride Count by Bike Type (Months, Aug 2022 - Jul 2023)", x = "Bike Type", y = "# of Rides") +  
  scale_fill_discrete(name = "Bike Type") + facet_wrap(~member_casual, nrow = 2) +
  theme_gray(base_size = 19) + geom_text(aes(label = count), size = 4.5, position = position_stack(vjust = 0.9))

rideable_type_summary2 <- CyclisticClean %>%
  filter(rideable_type == "electric_bike" | rideable_type == "classic_bike") %>%
  group_by(weekday, rideable_type, member_casual) %>%
  summarise(count = n(), .groups = "drop")

options(repr.plot.width = 16, repr.plot.height = 12)

ggplot(data = rideable_type_summary2, aes(x = member_casual, y = count, fill = rideable_type)) +
  geom_col(position = "stack") + labs(title = "Ride Count by Bike Type (Weeks, Aug 2022 - Jul 2023)", 
  x = "User Type", y = "# of Rides") + scale_fill_discrete(name="Bike Type") +
  facet_wrap(~weekday) + theme(axis.text.x = element_text(angle = 25)) +  coord_flip() + theme_gray(base_size = 19) + 
  geom_text(aes(label = count), size = 5, position = position_stack(vjust = 0.5))

rideable_type_summary_weekday <- CyclisticClean %>%
  filter(weekday == "Monday" | weekday == "Tuesday"| weekday == "Wednesday" | weekday == "Thursday" |
  weekday == "Friday") %>% 
  group_by(hour_started, rideable_type, member_casual) %>%
  summarise(count = n(), .groups = "drop")

rideable_type_summary_weekend <- CyclisticClean %>%
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(hour_started, rideable_type, member_casual) %>% 
  summarise(count = n(), .groups = "drop")

ggplot(data = rideable_type_summary_weekday, aes(x = hour_started, y = count, group = rideable_type, 
  color = rideable_type)) + geom_line() + geom_point() +
  labs(title = "Daily Ride Count by Bike Type (Weekdays, Aug 2022 - Jul 2023)", x = "Time of Day",
  y = "# of Rides") + scale_fill_discrete(name="Bike Type") + facet_wrap(~member_casual, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 19)) + 
  guides(color = guide_legend(title = "Bike Type")) 

ggplot(data = rideable_type_summary_weekend, aes(x = hour_started, y = count, group = rideable_type, 
                                                 color = rideable_type)) + geom_line() + geom_point() + 
  labs(title = "Daily Ride Count by Bike Type (Weekends, Aug 2022 - Jul 2023)", x = "Time of Day", 
  y = "# of Rides") + scale_fill_discrete(name="Bike Type") + facet_wrap(~member_casual, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 19)) + 
  guides(color = guide_legend(title = "Bike Type"))

# look at the mean ride time for each bike type and group the data by User type

rideable_type_summary5 <- CyclisticClean %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(mean = round(mean(ride_time_minutes),2), .groups = "drop")

options(repr.plot.width = 10, repr.plot.height = 8)

ggplot(data = rideable_type_summary5, aes(x = rideable_type, y = mean, fill = rideable_type)) +
  geom_col() + labs(title = "Mean Ride Time by Bike Type (Aug 2022 - Jul 2023)", x = "Bike Type",
  y = "Ride Time (minutes)") + facet_wrap(~member_casual) + geom_text(aes(x = rideable_type,
  y = mean, label = mean), vjust = -0.25) + theme(legend.position = "none", text = element_text(size = 19))

# analyze classic bike trip duration in more detail by incorporating some time variables

ridetime_summary_biketype1 <- CyclisticClean %>%  
  filter(member_casual == "casual") %>% 
  group_by(rideable_type, month) %>% 
  summarise(mean = round(mean(ride_time_minutes),1), .groups = "drop")

options(repr.plot.width = 16, repr.plot.height = 8)

ggplot(data = ridetime_summary_biketype1, aes(x = month , y = mean, fill = rideable_type), 
  show.legend = FALSE) + geom_col(position = "dodge", width = 0.7) + 
  labs(title = "Mean Ride Time by Bike Type for Casual Users (Aug 2022 - July 2023)", x = "Month",
  y = "Mean Ride Time (minutes)") + scale_fill_discrete(name="Bike Type") + theme_gray(base_size = 19) + 
  geom_text(aes(label = mean), size = 5, vjust = -0.25, position = position_dodge(width = 0.7))

ridetime_summary_biketype2 <- CyclisticClean %>%  
  filter(member_casual == "casual") %>%
  filter(weekday == "Saturday" | weekday == "Sunday") %>%
  group_by(rideable_type, hour_started) %>% 
  summarise(mean = mean(ride_time_minutes), .groups = "drop")

ridetime_summary_biketype3 <- CyclisticClean %>%  
  filter(member_casual == "casual") %>%
  filter(!weekday == "Saturday" | !weekday == "Sunday") %>%
  group_by(rideable_type, hour_started) %>% 
  summarise(mean = mean(ride_time_minutes), .groups = "drop")

ggplot(data = ridetime_summary_biketype2, aes(x = hour_started, y = mean, group = rideable_type, 
  color = rideable_type)) + geom_line() + geom_point() +
  labs(title = "Mean Ride Time by Bike Type for Casual Users (Weekends, Aug 2022 - July 2023)", x = "Hour", 
  y = "Ride Time (minutes)") + theme(axis.text.x = element_text(angle = 90), 
  text = element_text(size = 19)) + guides(color = guide_legend(title = "Bike Type"))

ggplot(data = ridetime_summary_biketype3, aes(x = hour_started, y = mean, group = rideable_type, 
  color = rideable_type)) + geom_line() + geom_point() + 
  labs(title = "Mean Ride Time by Bike Type for Casual Users (Weekends, Aug 2022 - July 2023)", x = "Hour", 
  y = "Ride Time (minutes)") + theme(axis.text.x = element_text(angle = 90), 
  text = element_text(size = 19)) + guides(color = guide_legend(title = "Bike Type"))

# analyze the distance between users starting and ending positions
# Rides where distance is 0 km indicates a trip where the user returned to their starting position (round trip)
# subset data to where this occurs and visualize to explore further

distance_summary <- CyclisticClean %>% 
  drop_na(distance_between_stations_km) 

distance0 <- distance_summary %>% 
  filter(distance_between_stations_km == 0) %>% 
  filter(rideable_type == "classic_bike")

dist0_summary <- distance0 %>%  
  group_by(member_casual) %>% 
  summarise(count = n(), percentage = round(length(ride_id)/nrow(distance0)*100,2), .groups = "drop")

options(repr.plot.width = 10, repr.plot.height = 8)

ggplot(dist0_summary, aes(x= " ", y = count, fill = member_casual)) +
  geom_col() + coord_polar(theta = "y") + theme_void(base_size = 19) +
  geom_text(aes(label = paste(count, " --- ", percentage,"%")),
  position = position_stack(vjust = 0.5), size = 6) + 
  labs(title ="Count of Round Trips for Classic Bikes by User Type", 
  subtitle = "(Aug 2022 - July 2023)") + scale_fill_discrete(name = "User Type") 

distance0casual <- distance0 %>% 
  filter(member_casual == "casual")

summary(distance0$ride_time_minutes)
summary(distance0casual$ride_time_minutes)

# look at round trip occurences in relation to the time variables

distance0_2 <- distance0 %>%  
  group_by(month, member_casual) %>% 
  summarise(mean = mean(ride_time_minutes), count = n(), 
  percentage = round(length(ride_id)/nrow(distance0)*100,2), .groups = "drop")

options(repr.plot.width = 16, repr.plot.height = 8)

ggplot(data = distance0_2, aes(x = month, y = count, group = member_casual, color = member_casual)) +
  geom_line() + geom_point() +
  labs(title = "Count of Round Trips for Classic Bikes by Month (Aug 2022 - July 2023)", x = "Month",
  y = "# of Rides") + theme(axis.text.x = element_text(angle = 25), text = element_text(size = 19)) + 
  guides(color = guide_legend(title = "User Type")) + geom_text(aes(x = month, y = count, label = count),
  size = 4.5, vjust = -0.35)

distance0_1 <- distance0 %>% 
  group_by(weekday, member_casual) %>% 
  summarise(mean = mean(ride_time_minutes), count = n(), percentage = round(length(ride_id)/nrow(distance0)*100,2),
            .groups = "drop")

ggplot(data = distance0_1, aes(x = weekday, y = count, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.8) + scale_fill_discrete(name="User Type") +
  labs(title = "Count of Round Trips for Classic Bikes by Weekday (Aug 2022 - July 2023)", x = "Day of Week",
  y = "# of Rides") + theme_gray(base_size = 19) + geom_text(aes(x = weekday, y = count, label = count),
  size = 5, position = position_dodge(width = 0.8), vjust = -0.25) 

distance0_3 <- distance0 %>% 
  filter(weekday == "Monday" | weekday == "Tuesday" | weekday == "Wednesday" | weekday == "Thursday" |
  weekday == "Friday") %>% 
  group_by(hour_started, member_casual) %>% 
  summarise(mean = mean(ride_time_minutes), count = n(), .groups = "drop")

distance0_4 <- distance0 %>% 
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(hour_started, member_casual) %>% 
  summarise(mean = mean(ride_time_minutes), count = n(), .groups = "drop")

ggplot(data = distance0_3, aes(x = hour_started, y = count, group = member_casual, color = member_casual)) + 
  geom_line() + geom_point() +
  labs(title = "Count of Round Trips for Classic Bikes by Time of Day (Weekdays, Aug 2022 - July 2023)", 
  x = "Time of Day", y = "# of Rides") + theme(axis.text.x = element_text(angle = 90),
  text = element_text(size = 19)) + guides(color = guide_legend(title = "User Type"))

ggplot(data = distance0_4, aes(x = hour_started, y = count, group = member_casual, color = member_casual)) + 
  geom_line() + geom_point() +
  labs(title = "Count of Round Trip Rides for Classic Bikes by Time of Day (Weekends, Aug 2022 - July 2023)",
  x = "Time of Day", y = "# of Rides") + theme(axis.text.x = element_text(angle = 90), 
  text = element_text(size = 19)) + guides(color = guide_legend(title = "User Type"))

# compare summary stats for distance_between_stations_km for each group

distance_summary_casual <- distance_summary %>% 
  filter(member_casual =="casual")

distance_summary_member <- distance_summary %>% 
  filter(member_casual =="member")

print(paste("distance_between_stations_km - CASUAL:"))
summary(distance_summary_casual$distance_between_stations_km)
print(paste("distance_between_stations_km - MEMBER:"))
summary(distance_summary_member$distance_between_stations_km)

# group means by some additional variables (bike type, time components) to see if we can discover any differences

distance_summary_biketype <- distance_summary %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(mean = round(mean(distance_between_stations_km),2), .groups = "drop")

ggplot(data = distance_summary_biketype, aes(x= rideable_type, y = mean, fill = rideable_type)) + 
  geom_col() + facet_wrap(~member_casual) +  
  geom_text(aes(x = rideable_type, y = mean, label = mean), vjust = -0.25, size = 5) +
  theme(legend.position = "none", text = element_text(size = 19)) + 
  labs(title = "Mean Distance Between Start and End Point (Aug 2022 - July 2023)", x = "Bike Type", 
  y = "Distance Between Start/End Point (km)") 

distance_summary_weekday <- distance_summary %>% 
  filter(weekday == "Monday" | weekday == "Tuesday"| weekday == "Wednesday" |
  weekday == "Thursday" |weekday == "Friday") %>% 
  group_by(hour_started, member_casual) %>%
  summarise(mean = round(mean(distance_between_stations_km),2), .groups = "drop")

distance_summary_weekends <- distance_summary %>% 
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(hour_started, member_casual) %>%
  summarise(mean = round(mean(distance_between_stations_km),2), .groups = "drop")

ggplot(data = distance_summary_weekday, aes(x= hour_started, y = mean, group = member_casual, 
                                            color = member_casual)) + geom_line() + geom_point() + 
  labs(title = "Mean Distance Between Start and End Point (Weekdays, Aug 2022 - July 2023)",
  x = "Time of Day", y = "Distance Between Start/End Point (km)") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 19)) + ylim(1.5,3) +
  guides(color = guide_legend(title = "User Type"))

ggplot(data = distance_summary_weekends, aes(x= hour_started, y = mean, group = member_casual, 
                                             color = member_casual)) + geom_line() + geom_point() + theme_gray(base_size = 19) + 
  labs(title = "Mean Distance Between Start and End Point (Weekends, Aug 2022 - July 2023)",
  x = "Time of Day", y = "Distance Between Start/End Point (km)") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 19)) + ylim(1.5,3) +
  guides(color = guide_legend(title = "User Type"))

# visualize where in the City of Chicago our riders are using our bikes.

# Group the data by starting and ending coordinates and filter the highest counts to create a summary of the 
# most popular routes.

# create a summary for the most popular coordinates where round trips take place.

route_summary_dist0 <- distance0 %>% 
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual) %>%
  summarise(total = n(),.groups="drop") %>%
  filter(total>100) %>% 
  arrange(desc(total))

route_summary <- distance_summary %>% 
  filter(!distance_between_stations_km == 0) %>% 
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual) %>%
  summarise(total = n(),.groups="drop") %>%
  filter(total>200) %>% 
  arrange(desc(total))

# code for updating ggmap to access tiles from stadiamaps:


#remove.packages("ggmap")
#install.packages("devtools")
#devtools::install_github("stadiamaps/ggmap")
#register_stadiamaps("your-api-key-here")
#library(ggmap)

# create the following:

# Geospatial viz highlighting the most popular stations used for round trips for each User type
# Geospatial viz highlighting the most popular routes, excluding round trips for each User type

# source for defining coordinates of bbox arguement : https://www.openstreetmap.org/export#map=10/41.8338/-87.7327

chicago <- c(left = -87.77, bottom = 41.75, right = -87.55, top = 42.10)
chimap <- get_stadiamap(chicago, zoom = 12, maptype = "alidade_smooth_dark")

ggmap(chimap) +
  geom_point(route_summary_dist0, mapping = aes(x = start_lng, y = start_lat, size = total, 
  color = member_casual, alpha = 0)) +
  labs(title = "Popular Stations for Round Trips (Classic Bikes, Ride Count>100)", x=NULL, y=NULL) +
  theme(legend.position="none") + facet_wrap(~member_casual) + scale_size(range = c(1,8)) + coord_fixed(0.7)

ggmap(chimap) +
  geom_segment(route_summary, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, color = member_casual)) +
  labs(title = "Popular Routes, Excluding Round Trips (Ride Count>200)", x=NULL, y=NULL) + coord_fixed(0.7) +
  theme(legend.position="none") + facet_wrap(~member_casual)