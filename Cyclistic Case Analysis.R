library(tidyverse)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
q1_2019 <- read_csv("Divvy 2019 Q1.csv")
q1_2020 <- read_csv("Divvy 2020 Q1.csv")

colnames(q1_2019)
colnames(q1_2020)

#Rename columns to match Q1 2019
(q1_2019 <- rename(q1_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  ))
(q1_2019 <- rename(q1_2019,
                   member_casual = usertype
                   ))

str(q1_2019)
str(q1_2020)

# Change the data type of ride_id and rideable_type to character type.
(q1_2019 <- mutate(q1_2019,
                   ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type))
)
str(q1_2020)

#Combine q1_2019 and q1_2020
all_trips <- bind_rows(q1_2019, q1_2020)

#remove columns that are not in common
all_trips <- all_trips %>%
  select(-c(gender, birthyear, start_lat, start_lng, end_lat, end_lng, tripduration))

#Descriptive Analysis
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#table helps identify the count of each unique data input for a column
table(all_trips$member_casual)

#adjust member_casual to only have member or casual as possible data entries
all_trips <- all_trips %>%
            mutate(member_casual = recode(member_casual,
                                         "Subscriber" = "member",
                                         "Customer" = "casual"))
table(all_trips$member_casual)

#Separate the date, month, and year for the starting date of each trip
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$started_at), "%m")
all_trips$year <- format(as.Date(all_trips$started_at), "%Y")
all_trips$day <- format(as.Date(all_trips$started_at), "%d")
all_trips$day_of_week <- format(as.Date(all_trips$started_at), "%A")

#calculate length of the rides in new column ride_length
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)
#change data type of ride_length from factor to numeric
all_trips$ride_length <- as.numeric(all_trips$ride_length)
#check if numeric
is.numeric(all_trips$ride_length)

#Remove bad data where bikes taken out for service, as well as when ride times are negative. Do this by creating a new table. 
#Use Square brackets to extract elements from the table. ! means NOT
all_trips_v2 <- all_trips[!(all_trips$ride_length < 0 | all_trips$start_station_name == "HQ QR"),]

#Descriptive Anlaysis for ride length
summary(all_trips_v2$ride_length)

#Compare Casual and Member Rides.
#aggregate() creates subset, ~ establishes relationship, FUN means function
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

#now compare by day of the week as well
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
#order table by day of week. use ordered() and levels.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#analyze ridership by type and weekday, create visualizations.
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #for label: TRUE shows the day of the week as a character while FALSE shows the day of the week as a number
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%  #order by
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") #position dodge is used so that groups of bars are displayed beside each other instead of on-top of each other.

#visualization for ride length
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #for label: TRUE shows the day of the week as a character while FALSE shows the day of the week as a number
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%  #order by
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") #position dodge is used so that groups of bars are displayed beside each other instead of on-top of each other.

ride_file <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, weekday)
write_csv(ride_file, file = "number_of_rides.csv")
  

#saving table as a csv
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write_csv(counts, file="avg_ride_length.csv")










