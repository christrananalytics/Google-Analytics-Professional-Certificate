# Name: Chris Tran 
# Date: 10/08/2022
# Objective: Determine the differences between casual riders and annual members
# Process: 
## Download and store individual datasets appropriately
## Perform data cleaning
## Ensure data is efficient for analysis 
## Analyze 
## Visualize findings


# Loading necessary packages
library(tidyverse)  
library(lubridate) 
library(ggplot2)  

# Reading data into R 
q2_2019 <- read_csv('Divvy_Trips_2019_Q2.csv')
q3_2019 <- read_csv('Divvy_Trips_2019_Q3.csv')
q4_2019 <- read_csv('Divvy_Trips_2019_Q4.csv')
q1_2020 <- read_csv('Divvy_Trips_2020_Q1.csv')

# See column names of the datasets
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

# Make the column names consistent with Q1_2020
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

# Check for inconsistencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

#changing h
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type)) 

## Combining 4 datasets into 1 

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# Remove columns that were not applicable anymore starting in 2020
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

##### Cleaning and adding data to prepare for analysis #####
colnames(all_trips) #checking the column names in the dataset 
dim(all_trips) #dataset has 3,879,822 observations and 9 variables associated with each variable
summary(all_trips) #checking a quick mathematical summary of each variable. 

unique(all_trips$member_casual) #checking to see unique values in column 'member_casual'
# We need to change 'subscriber' and 'customer' to either 'member' or 'casual' for this column

table(all_trips$member_casual) #check to see how many observations fall under each category
# Reassign
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
table(all_trips$member_casual) #check to see the results 

# Add columns that include day, month, year for each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Calculate ride_length
all_trips$ride_length = difftime(all_trips$ended_at, all_trips$started_at)

str(all_trips)
# We noticed that ride_length is in factor form
# Converting ride_length to numeric
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length) #ride_length has been converted to numeric

# Remove entries when bikes were taken out of docks (divvy or ride_length are negative)
# Creating a new dataset
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


#### CONDUCT DESCRIPTIVE ANALYSIS ####
# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)
# Average ride_length is 1479 seconds 
# The midpoint of the ride_length array is at 712 seconds
# Longest ride lasted for 9,387,024 seconds
# Shortest ride lasted for 1 second

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
# Casual members, on average, have longer bike rides at 3,553 seconds compared to that of members at 850 seconds
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
# The midpoint of casual members' ride length array is larger than that of members' (1546 > 589)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
# The longest ride for casual members lasted 9,387,024 seconds compared to that of members' at 9,056,634 seconds
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
# The shortest ride for casual members lasted 2 seconds compared to that of members' at 1 second

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
# Days of the week are out of order
# Putting days of week in order using ordered()
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                       "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
# The result of the aggregation suggests that on average casual members have longer bike rides every day of the week
#compared to members

## Analyze ridership data by type and weekday
type_weekday <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%# calculates the average duration
  arrange(member_casual, weekday)	
# We can conclude that casual members take longer bike rides but take fewer rides compared to members 
# Members, on the other hand, take shorter rides but overall take more rides in total

# VISUALIZE DATA 
ggplot(data=type_weekday)+
  geom_point(mapping=aes(x=weekday, y=number_of_rides, color=member_casual))+
  labs(title='Number of Rides for Each Customer Group', subtitle ='From Q2 of 2019 to Q1 of 2020',
       caption = 'by Chris Tran')

#visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title='Average Duration On Each Day', subtitle ='From Q2 of 2019 to Q1 of 2020', caption = 'by Chris Tran')

#creating a CSV file to export to use in Tableau or any other software
ride_length_each_day <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(ride_length_each_day, file = '~/Desktop/Google Cert./avg_ride_length.csv')


