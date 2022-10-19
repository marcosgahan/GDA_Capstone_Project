
 # First prepare the enviroment and load the packages

setwd("C:/Users/Marcos/Desktop/trip-data")

  library(tidyverse)
  library(lubridate)
  library(janitor)


# Import datasets, merge them and take a 500,000 row sample

  a <- read_csv("202109-divvy-tripdata.csv")
  b <- read_csv("202110-divvy-tripdata.csv")
  c <- read_csv("202111-divvy-tripdata.csv")
  d <- read_csv("202112-divvy-tripdata.csv")
  e <- read_csv("202201-divvy-tripdata.csv")
  f <- read_csv("202201-divvy-tripdata.csv")
  g <- read_csv("202201-divvy-tripdata.csv")
  h <- read_csv("202201-divvy-tripdata.csv")
  i <- read_csv("202201-divvy-tripdata.csv")
  j <- read_csv("202201-divvy-tripdata.csv")
  k <- read_csv("202201-divvy-tripdata.csv")
  l <- read_csv("202201-divvy-tripdata.csv")
  
  trips_data <- rbind(a,b,c,d,e,f,g,h,i,j,k,l)

  sample_trips_data <- sample_n(trips_data,500000)

  rm(trips_data,a,b,c,d,e,f,g,h,i,j,k,l)
# Now you have data ready to be cleaned
  
  rename_with(sample_trips_data,tolower)
  clean_names(sample_trips_data)
  

# There are 13 columns in total: "ride_id","rideable_type","start_day","start_hour","ended_day","ended_hour","start_station_name","start_station_id","end_station_name","end_station_id","start_lat","start_lng","end_lat","end_lng","member_casual"    

# We check for missing values
sapply(sample_trips_data, function(x) sum(is.na(x)))

#Now we see if all the columns data types are correct

glimpse(sample_trips_data)

# All good so now it is time to analyze.

write_csv(sample_trips_data,"sample_trips_data.csv")

# Analyze phase

#Let's create a new row with the trip length in minutes

sample_trips_data$trip_duration_in_mins <- difftime(sample_trips_data$ended_at,sample_trips_data$started_at, units = "mins")
sample_trips_data$trip_duration_in_mins <- round(sample_trips_data$trip_duration_in_mins, digits = 0)

head(sort(sample_trips_data$trip_duration_in_mins, decreasing = TRUE))

# Some of the trips are very long and make the data biases. Let's leave them out and focus on trips smaller than 120 minutes

duration_trips_data <- sample_trips_data %>% 
  filter(trip_duration_in_mins < 120)

a <- duration_trips_data %>% 
  select(member_casual,trip_duration_in_mins) %>%
  group_by(member_casual) %>% 
  summarise(avg_duration = round(mean(trip_duration_in_mins), digits = 0))

b <- duration_trips_data %>%
  select(member_casual,trip_duration_in_mins) %>%
  group_by(member_casual) %>%
  count(member_casual) %>%
  rename(amount = n)

trip_duration_per_user <-  inner_join(b,a, by = "member_casual")

rm(a,b)

head(trip_duration_per_user)

# It seems there's something interesting there, members tend to use a bike 8 minutes less per trip in average than a casual user.

# We group by stations and remove stations with fewer than 50 trips to not make the data biased.

a <- duration_trips_data %>% 
  select(start_station_name,trip_duration_in_mins) %>% 
  group_by(start_station_name) %>% 
  summarise(avg_duration = mean(trip_duration_in_mins)) %>%
  arrange(avg_duration*-1)

b <- duration_trips_data %>%
  select(start_station_name,trip_duration_in_mins,member_casual) %>%
  group_by(start_station_name) %>%
  count(start_station_name) %>%
  rename(amount = n)

c <- duration_trips_data %>% 
  select(start_station_name,member_casual) %>%
  filter(member_casual == "member") %>%
  group_by(start_station_name) %>%
  count(member_casual)

duration_vs_station <- inner_join(b,inner_join(c,a,by = "start_station_name"), by = "start_station_name")
duration_vs_station$percentage_of_members <- (duration_vs_station$n*100)/duration_vs_station$amount

duration_vs_station <- duration_vs_station %>%
  arrange(avg_duration*-1) %>%
  filter(n > 50)

rm(a,b,c,duration_trips_data)

# We now analyze which type of bike each type of user uses the most.

casual_vs_members_rideabletype <- sample_trips_data %>%
  select(rideable_type,member_casual) %>%
  group_by(rideable_type) %>%
  count(member_casual) %>%
  mutate(percentage = (n * 100)/ 500000) %>%
  rename(amount = n)

amount_of_members = 331422
amount_of_casuals = 164849

rideabletype_members <- casual_vs_members_rideabletype %>%
  filter(member_casual == "member") %>%
  mutate(percentage = (amount * 100)/ amount_of_members) %>%
  arrange(percentage*-1)

rideabletype_casuals <- casual_vs_members_rideabletype %>%
  filter(member_casual == "casual") %>%
  mutate(percentage = (amount * 100)/ amount_of_casuals) %>%
  arrange(percentage*-1)

rm(casual_vs_members_rideabletype)

# The next step is analyzing the variation in the weekly basis use of each type of user.

sample_trips_data$day_of_the_week <- wday(sample_trips_data$started_at, week_start=1)

weekly_members_use <-  sample_trips_data %>%
  select(member_casual,day_of_the_week) %>%
  filter(member_casual == "member") %>%
  group_by(day_of_the_week) %>%
  count(member_casual) %>%
  rename(amount = n) %>%
  mutate(percentage = (amount*100)/amount_of_members) %>% 
  arrange(day_of_the_week)
weekly_members_use$day <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

weekly_casuals_use <-  sample_trips_data %>%
  select(member_casual,day_of_the_week) %>%
  filter(member_casual == "casual") %>%
  group_by(day_of_the_week) %>%
  count(member_casual) %>%
  rename(amount = n) %>%
  mutate(percentage = (amount*100)/amount_of_casuals)
weekly_casuals_use$day <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")


# We proceed with analyzing at what hours members and casuals use bikes.

sample_trips_data$hour <- format(as.POSIXct(sample_trips_data$started_at), format = "%H")

daily_members_use <-  sample_trips_data %>%
  select(member_casual,hour) %>%
  filter(member_casual == "member") %>%
  group_by(hour) %>%
  count(member_casual) %>%
  rename(amount = n) %>%
  mutate(percentage = (amount*100)/amount_of_members)

daily_casuals_use <-  sample_trips_data %>%
  select(member_casual,hour) %>%
  filter(member_casual == "casual") %>%
  group_by(hour) %>%
  count(member_casual) %>%
  rename(amount = n) %>%
  mutate(percentage = (amount*100)/amount_of_casuals)

# Finally we'll analyze the coordinates in Tableau.


# Now we jump into the Visualization phase
# I'll only do one chart in R and the others in Tableau

# We create a scatter plot where we see the relation between trip duration and percentage of members.

ggplot(duration_vs_station, aes(x=avg_duration, y=percentage_of_members)) +
  geom_point(color="#7895B2") +
  geom_smooth(fill = "#7895B2") +
  xlab("Average Trip Duration in minutes") +
  ylab("Percentage of members %") +
  labs(title = "Trip duration chart", subtitle = "Percentage of members vs average trip duration in each station") +
  theme(axis.title = element_text(color = "#7895B2",size = 14), 
        plot.title = element_text(size = 28, color = "#7895B2"),
        plot.subtitle = element_text(size = 14, color = "#AEBDCA"),
        axis.text = element_text(color = "#7895B2",size = 12), 
        panel.background = element_rect(fill = "#F5EFE6"),
        panel.grid = element_line(color = 'grey', linetype = 'dotted')
  ) +
  xlim(7, 30)

# Then we check the correlation between the two vectors

print(cor(as.numeric(duration_vs_station$avg_duration),duration_vs_station$percentage_of_members))
