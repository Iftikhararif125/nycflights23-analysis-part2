library(nycflights23)
library(tidyverse)

# Question 1 — Filtering flights
# Find all flights that:
#   - Departed from JFK.
# - Arrived at LAX.
# - Had a distance greater than 2,000 miles.
# Display only month, day, carrier, flight, origin, dest, and distance.
flights %>% 
  filter(origin == "JFK" ,
         dest == "LAX" ,
         distance > 2000) %>% 
  select(month, day, carrier, flight, origin , dest, distance)


# Question 2 — Sorting flights
# Find flights with an arrival delay greater than 60 minutes.
# 
# Display carrier, flight, origin, dest, and arr_delay.
# 
# Sort them from the longest arrival delay to the shortest.

flights %>% 
  filter(arr_delay > 60 ) %>% 
  select(carrier, flight, origin, dest, arr_delay) %>% 
  arrange(desc(arr_delay))

# Question 3 — Counting flights
# Count how many flights departed from each airport (JFK, LGA, and EWR).
# Display the airport name and total number of flights. Sort from highest to lowest.

flights %>%
  group_by(origin) %>% 
  summarise(
    total_flights = n()
  ) %>%
  arrange(desc(total_flights))

# Part 2: Intermediate exercises
# Question 4 — Creating a delay category
# Create a new column called Delay_Status using case_when():
#   - dep_delay <= 0: "On Time or Early"
# - dep_delay > 0 and dep_delay <= 30: "Short Delay"
# - dep_delay > 30 and dep_delay <= 120: "Medium Delay"
# - dep_delay > 120: "Long Delay"
# - Missing departure delay: "Unknown"
# Display carrier, dep_delay, and Delay_Status.
  flights %>%
    mutate(
      Delay_Status = case_when(
        dep_delay <= 0 ~ "On Time or Early",
        dep_delay <= 30 ~ "Short Delay",
        dep_delay <= 120 ~ "Medium Delay",
        dep_delay > 120 ~ "Long Delay",
        is.na(dep_delay) ~ "Unknown"
      )
    ) %>%
    select(carrier, dep_delay, Delay_Status)


# Question 5 — Monthly flight statistics
# For each month, calculate:
#   - Total number of flights.
# - Average departure delay.
# - Average arrival delay.
# - Average distance.
# Ignore missing values when calculating averages. 
  # Sort months by average departure delay, highest first.
flights %>% group_by(month) %>% summarise(
  total_flight =  n(),
  average_dep_delay = mean(dep_delay, na.rm = T),
  average_arr_delay = mean(arr_delay, na.rm = T),
  avg_dist = mean(distance, na.rm = T),
  .groups = "drop"
) %>% arrange(desc(average_dep_delay))

# Question 6 — Airline comparison
# For each carrier, calculate:
#   - Total flights.
# - Average departure delay.
# - Average arrival delay.
# - Maximum flight distance.
# Sort carriers by total flights, from highest to lowest.
flights %>% group_by(carrier) %>% summarise(
  total_flight = n(),
  average_dep_delay = mean(dep_delay, na.rm = T),
  average_arr_delay = mean(arr_delay, na.rm = T),
  max_dist = max(distance, na.rm = T),
  .groups = "drop"
) %>% arrange(desc(total_flight))

# Part 3: Advanced exercises
# Question 7 — Top 3 busiest routes
# Find the three busiest flight routes based on the combination of origin and dest.
# 
# Display:
#   - Origin airport.
# - Destination airport.
# - Total number of flights.
# Hint: Use group_by(), summarise(), arrange(), and slice().

flights %>% 
  group_by(origin,dest) %>% 
  summarise(
  total_flights = n(),
  .groups = "drop"
) %>% 
  slice_max(
    order_by = total_flights,
    n = 3,
    with_ties = F
  )

# Question 8 — Best performing airlines
# For each carrier, calculate the percentage of flights 
# that arrived on time or early (arr_delay <= 0).
# Requirements:
#   - Exclude flights with missing arr_delay.
# - Calculate the percentage correctly.
# - Display the carrier, total flights with known arrival delays, and on-time percentage.
# - Sort from highest to lowest percentage.

flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  summarise(
    total_flights = n(),
    on_time_percentage = mean(arr_delay <=0)*100,
    .groups = 'drop'
  ) %>% 
  arrange(desc(on_time_percentage))


# Question 9 — Longest flights by origin
# Find the five longest flights departing from each airport.
# Display origin, dest, carrier, flight, and distance.
# Use slice_max(). Return exactly five flights per origin, even when distances are tied.
flights %>% 
  group_by(origin) %>% 
  slice_max(
    order_by = distance, 
    n = 5,
    with_ties = F,
    na_rm = T)%>% 
  select(origin, dest , carrier, flight, distance) %>% ungroup()
 

# Question 10 — Advanced delay analysis
# Find the carriers that satisfy all these conditions:
#   1. They operated at least 1,000 flights.
# 2. Their average departure delay was greater than 10 minutes.
# 3. Their average arrival delay was greater than 5 minutes.
# For each qualifying carrier, display:
#   - Carrier.
# - Total flights.
# - Average departure delay.
# - Average arrival delay.
# - Maximum departure delay.
# Ignore missing values when calculating averages and maximum delay.

flights %>%
  group_by(carrier) %>% 
  summarise(
    total_flights = n(),
    avg_dd = mean(dep_delay, na.rm = T),
    avg_ad = mean(arr_delay, na.rm = T),
    max_dd = max(dep_delay, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter(total_flights > 1000,
         avg_dd > 10,
         avg_ad > 5
         )
