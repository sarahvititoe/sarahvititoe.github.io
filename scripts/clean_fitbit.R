#### Library Steps ####
library(tidyverse)
library(jsonlite)
library(lubridate)

#### Get All 2019 Data ####
data <- str_c("./portfolio/fitbit/SarahVititoe/user-site-export/", 
      list.files(path = "./portfolio/fitbit/SarahVititoe/user-site-export/")) %>% 
  tibble(file_name = .) %>% 
  group_by(file_name) %>% 
  nest 

#### Functions ####

get_single_number <- function(df, my_filter){
  df <- df %>% 
    filter(str_detect(file_name, my_filter) == TRUE) %>% 
    mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
    mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
    mutate(file_name = str_replace(file_name, ".json", "")) %>% 
    separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
    select(file_type, data) %>% 
    unnest(data) %>% 
    janitor::clean_names() %>% 
    group_by(date_time) %>% 
    spread(key = file_type, value = value) %>% 
    ungroup() %>% 
    mutate(date_time = mdy_hms(date_time, tz = "America/New_York")) %>% 
    mutate(date_time_day = floor_date(date_time, "day"))
  
  df
}

get_resting_hr <- function(df){
  df <- df %>% 
    filter(str_detect(file_name, "resting_heart_rate") == TRUE) %>% 
    mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
    mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
    mutate(file_name = str_replace(file_name, ".json", "")) %>% 
    separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
    select(file_type, data) %>% 
    unnest(data) %>% 
    janitor::clean_names() %>% 
    group_by(date_time) %>% 
    select(-value_date, -value_error) %>% 
    spread(key = file_type, value = value_value) %>% 
    ungroup() %>% 
    mutate(date_time = mdy_hms(date_time, tz = "America/New_York")) %>% 
    mutate(resting_hr = ifelse(resting_heart_rate == 0, NA, round(resting_heart_rate, 1)))
  
  df
}

get_hr_zones <- function(df){
  df <- df %>% 
  filter(str_detect(file_name, "time_in_heart_rate") == TRUE) %>% 
  mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
  mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
  mutate(file_name = str_replace(file_name, ".json", "")) %>% 
  separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
  select(file_type, data) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  select(date_time, hr_zone3_min = value_values_in_zones_in_default_zone_3, hr_zone2_min = value_values_in_zones_in_default_zone_2, 
         hr_zone1_min = value_values_in_zones_in_default_zone_1) %>% 
  mutate(date_time = mdy_hms(date_time, tz = "America/New_York")) 
  
  df
}

get_hr <- function(df){
  df <- df %>% 
  filter(str_detect(file_name, "export/heart_rate") == TRUE) %>% 
  mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
  mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
  mutate(file_name = str_replace(file_name, ".json", "")) %>% 
  separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
  select(file_type, data) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  mutate(date_time = mdy_hms(date_time, tz = "America/New_York")) %>% 
  mutate(date_time = floor_date(date_time, "minutes")) %>% 
  group_by(date_time, file_type) %>% 
  summarise(value_bpm = round(mean(value_bpm, na.rm = TRUE), 1)) %>% 
  spread(file_type, value_bpm) %>% 
  ungroup() %>% 
  mutate(zone = ifelse(heart_rate < 120, "Zone 0", 
                       ifelse(heart_rate >= 120 & heart_rate < 129, "Zone 1", 
                              ifelse(heart_rate >= 129 & heart_rate < 153, "Zone 2", 
                                     ifelse(heart_rate >= 153 & heart_rate < 163, "Zone 3", 
                                            ifelse(heart_rate >= 163 & heart_rate < 169, "Zone 4", 
                                                   ifelse(heart_rate >= 169, "Zone 5", NA))))))) %>% 
  mutate(zone_intensity = ifelse(zone %in% c("Zone 1", "Zone 2"), "low intensity", 
                                 ifelse(zone == "Zone 3", "moderate intensity", 
                                        ifelse(zone %in% c("Zone 4", "Zone 5"), "high intensity", "out of zone")))) %>% 
  mutate(zone = as.factor(zone)) %>% 
  mutate(zone_intensity = as.factor(zone_intensity))
  
  df
}
  
get_vo2 <- function(df){
df <- df %>% 
  filter(str_detect(file_name, "run_vo2") == TRUE) %>% 
  mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
  mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
  mutate(file_name = str_replace(file_name, ".json", "")) %>% 
  separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
  select(file_type, data) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  select(date_time, run_vo2_max = value_filtered_run_vo2max) %>% 
  mutate(date_time = mdy_hms(date_time, tz = "America/New_York")) %>% 
  mutate(date_time = floor_date(date_time, "day"))
}

get_sleep <- function(df){
  df <- df %>% 
  filter(str_detect(file_name, "sleep")) %>% 
  mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
  mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
  mutate(file_name = str_replace(file_name, ".json", "")) %>% 
  separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
  select(file_type, data) %>% 
  unnest(data) %>% 
  separate(endTime, into = c("del1", "end_time"), sep = "T") %>% 
  separate(end_time, into = c("end_time", "del2"), sep = "\\.") %>% 
  janitor::clean_names() %>%  
  mutate(end_time = str_c(date_of_sleep, end_time, sep = " ")) %>% 
  mutate(end_time = ymd_hms(end_time, tz = "America/New_York")) %>% 
  mutate(date_time = floor_date(end_time, "day")) %>% 
  mutate(duration = duration(duration/1000, units = "seconds")) %>% 
  mutate(start_time = end_time - duration) %>% 
  mutate(minutes_to_fall_asleep = duration(minutes_to_fall_asleep, units = "minutes")) %>% 
  mutate(minutes_asleep = duration(minutes_asleep, units = "minutes")) %>% 
  mutate(minutes_awake = duration(minutes_awake, units = "minutes")) %>% 
  mutate(minutes_restless = duration(levels_summary_restless_minutes, units = "minutes")) %>% 
  mutate(minutes_rem = duration(levels_summary_rem_minutes, units = "minutes")) %>% 
  mutate(minutes_light_sleep = duration(levels_summary_light_minutes, units = "minutes")) %>% 
  mutate(minutes_deep_sleep = duration(levels_summary_deep_minutes, units = "minutes")) %>% 
  select(date_time, start_time, end_time, duration, efficiency, starts_with("minutes"), 
         sleep_wake_count = levels_summary_wake_count, sleep_rem_count = levels_summary_rem_count) 
  
  df
}

get_weight <- function(df){
  df <- df %>% 
    filter(str_detect(file_name, "weight")) %>% 
    mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
    mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
    mutate(file_name = str_replace(file_name, ".json", "")) %>% 
    separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
    select(file_type, data) %>% 
    unnest(data) %>% 
    janitor::clean_names() %>% 
    mutate(date_time = str_c(date, time, sep = " ")) %>% 
    mutate(date_time = mdy_hms(date_time, tz = "America/New_York")) %>% 
    select(date_time, weight, bmi, fat) %>% 
    mutate(date_time = floor_date(date_time, "day"))
  
  df
}

#### Create a Race Dataset ####
race <- read_csv("./portfolio/fitbit/official_race_times.csv") %>% 
  mutate(race_date = as_datetime(dmy(race_date))) %>% 
  rename(date_time = race_date) %>% 
  mutate(race_distance = ifelse(race_distance_unit == "kilometers", round(race_distance*0.621371, 1), race_distance)) %>% 
  select(-race_distance_unit)
# by day 

#### Create an Exercise Log 
race_day <- race %>% 
  select(date_time) %>% 
  mutate(is_race_day = "Yes")

race %>% 
  write_csv("./portfolio/fitbit/clean/race.csv")


notes <- read_csv("./portfolio/fitbit/run_notes.csv") %>% 
  mutate(workout_date = as_datetime(mdy(workout_date))) %>% 
  select(date_time = workout_date, notes_workout_type = workout_type, 
         notes_training_for = training_for, notes_workout_goals = workout_notes_goals)


notes %>% write_csv("./portfolio/fitbit/clean/notes.csv")
  

# by day 

str_c("./portfolio/fitbit/SarahVititoe/user-site-export/", 
      list.files(path = "./portfolio/fitbit/SarahVititoe/user-site-export/", pattern = "exercise")) %>% 
  tibble(file_name = .) %>% 
  group_by(file_name) %>% 
  nest %>% 
  mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
    mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
    mutate(file_name = str_replace(file_name, ".json", "")) %>% 
    separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
    select(file_type, data) %>% 
    unnest(data) %>% 
    janitor::clean_names() %>% 
    mutate(start_time = mdy_hms(start_time, tz = "America/New_York")) %>% 
    mutate(date_time = floor_date(start_time, "day")) %>% 
    select(date_time, start_time, activity_name, calories, duration, active_duration, steps, distance, 
           distance_unit, speed, pace, elevation_gain, average_heart_rate) %>% 
    mutate(duration = seconds_to_period(duration/1000)) %>% 
    mutate(active_duration = seconds_to_period(active_duration/1000)) %>% 
    mutate(pace = seconds_to_period(pace)) %>% 
  mutate_at(vars(pace, duration, active_duration), funs(round(., 1))) %>% 
  mutate_at(vars(speed, distance, elevation_gain), funs(round(., 2))) %>% 
  mutate(end_time = start_time + duration) %>% 
  select(date_time, start_time, end_time, everything(), -duration) %>% 
  write_csv("./portfolio/fitbit/clean/exercise.csv")

rm(race_day)
rm(notes)

#### Create badges dataset ####

str_c("./portfolio/fitbit/SarahVititoe/user-site-export/", 
        list.files(path = "./portfolio/fitbit/SarahVititoe/user-site-export/", pattern = "badge")) %>% 
    tibble(file_name = .) %>% 
    group_by(file_name) %>% 
    nest  %>% 
    mutate(data = map(.$file_name, ~fromJSON(., flatten = TRUE) %>% as_tibble())) %>% 
    mutate(file_name = str_replace(file_name, "./portfolio/fitbit/SarahVititoe/user-site-export/", "")) %>% 
    mutate(file_name = str_replace(file_name, ".json", "")) %>% 
    separate(file_name, into = c("file_type", "file_year", "file_month", "file_day"), sep = "-") %>% 
    select(file_type, data) %>% 
    unnest(data) %>% 
    janitor::clean_names() %>% 
    mutate(date_time = ymd(date_time)) %>% 
    select(date_time, everything())  %>% 
    write_csv("./portfolio/fitbit/clean/badges.csv")

#### Combine all minute-level data (altitude, calories, distance, and steps)) ####

minute <- get_single_number(data, "altitude") %>% 
  full_join(get_single_number(data, "calories")) %>% 
  full_join(get_single_number(data, "distance")) %>% 
  full_join(get_single_number(data, "steps"))

#### Create another version that's aggregated at the day-level ####
minute %>% 
  select(-date_time) %>% 
  group_by(date_time_day) %>% 
  mutate_all(funs(as.numeric)) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename(date_time = date_time_day) %>% 
  filter(!is.na(date_time)) %>% 
  mutate(distance = ifelse(distance == 0, NA, distance)) %>% 
  mutate(steps = ifelse(steps == 0, NA, steps)) %>% 
  mutate(altitude = ifelse(altitude == 0, NA, altitude)) %>% 
  write_csv("./portfolio/fitbit/clean/minute_data_aggregate_daily.csv")

minute %>% 
  select(-date_time_day) %>% 
  write_csv("./portfolio/fitbit/clean/minute_data.csv")

rm(minute)

#### Daily Data - active minutes, resting heart rate, vo2, and weight ####

get_single_number(data, "lightly_active") %>% 
  full_join(get_single_number(data, "moderately_active")) %>% 
  full_join(get_single_number(data, "very_active")) %>% 
  full_join(get_single_number(data, "sedentary")) %>% 
  full_join(get_resting_hr(data)) %>% 
  full_join(get_vo2(data)) %>% 
  full_join(get_weight(data)) %>% 
  select(-date_time_day, -resting_heart_rate) %>% 
  write_csv("./portfolio/fitbit/clean/daily.csv")

# hr_zones <- get_hr_zones(data)
  # aggregated by day - going to comment theses out because I want to use my own heart rate zones 
  
get_hr(data) %>% 
  write_csv("./portfolio/fitbit/clean/hr_minute.csv")
  #aggregated by minute

get_sleep(data)  %>% 
  write_csv("./portfolio/fitbit/clean/sleep.csv")
  # by day