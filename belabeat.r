sleep_day <- read.csv("sleepDay_merged.csv")
view(sleep_day)
n_distinct(sleep_day$Id)
nrow(sleep_day)
weight_info <- read.csv("weightLogInfo_merged.csv")
view(weight_info)
n_distinct(weight_info)
n_distinct(weight_info$Date)
hourly_steps <- read.csv("hourlySteps_merged.csv")
view(hourly_steps)
weight_info %>%
  group_by(Id)%>%
  summarise(min(WeightKg),max(WeightKg))
hourly_calories <- read.csv("hourlyCalories_merged.csv")
view(hourly_calories)
install.packages

library(janitor)

clean_daily_activity <- clean_names(daily_activity) %>% # daily_activity df clean
  rename(date = activity_date) %>%
  remove_empty(which = c("rows")) %>%
  remove_empty(which = c("cols"))

view(daily_activity)
view(clean_daily_activity)

clean_daily_activity$date <- as.Date(clean_daily_activity$date, format = "%B/%m/%Y"

view(clean_daily_activity$date)

clean_sleep_day <- clean_names(sleep_day) %>% # sleep_day df clean
  rename(date = sleep_day) %>%
  remove_empty(which = c("rows")) %>%
  remove_empty(which = c("cols"))

view(clean_sleep_day$date)

clean_sleep_day$date <- as.Date(clean_sleep_day$date, format ="%B/%m/%Y")

clean_hourly_steps <- clean_names(hourly_steps) %>% # hourly_steps df clean
  remove_empty(which = c("rows")) %>%
  remove_empty(which = c("cols"))

clean_hourly_calories <- clean_names(hourly_calories) %>% # hourly_calories df clean
  remove_empty(which = c("rows")) %>%
  remove_empty(which = c("cols"))

clean_daily_activity %>%
  select(total_steps,total_distance,sedentary_minutes) %>%
  summary()

clean_sleep_day %>%
  select(total_sleep_records,total_minutes_asleep,total_time_in_bed) %>%
  summary()

clean_hourly_steps %>%
  select(step_total) %>%
  summary()

clean_hourly_calories %>%
  select(calories) %>%
  summary()

ggplot(data = clean_daily_activity, mapping = aes(x= sedentary_minutes, y = total_steps)) +
  geom_point() + labs(title = "Total sedentary time vs Step count daily")

ggplot(data =clean_sleep_day, mapping = aes(x = total_minutes_asleep, y = total_time_in_bed)) +
  geom_point() + labs(title = "Total minutes asleep vs Total time in bed")

combine_step_calories <- merge(clean_hourly_calories, clean_hourly_steps, by = c("id","activity_hour"))
glimpse(combine_step_calories)

combine_step_calories %>%
  group_by(id) %>%
  summarise(count_records=n()) %>%
  arrange(count_records)

combine_sleep_daily_activity <- merge(clean_daily_activity, clean_sleep_day, by = c("id", "date"))
view(combine_sleep_daily_activity)
combine_sleep_daily_activity <- mutate(sleep_combine_daily_activity,
                                        awake_time_in_bed = total_time_in_bed - total_minutes_asleep)
n_distinct(combine_sleep_daily_activity$id)

ggplot(data=combine_step_calories,mapping = aes(x= calories, y = step_total)) +
  geom_line() + labs(title= "Hourly step vs Hourly Calories burn for each user") +
  facet_wrap(~id)

cor(combine_step_calories$step_total,combine_step_calories$calories)
# correlation between step total and calories

ggplot(data=combine_sleep_daily_activity,mapping=aes(x=sedentary_minutes, y=total_minutes_asleep)) +
  geom_point() + labs(title= "Sleep Duration and Sedentary Time")
