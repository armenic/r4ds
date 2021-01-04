library(dplyr)
library(nycflights13)
library(stringr)


# Had an arrival delay of two or more hours
flights %>%
  filter(arr_delay >= 120)


# Flew to Houston (IAH or HOU)
zz <- flights %>%
  filter(dest %in% c("IAH", "HOU"))
View(zz)

# Were operated by United, American, or Delta
View(airlines)
flights %>%
  filter(carrier %in% c("UA", "AA", "DL"))

# Departed in summer (July, August, and September)
flights %>%
  filter(month %in% c(7, 8, 9))


# Arrived more than two hours late, but didn’t leave late
zz <- flights %>%
  filter(arr_delay >= 120 & dep_delay <= 0)
View(zz)

# Were delayed by at least an hour, but made up over 30 minutes in flight
zz <- flights %>%
  filter(dep_delay >= 60 & arr_delay <= 30)
View(zz)

# Departed between midnight and 6am (inclusive)
zz <- flights %>%
  filter(dep_time >= 0 & dep_time <= 600)
View(zz)
zz <- flights %>%
  filter(between(dep_time, 0, 600))
View(zz)


# How many flights have a missing dep_time? What other variables are missing?
# What might these rows represent?
zz <- flights %>% 
  filter(is.na(dep_time))
View(zz)
# Cancellend flight?



# How could you use arrange() to sort all missing values to the start? (Hint:
# use is.na()).
df <- tibble(x = c(5, NA, 2, NA))
arrange(df, desc(is.na(x)))


# Sort flights to find the most delayed flights. Find the flights that left
# earliest.
flights %>% 
  arrange(desc(dep_delay))
flights %>% 
  arrange(dep_delay)


# Sort flights to find the fastest (highest speed) flights.
zz <- flights %>% 
  filter(!is.na(distance) & !is.na(air_time)) %>% 
  mutate(speed = distance / (air_time / 60)) %>% 
  arrange(desc(speed))
View(zz)

# Which flights travelled the farthest? Which travelled the shortest?
flights %>% 
  arrange(desc(distance))
flights %>% 
  arrange(distance)


flights %>% 
  select(any_of(c("year", "month", "day", "dep_delay", "arr_delay")))

select(flights, contains("TIME", ignore.case = FALSE))

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)


# Currently dep_time and sched_dep_time are convenient to look at, but hard to
# compute with because they’re not really continuous numbers. Convert them to a
# more convenient representation of number of minutes since midnight.

zz <- flights %>%
  select(dep_time, sched_dep_time) %>%
  mutate(
    dep_time_str = str_pad(dep_time, 4, "left", "0"),
    sched_dep_time_str = str_pad(sched_dep_time, 4, "left", "0"),
    dep_time_num = as.numeric(substr(dep_time_str, 1, 2)) * 60 +
      as.numeric(substr(dep_time_str, 3, 4)),
    sched_dep_time_num = as.numeric(substr(sched_dep_time_str, 1, 2)) * 60 +
      as.numeric(substr(sched_dep_time_str, 3, 4))
  ) %>% 
  select(!c(dep_time_str, sched_dep_time_str))
View(zz)


# Compare air_time with arr_time - dep_time. What do you expect to see? What do
# you see? What do you need to do to fix it?




















