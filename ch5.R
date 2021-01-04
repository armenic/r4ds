library(dplyr)
library(nycflights13)


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
























