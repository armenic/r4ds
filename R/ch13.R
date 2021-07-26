library(tidyverse)
library(nycflights13)

intersect(
  names(weather),
  names(flights)
)

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)


# exercises ---------------------------------------------------------------

flights <- flights %>% 
  mutate(surrogate_key = row_number()) %>% 
  View()

library(Lahman)

dups <- Batting %>% 
  count(yearID, lgID, teamID, playerID, stint) %>% 
  filter(n > 1)

dups_full <- Batting %>% 
  dplyr::inner_join(dups)

dups <- diamonds %>% 
  count(carat, cut, clarity, color, depth, x) %>% 
  filter(n > 1)
dups_full <- diamonds %>% 
  dplyr::inner_join(dups)
