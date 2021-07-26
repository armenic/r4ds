library(tidyverse)

table1

table1 %>% 
  mutate(rate = cases / population * 10000)

table1 %>% 
  count(year, wt = cases)

table1 %>% 
  group_by(year) %>% 
  summarise(sum(cases))

ggplot(table1, aes(as.factor(year), cases)) + 
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))


# exercises ---------------------------------------------------------------

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")


people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people %>% 
  group_by(name, names) %>% 
  mutate(n = row_number()) %>% 
  mutate(names = sprintf("%s_%s", names, n)) %>% 
  select(-n) %>% 
  pivot_wider(
    names_from = names,
    values_from = values
  )


preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg %>%
  pivot_longer(
    c(male, female), names_to = "gender", values_to = "count")
  )