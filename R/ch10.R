library(tidyverse)

df <- tribble(
  ~xi, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)

# control number of rows and width of the output
print(tibble1, n = 1, width = 15)
print(tibble1, n = 1, width = Inf)

# to always print all columns and all rows
options(tibble.print_min = Inf) # this will be slow
options(tibble.width = Inf)


# Exercises ---------------------------------------------------------------


# Compare and contrast the following operations on a data.frame and equivalent
# tibble. What is different? Why might the default data frame behaviours cause
# you frustration?

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

df <- tibble(abc = 1, xyz = "a")
df$x # tibble does not do a partial matching
df[, "xyz"] # tibble always returns a dataframe regardless of how many rows
df[, c("abc", "xyz")]

# If you have the name of a variable stored in an object, e.g. var <- "mpg", how
# can you extract the reference variable from a tibble?

df <- as_tibble(mtcars)
var <- "mpg"
df[[var]]

df %>% 
  select(all_of(var))

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

annoying[["1"]]

ggplot(annoying) + 
  geom_point(aes(`1`, `2`))

annoying <- annoying %>% 
  mutate(
    `3` = `2` / `1`
  )

annoying %>% 
  rename(
    "one" = `1`,
    "two" = `2`,
    "three" = `3`,
  )

tibble::enframe(c(a = 1, b = 3))
