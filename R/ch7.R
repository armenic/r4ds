library(tidyverse)
# variation is a tendency of the values of a variable to change from measurement
# to measurement. The best way to understand the pattern of variation is to 
# visualize the distribution of the variable's values.


# distributions -----------------------------------------------------------


# categorical -------------------------------------------------------------

# bar plot with values
ggplot(diamonds, aes(x = cut)) + 
  geom_bar() + 
  # the ..count.. represents the count values from stat
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, color = "white")

diamonds %>% 
  count(cut)


# continuous --------------------------------------------------------------

ggplot(diamonds) +
  geom_histogram(aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

# for multiple continuous distros it is better to use freqpoly
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(smaller, aes(x = carat, color = cut)) + 
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# we can zoom in 

ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) + 
  coord_cartesian(ylim = c(0, 50))


unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>% 
  arrange(y)


# exercises ---------------------------------------------------------------


# Explore the distribution of each of the x, y, and z variables in diamonds.
# What do you learn? Think about a diamond and how you might decide which
# dimension is the length, width, and depth.

ggplot(diamonds) + 
  geom_histogram(aes(x = x), binwidth = 0.5)
# most of the length values are between 3 and 9

ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) + 
  coord_cartesian(xlim = c(0, 10))
# most of the width values are between 3 and 9, which probably means they are 
# round cuts 


ggplot(diamonds) + 
  geom_histogram(aes(x = z), binwidth = 0.5)



# Explore the distribution of price. Do you discover anything unusual or
# surprising? (Hint: Carefully think about the binwidth and make sure you try a
# wide range of values.)


ggplot(diamonds) + 
  geom_histogram(aes(x = price), binwidth = 25) + 
  coord_cartesian(xlim = c(0, 5000))
# most of the diamonds are withing a price range of 500  and 1500, there seems
# to be a gap of diamonds with $1500 price
# there seems to be a spike at $1800 and $4500

# How many diamonds are 0.99 carat? How many are 1 carat? What do you think is
# the cause of the difference?
ggplot(diamonds) + 
  geom_histogram(aes(x = carat))

diamonds %>% 
  filter(between(carat, 0.99, 1)) %>% 
  count(carat)
# there are only 23 diamonds with 0.99 carat and 1558 with carat 1, it seems
# they have rounded up














