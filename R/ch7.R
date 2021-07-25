library(tidyverse)


# variation ---------------------------------------------------------------

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



# covariation -------------------------------------------------------------


# continuous --------------------------------------------------------------



# If a variation describes the behavior within a variable, covariation describes
# the behavior between variables. Covariation is the tendency for the values of 
# two or more variables to vary together in a related way.


ggplot(diamonds, aes(x = price)) + 
  geom_freqpoly(aes(color = cut), binwidth = 500)

# to make polygons comparable we can use the density as the y axis, which is the
# standardized area under curve (equal to 1)

ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(color = cut), binwidth = 500)

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot(mpg) + 
  geom_boxplot(aes(x = reorder(class, hwy, median), y = hwy))

# exercises ---------------------------------------------------------------

# Use what you’ve learned to improve the visualisation of the departure times of
# cancelled vs. non-cancelled flights.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time, ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)


# What variable in the diamonds dataset is most important for predicting the
# price of a diamond? How is that variable correlated with cut? Why does the
# combination of those two relationships lead to lower quality diamonds being
# more expensive?

ggplot(diamonds, aes(x = carat, y = ..density..)) + 
  geom_freqpoly(aes(color = cut), binwidth = 5)

ggplot(diamonds) + 
  geom_boxplot(aes(x = cut, y = carat))
# it seems lower quality diamonds are bigger, hence are more expensive on
# average




# categorical -------------------------------------------------------------


# To visualise the covariation between categorical variables, you'll need to 
# count the number of observations for each combination. 
# Covariation will appear as a strong correlation between specific x values and
# specific y values.
ggplot(diamonds) + 
  geom_count(aes(x = cut, y = color))

counts <- diamonds %>% 
  count(color, cut) %>% 
  print()

counts %>% 
  ggplot(aes(x = color, y = cut)) + 
  geom_tile(aes(fill = n))


# exercises ---------------------------------------------------------------

# How could you rescale the count dataset above to more clearly show the
# distribution of cut within colour, or colour within cut?
counts <- diamonds %>% 
  count(color, cut) %>% 
  mutate(n = n * 0.001) %>% 
  print()
counts %>% 
  ggplot(aes(x = color, y = cut)) + 
  geom_tile(aes(fill = n))


# continuous --------------------------------------------------------------
ggplot(diamonds) + 
  geom_point(aes(x = carat, y = price), alpha = 1 / 10)


ggplot(diamonds) + 
  geom_bin2d(aes(x = carat, y = price))

smaller <- diamonds %>% 
  filter(carat < 3)


ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

# exercises ---------------------------------------------------------------


# Instead of summarising the conditional distribution with a boxplot, you could
# use a frequency polygon. What do you need to consider when using cut_width()
# vs cut_number()? How does that impact a visualisation of the 2d distribution
# of carat and price?

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(smaller, aes(x = price, color = cut_number(carat, 5))) + 
  geom_freqpoly(binwidth = 5)

# Visualise the distribution of carat, partitioned by price.
ggplot(smaller, aes(x = price, y = carat)) + 
  geom_boxplot(aes(group = cut_width(price, 1000)))


# How does the price distribution of very large diamonds compare to small
# diamonds? Is it as you expect, or does it surprise you?
# it seems that large diamonds have wider distributions compared with small 
# diamonds.


# Patterns provide one of the most useful tools for data scientists because they
# reveal covariation. If you think of variation as a phenomenon that creates
# uncertainty, covariation is a phenomenon that reduces it. If two variables
# covary, you can use the values of one variable to make better predictions
# about the values of the second. If the covariation is due to a causal
# relationship, then you can use the value of one variable to control the value
# of the second.
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

# Models are a tool for extracting patterns out of data. For example, consider
# the diamonds data. It's hard to understand the relationship between cut and
# price, because cut and carat, and carat and price are tightly related. It's
# possible to use a model to remove the very strong relationship between price
# and carat so we can explore the subtleties that remain. The following code
# fits a model that predicts price from carat and then computes the residuals
# (the difference between the predicted value and the actual value). The
# residuals give us a view of the price of the diamond, once the effect of carat
# has been removed.

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(diamonds2) + 
  geom_point(aes(x = carat, y = resid))

# Once you've removed the strong relationship between carat and price, you can 
# see what you expect in the relationship between cut and price: relative to 
# their size, better quality diamonds are more expensive

ggplot(diamonds2) +
  geom_boxplot(aes(x = cut, y = resid))
