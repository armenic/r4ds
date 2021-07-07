library(tidyverse)

ggplot(mpg) + 
  geom_point(aes(displ, hwy))

str(mpg) # 234 rows and 11 columns

?mpg

ggplot(mpg) + 
  geom_point(aes(cyl, hwy))

ggplot(mpg) + 
  geom_point(aes(class, drv))

ggplot(mpg) + 
  geom_point(aes(displ, hwy, color = class))

ggplot(mpg) + 
  geom_point(aes(displ, hwy, size = class))

# static color should be outside of aes 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

sapply(mpg, is.character)
sapply(mpg, negate(is.character))


ggplot(mpg) + 
  geom_point(aes(displ, hwy)) + 
  facet_wrap(~ class, nrow = 2)
