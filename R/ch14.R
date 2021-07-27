library(tidyverse)

x <- c("\"", "\\")
writeLines(x)

x <- "\u00b5"

x <- c("apple", "banana", "pear")
str_view(x, "an")
