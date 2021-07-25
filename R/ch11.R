library(tidyverse)

read_csv("a,b,c
1,2,3
4,5,6
")


# exercises ---------------------------------------------------------------


# What function would you use to read a file where fields were separated with
# “|”?
read_delim("a|b|c\n1|2|3", "|")

read_csv("a,b\n1,2,3\n4,5,6") # has less column name
read_csv("a,b\n1,2,3\n4,5,6", col_names = c("a", "b", "c"), skip = 1)

read_csv("a,b,c\n1,2\n1,2,3,4") # has less column names
read_csv("a,b,c\n1,2\n1,2,3,4", col_names = letters[1:4], skip = 1)


read_csv("a,b\n\"1") # has unmatched "
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3") # usese semicolon as a delim
read_csv2("a;b\n1;3")



# Generate the correct format string to parse each of the following dates and
# times:
  
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1, format = "%B %d, %Y")
parse_date(d2, format = "%Y-%b-%d")
parse_date(d3, format = "%d-%b-%Y")
parse_date(d4, format = "%B %d (%Y)")
parse_date(d5, format = "%m/%d/%y")
parse_time(t1, format = "%H%M")
parse_time(t2, format = "%H:%M:%S %p")
