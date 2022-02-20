library(tidyverse)
library(jsonlite)

# ensure that directory is correct
setwd('/home/ouecon003/DSCourseS22/ProblemSets/PS4')

# import JSON
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# get class
class(mydf) # should be a tibble
class(mydf$date)

# look at first 20 rows
head(mydf, 20)
