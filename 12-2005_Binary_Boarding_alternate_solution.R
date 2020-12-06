library(tidyverse)

# from Emil Hvitfeldt 
# (https://twitter.com/Emil_Hvitfeldt/status/1335356890314731520/photo/1)

id <- 
  read_lines("20-1205_input_binary_boarding.txt") %>% 
  str_replace_all(c("[FL]" = "0", "[BR]" = "1")) %>% 
  strtoi(base = 2)

# exercise 1

max(id)
  
# exercise 2

# setdiff is asymetric difference
# here between all the values between the min and max of the set and the actual values we have
# the difference should by my seat for the exercise

setdiff(seq(min(id), max(id)), id)
