# From David Robinson @drob on twitter
# https://twitter.com/drob/status/1337098900553994246/photo/1

library(tidyverse)

input <- as.double(read_lines("20-1210_input_adapter_array.txt"))

jolts <- sort(c(0, input, max(input) + 3))

# EXERCISE 1 ---------------------------
# product of 1-jolt differences multiplied by 3-jolt differences

x <- diff(jolts) %>% table() 

# we can visually see that we only have 1 and 3 jolt jumps, but let's make the code "safe" in case that were not the case 

x["1"] * x["3"]
  
# answer is 1917



# EXERCISE 2 ---------------------------
# David Robinson uses the "memoise" package which lets a function remember its result when you call it repeatedly

# from documentation:
#  caches function calls so that if a previously seen set of inputs is seen, it can return the previously computed output.

# I'm more curious about the construction of the recursive function, so ignoring "memoise" for now

num_combinations <- function(x) {
  
  if (length(x) == 1) {
    return(1)
  }  
  
  jumps <- which(x[2:4] - x[1] <= 3)
  
  sum(map_dbl(jumps, ~ num_combinations(tail(x, -.))))
  
}

num_combinations(1:10)


