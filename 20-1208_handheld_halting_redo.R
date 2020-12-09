library(tidyverse)

input <- readLines("20-1208_input_handheld_halting.txt")

# accumulator starts at zero
# start at first element, then follow instructions
# "acc" increases or decreases accumulator value
# "jmp" jumps in the instructions forwards/backwards by value indicated
# "nop" is no operation, continue to next element

# EXERCISE 1 -------
# instructions produce infinite loop
# what is the accumulator value before executing single instruction a second time?

run_program <- function(input, part2 = FALSE) {
  
  accumulator <- 0
  
  n_times <- vector("integer", length(input))
  
  i <- 1 # index
  
  repeat {
    
    # what we're working with each loop
    x <- str_split(input[i], " ")[[1]]
    
    # increase execution times for that element by 1
    n_times[i] <- n_times[i] + 1
    
    if (i > length(input)) return(accumulator)
    
    if (n_times[i] == 2) {
      
      if (part2 == TRUE) {
        return(NA)
      } else {
        return(accumulator)
      }
    }
    
    if (x[1] == "nop") {
      i <- i + 1
    } else if (x[1] == "acc") {
      accumulator <- accumulator + parse_number(x[2])
      i <- i + 1
    } else if (x[1] == "jmp") {
      i <- i + parse_number(x[2])
    }
    
  }
}

run_program(input = input)
# 1137

# EXERCISE 2 ----------------------------------------
# to avoid the infinite loop, we can swap one "jmp" to a "nop" or vice-versa (only 1!)
# after making the change to avoid the infinite loop, 
#  what is the value of the accumulator for that complete successful execution?

for (j in seq_along(input)) {
  
  input_loop <- input
  
  # can't change accumulations, so jump to next loop if we encounter this
  if (str_detect(input_loop[j], "acc")) next 
  
  if (str_detect(input_loop[j], "jmp")) {
    input_loop[j] <- str_replace(input_loop[j], "jmp", "nop")
  } else {
    input_loop[j] <- str_replace(input_loop[j], "nop", "jmp")
  }
  
  res <- run_program(input = input_loop, part2 = TRUE)
  
  # once we find a successful completion, stop the loop and return the result to console
  if (!is.na(res)) {
    print(res)
    break
  }
  
}

# 1125