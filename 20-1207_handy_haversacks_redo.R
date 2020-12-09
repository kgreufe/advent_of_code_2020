library(tidyverse)

# going though these steps again to cement understanding in accumulate and recursion formula
# original code from @drob on twitter
# https://twitter.com/drob/status/1336003816395845632

shinygold <- "shiny gold"


input <- tibble(x = readLines("20-1207_input_handy_haversacks.txt"))

bag_rules <- 
  input %>% 
  extract(col = x, 
          into = c("color", "contents"), 
          regex = "(.*) bags contain (.*)\\.") %>% 
  separate_rows(contents, 
                sep = ", ") %>% 
  extract(col = contents, 
          into = c("content_qty", "content_color"), 
          regex = "([0-9]*) (.*) bag", 
          remove = FALSE, convert = TRUE) %>% 
  filter(!is.na(content_qty))

## EXERCISE 1 ---------------------------------------------------
# how many bag colors can eventually contain at least 1 gold bag

contained_by <- function(colors, ...) {
  bag_rules %>% 
    filter(content_color %in% colors) %>% 
    pull(color)
}


accumulate(.x = 1:50, 
           .f = contained_by, 
           .init = shinygold) %>% 
  tail(-1) %>% 
  unlist() %>% 
  n_distinct()

## EXERCISE 2 --------------------------------------------------

n_contain <- function(colors) {
  
  r <- filter(bag_rules, color == colors)
  

  # recursively, each bag multiplies what it contains
  # each bag contains itself plus each it contains 
  sum(r$content_qty * (1 + map_dbl(.x = r$content_color, .f = n_contain)))
  
}

n_contain(shinygold)  

