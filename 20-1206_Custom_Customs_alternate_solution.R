library(tidyverse)
library(tidytext)

# From Emil Hvitfeldt
# https://twitter.com/Emil_Hvitfeldt/status/1335798494590685185/photo/1

df <- 
  readLines("20-1206_input_custom_customs.txt")

df <- 
  df %>% 
  str_c(collapse = "\n") %>% 
  as_tibble() %>% 
  unnest_paragraphs(output = text, input = value) %>% 
  rowid_to_column("group_id") %>% 
  unnest_tokens(output = text, input = text, token = "words") %>% #default
  rowid_to_column("person_id") %>% 
  unnest_characters(output = text, input = text)

# exercise 1

df %>% 
  count(group_id, text) %>% 
  nrow()


# exercise 2

df %>% 
  group_by(group_id) %>% 
  mutate(group_size = n_distinct(person_id)) %>% 
  count(group_size, text) %>% 
  filter(group_size != n) %>% 
  nrow()

