#### Load packages, set parameters ####
library(tidyverse)

ID_title_length <- 5

removable <- c("a", "and", "for", "in", "of", "the", "to", "with") %>% 
  str_to_upper() # titles will be caps for consistency
removable_regex <- paste0("^", removable, "$", collapse = "|")



#### Read and set up data ####
books <- read_csv("book_list.csv")

set.seed(2022)
books_sample <- slice_sample(books, n = 20)

books_sample <- books_sample %>%
  mutate(no_punct_title = str_remove_all(title, "[:punct:]"), # don't want punctuation in ID
         words = str_split(str_to_upper(no_punct_title), # caps for consistency
                           boundary("word")))



#### Test cases - titles ####
single_word <- books_sample$words[8] %>% unlist() # Thud!
many_words <- books_sample$words[10] %>% unlist() # TDitW
extreme_words <- books_sample$words[3] %>% unlist() # Falchester


##### Single word solution #####
if(length(single_word) == 1) {
  ID_result <- str_sub(single_word, 1, ID_title_length)
}
ID_result


##### Many word solution, pt 1 #####
# Part 1 because it's the easiest version (exact number of words as ID-characters)
# Grab the first letter of each word

## For loop version
if(length(many_words) == ID_title_length) {
  ID_result1 <- character(ID_title_length)
  for(i in seq_along(many_words)) {
    ID_result1[i] <- str_sub(many_words[i], 1, 1)
  }
}
rm(i)
ID_result1 <- paste0(ID_result1, collapse = "")
ID_result1


## purrr version
if(length(many_words) == ID_title_length) {
  ID_result2 <- map_chr(many_words,
                        function(x) {
                          len <- seq_along(x)
                          map_chr(len, ~str_sub(x[.], 1, 1))
                        }) %>% 
    reduce(paste0, collapse = "")
}
ID_result2


## Non-loop version
# Adapted from: stackoverflow.com/questions/58659318/
if(length(many_words) == ID_title_length) {
  ID_result3 <- str_extract(many_words, "^[:alpha:]{1}") %>%
    unlist() %>%
    str_flatten()
}
ID_result3

ID_result1 == ID_result2
ID_result2 == ID_result3
ID_result1 == ID_result3


##### Many word solution, pt 2 #####
# Part 2 gets into more complicated cases (title has more words than ID-characters)

if(length(extreme_words) > ID_title_length) {
  # Check for removable words
  rm_words <- str_detect(extreme_words, removable_regex)
  use_words <- extreme_words[!rm_words]
  
  # if length(use_words) == ID_title_length
  # then: use pt1 solution
  # make it into a function? `extract_(first/n)_letters`? (specific "first" vs general "n")
  if(length(use_words) == ID_title_length) {
    
    ID_result4 <- str_extract(use_words, "^[:alpha:]{1}") %>%
      unlist() %>%
      str_flatten()
  }
  
  # if: length(use_words) > ID_title_length
  # then: use initial letter of the first five words? - def make&integrate a function as next step
  else if(length(use_words) > ID_title_length) {
    ID_result4 <- str_extract(use_words, "^[:alpha:]{1}") %>%
      unlist() %>%
      str_flatten() %>% 
      str_sub(start = 1, end = ID_title_length)
  }
  
  # if: length(use_words) < ID_title_length
  # then: most complicated, need to work out a system
  else if(length(use_words) < ID_title_length) {
    # TBD
  }
}
ID_result4


#### Test cases - authors ####
# Should be fairly straightforward, but what about multi-author works? Just use first author?
