#### Load packages, set parameters ####
library(tidyverse)

ID_title_length <- 5

removable <- c("a", "and", "for", "in", "of", "the", "to", "with")
removable_regex <- paste0("^", removable, "$", collapse = "|")



#### Read and set up data ####
books <- read_csv("book_list.csv")

set.seed(2022)
books_sample <- slice_sample(books, n = 20)

books_sample <- books_sample %>%
  mutate(no_punct_title = str_remove_all(title, "[:punct:]"),
         words = str_split(str_to_upper(no_punct_title), # caps for consistency
                           boundary("word")))



#### Test cases ####
single_word <- books_sample$words[8] %>% unlist() # Thud!
many_words <- books_sample$words[10] %>% unlist() # TDitW
extreme_words <- books_sample$words[3] %>% unlist() # Falchester


##### Single word solution #####
# if(length(single_word) == 1) {
  ID_result <- str_sub(single_word, 1, ID_title_length)
# }
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
    paste0(collapse = "")
}
ID_result3

ID_result1 == ID_result2
ID_result2 == ID_result3
ID_result1 == ID_result3


##### Many word solution, pt 2 #####
if(length(extreme_words) > ID_title_length) {
  # check for removable words
  # then it gets complicated
}
