#### Setup ####
library(tidyverse)

ID_title_length <- 5

removable <- c("a", "and", "for", "in", "of", "the", "to", "with") %>% 
  str_to_upper() # titles will be caps for consistency
removable_regex <- paste0("^", removable, "$", collapse = "|")

extract_n_letters <- function(words_string, n = 1, len = ID_title_length) {
  # Adapted from: stackoverflow.com/questions/58659318/
  ### Extract the first `n` letters of each element of character vector `words_string`,
  ### optionally truncating the resulting string to length `len`.
  ## Arguments:
  ## - words_string = character vector, each element is a word to grab letters from
  ##                  (here, generated via str_split(*, boundary("word")))
  ## - n = numeric; number of letters to extract (starts from beginning)
  ## - len = numeric; length of output (truncates result)
  ##         (if -1, no truncation; see `str_sub` documentation)
  
  str_extract(words_string, paste0("^[:alpha:]{", n, "}")) %>%
    unlist() %>%
    str_flatten() %>% 
    str_sub(start = 1, end = len)
}



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
if(length(many_words) == ID_title_length) {
  ID_result3 <- extract_n_letters(many_words)
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
  if(length(use_words) == ID_title_length) {
    ID_result4 <- extract_n_letters(use_words)
  }
  
  # if: length(use_words) > ID_title_length
  # then: use initial letter of the first five words?
  else if(length(use_words) > ID_title_length) {
    ID_result4 <- extract_n_letters(use_words)
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
