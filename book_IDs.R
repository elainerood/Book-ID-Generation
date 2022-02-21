#### Setup ####
library(tidyverse)

ID_title_length <- 5

removable <- c("a", "of", "to", "in", "and", "for", "with", "the") %>% 
  str_to_upper() # titles will be caps for consistency
# ^ ordered by below preference for being retained
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
filler_words <- books_sample$words[12] %>% unlist() # ASatCoG


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

# ID_result1 == ID_result2
# ID_result2 == ID_result3
# ID_result1 == ID_result3


##### Many word solution, pt 2 #####
# Part 2 gets into more complicated cases (title has more words than ID-characters)
if(length(extreme_words) > ID_title_length) {
  # Check for removable words
  # rm_words <- str_detect(extreme_words, removable_regex)
  # use_words <- extreme_words[!rm_words]
  rm_words <- str_detect(filler_words, removable_regex)
  use_words <- filler_words[!rm_words]
  
  # if length(use_words) == ID_title_length
  # then: use pt1 solution
  if(length(use_words) == ID_title_length) {
    ID_result4 <- extract_n_letters(use_words)
  }
  
  # if: length(use_words) > ID_title_length
  # then: use initial letter of the first five words?
  # **TO DO: in final version (i.e., function form), run`extract_n_letters` at the end,
  #          after obtaining a "correct" input for it
  #   (requires putting `if(length(use_words) < ID_title_length)` first vs last)
  else if(length(use_words) > ID_title_length) {
    ID_result4 <- extract_n_letters(use_words)
  }
  
  # if: length(use_words) < ID_title_length
  # then: most complicated, need to work out a system
  # - could just keep it as less than the max ID length (that's already a case for
  #   single-word titles), but it just doesn't look as good
  else if(length(use_words) < ID_title_length) {
    # bring back removable word(s), with preference:
    #   a > of > to > in > and > for > with > the
    # if multiple instances of a given word appear, use the first one(s)
    
    # Brute force solution: keep (re)adding/testing until the desired result is reached
    chr_needed <- abs(length(use_words) - ID_title_length) # number of characters to add
    for(re in removable) {
      regex_test <- paste0("^", re, "$")
      regex_count <- sum(str_detect(filler_words, regex_test)) # note you need the FULL title here
      
      if(regex_count < 1) {
        # Removable word does not appear in the original title, so try next one
        next
      } else {
        # Removable word does appear in the original title
        if (regex_count >= chr_needed) {
          # Enough (or more) instances of the removable word to pad out the ID, so
          # remove other words, then grab the first `chr_needed` instances of the kept word
          to_remove <- str_detect(filler_words,
                                  str_replace(removable_regex,
                                              paste0("\\|?\\^", re, "\\$"), ""))
          ID_words <- filler_words[!to_remove]
          
          if(length(ID_words) > ID_title_length) {
            # More words retained than `chr_needed`, so drop extra (later) ones
            rmv <- str_detect(ID_words, regex_test) %>%
              which(.) %>% 
              .[seq_len(chr_needed)]
            ID_words <- ID_words[-rmv]
          }
          
          ID_result4 <- extract_n_letters(ID_words)
        } else {
          # Less than enough instances of the removable word to pad out the ID
          # TBD
          # Have to save used word(s) (or at least indices of `filler_words`), then `next`
          # **WILL NEED TO ADJUST `chr_needed` -> move to different part of the loop
          #   (may need `while` loop anyway) or maybe update via `length(ID_words)`?
        }
      }
    }
  }
}
ID_result4


#### Test cases - authors ####
# Should be fairly straightforward, but what about multi-author works? Just use first author?
