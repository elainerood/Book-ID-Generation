#### Setup ####
library(tidyverse)

# ID_title_length <- 5
ID_title_length <- 6

removable <- c("a", "an", "of", "to", "in", "on", "and", "for", "with", "the") %>% 
  str_to_upper() # titles will be caps for consistency
# ^ ordered by below preference for being retained
removable_regex <- paste0("^", removable, "$", collapse = "|")

format_title <- function(title) {
  ### Convert a string (book title) to the desired format for later use
  ## Arguments:
  ## - title = string
  # **TO DO: make this look/sound nicer
  
  str_remove_all(title, "[:punct:]") %>% # don't want punctuation in ID
    str_to_upper() %>%                   # caps for consistency
    str_split(boundary("word")) %>% 
    unlist()
}

extract_n_letters <- function(words_string, n = 1L, len = ID_title_length, exact_n = TRUE) {
  # Adapted from: stackoverflow.com/questions/58659318/
  ### Extract the first `n` letters of each element of character vector `words_string`,
  ### optionally truncating the resulting string to length `len`.
  ## Arguments:
  ## - words_string = character vector, each element is a word to grab letters from
  ##                  (here, generated via str_split(*, boundary("word")))
  ## - n = numeric (converted to integer); number of letters to extract (starts from beginning)
  ##       (must be integer due to `if_else` typing requirements;
  ##        if non-integer, see `as.integer` documentation for coercion rules)
  ## - len = numeric (integer); length of output (truncates result)
  ##         (if -1, no truncation; see `str_sub` documentation)
  ## - exact_n = logical; return exactly `n` characters for each word
  ##             (if TRUE, function will return `NA` if ANY word has < `n` characters)
  
  ## **TO DO: this doesn't handle titles that include numbers... not too bad though,
  ## mostly needs a determination on whether numbers vs digits count as "words"
  ## (e.g. "13 Candles on a Shelf" --> "13COAS" vs "1COAS"), and an update to the regex
  
  n <- as.integer(n)
  
  if(exact_n) {
    extracted <- str_extract(words_string, paste0("^[:alpha:]{", n, "}"))
  } else {
    # str_extract() will return NA if n > the string length, so vary the requested length if needed
    extracted <- str_extract(words_string,
                paste0("^[:alpha:]{",
                       if_else(str_length(words_string) < n,
                               str_length(words_string),
                               n),
                       "}"
                )
    )
  }
  
  extracted %>% 
    unlist() %>%
    str_flatten() %>% 
    str_sub(start = 1, end = len)
}



#### Read and set up data ####
books <- read_csv("book_list.csv")

set.seed(2022)
books_sample <- slice_sample(books, n = 20) %>%
  mutate(words = map(title, format_title))



#### Test cases - titles ####
single_word <- books_sample$words[8] %>% unlist() # Thud!
many_words <- books_sample$words[10] %>% unlist() # TDitW
extreme_words <- books_sample$words[3] %>% unlist() # Falchester
filler_words <- books_sample$words[12] %>% unlist() # ASatCoG

# none of the titles in the sample have what I need for this test case
fake_words <- books_sample$words[19] %>% unlist() # ItLoD
fake_words[5:6] <- c("A", "DRAKE")


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
  else {
    # bring back removable word(s), with preference:
    #   a > of > to > in > and > for > with > the
    # if multiple instances of a given word appear, use the first one(s)
    
    # Brute force solution: keep (re)adding/testing until the desired result is reached
    chr_needed <- abs(length(use_words) - ID_title_length) # number of characters to add
    used_regex <- character()
    continue <- TRUE
    
    while(continue) {
      for(re in removable) {
        regex_test <- paste0("^", re, "$")
        regex_count <- sum(str_detect(filler_words, regex_test)) # note you need the FULL title here
        
        if(regex_count < 1) {
          # Removable word does not appear in the original title, so try next one
          next
          print("regex ct < 1; next")
        } else {
          # Removable word does appear in the original title
          
          # Need to move `<` scenario above this (& change if-condition),
          # otherwise could get less-preferred words used first, if there
          # are more of them? otherwise e.g., many "OF"s in a title could knock
          # out a number of "A"s that isn't enough to reach the min length on its own
          
          if (regex_count >= chr_needed) {
            # Enough (or more) instances of the removable word to pad out the ID, so
            # remove other words, then grab the first `chr_needed` instances of the kept word
            to_remove <- str_detect(filler_words,
                                    # text against `removable_regex` without the kept-word
                                    str_remove(removable_regex,
                                                paste0("\\|?\\^", re, "\\$")))
            ID_words <- filler_words[!to_remove]
            
            if(length(ID_words) > ID_title_length) {
              # More words retained than `chr_needed`, so drop the one(s) that show up later
              rmv <- str_detect(ID_words, regex_test) %>%
                which(.) %>% 
                .[seq_len(chr_needed)]
              ID_words <- ID_words[-rmv]
            }
            
            ID_result4 <- extract_n_letters(ID_words)
            # **TEST THIS
            continue <- FALSE
            print("regex > chr; break")
            break
          } else {
            # Less than enough instances of the removable word to pad out the ID
            # See if using previous words gets up to enough characters
            used_regex <- c(used_regex, re)
            short_rmv_regex <- str_remove_all(removable_regex,
                                               paste0("\\|?\\^", used_regex, "\\$", collapse = "|")
                                              ) %>% 
              str_remove_all("\\|$|^\\|") # remove any leading/trailing pipes
            
            to_remove <- str_detect(filler_words, short_rmv_regex)
            ID_words <- filler_words[!to_remove]
            
            if(length(ID_words) >= ID_title_length) {
              ID_result4 <- extract_n_letters(ID_words)
              # **TEST THIS
              continue <- FALSE
              print("regex < chr; break")
              break
            } else {
              next
              print("regex > chr; next")
            }
            # Possible: ID is not long enough even with ALL removable-words added back;
            # keep the < `ID_title_length` version, or pad it out?
            # Padding out would require (preferentially) taking the first `n` characters of
            # (certain) words instead of only the first 1 character
          }
        }
      }
      # **for testing only
      print("infinite while; break")
      break
    }
  }
}
ID_result4

#### Function-ify ####
generate_title_ID <- function(title, len = ID_title_length) {
  ### Generate an ID of a given length for a provided string (book title)
  ## Arguments:
  ## - title = character vector where each element is a word to grab letters from
  ##           -OR-
  ##           string that will be transformed into the above format
  ## - len = numeric (integer); defaults to desired ID length
  
  # **TO DO: pad out short IDs to the correct length? e.g., "THUD" -> "THUDxx"
  # (the pad-characters, if letters, would use lowercase since titles are caps;
  # not sure about using special characters e.g. underscore)
  
  title <- format_title(title) # does not affect pre-formatted input
  
  if(length(title) == 1) {
    ## Single word title
    ID_result <- str_sub(title, 1, len)
    return(ID_result)
    
  } else if(length(title) == len) {
    ## Title has n = `ID_title_length` words
    ID_result <- extract_n_letters(title, len = len)
    return(ID_result)
    
  } else if(length(title) < len) {
    ## Title has n < `ID_title_length` words
    if(sum(str_length(title)) < len) {
      ## Title does not have enough characters; use full title as ID
      ID_result <- paste0(title, collapse = "")
      return(ID_result)
      
    } else {
      ## Title has enough characters but not enough words
      rm_words <- str_detect(title, removable_regex)
      
      # **TO DO: alternative to below = if padding too-short titles, this can be padded too
      # could add a `pad` argument to the function, to make things more complicated
      
      if(any(rm_words)) {
        ## Removable words exist in the title
        use_words <- title[!rm_words]

        if(sum(str_length(use_words)) > len) {
          ## Title has enough characters without removable words
          ID_result <- extract_n_letters(use_words, n = ceiling(len/length(use_words)))
          
        } else {
          # **TO DO: this is hard to think about all situations for, so may need
          # to revisit after testing, if results aren't what's desired
          # (specifically, wondering about short kept-words causing problems, but
          # that probably isn't too common?)
          rmv_n <- len - str_length(use_words)
          keep_n <- ceiling(
            (len - rmv_n)/length(use_words)
            # number of characters available for kept-words divided by number of kept-words
          )
          
          if((rmv_n + keep_n) == len) {
            ID_result <- str_extract(title,
                                   paste0("^[:alpha:]{", if_else(rm_words, rmv_n, keep_n), "}")
                                   ) %>% 
            str_flatten()
          } else {
            print("too short - removable words; rmv+keep <> IDlen")
          }
        }
        
      } else {
        ## No removable words exist in the title
        # (have to use ceiling() instead of floor() because otherwise the result
        # doesn't necessarily meet `ID_title_length`)
        # could add a tweak for prefer early/late words, but that would mean a new setup
        # (check total length prior to str_flatten(), so individual words' trims can be adjusted?)
        ID_result <- extract_n_letters(title,
                                       n = ceiling(len/length(title)),
                                       exact_n = FALSE)
        # **TO DO: want this to have at least one letter for each word, which
        # it currently doesn't do (e.g., if `ID_title_length` is 6:
        # - In the Labyrinth of Drakes -> "LABDRA" instead of "ITLODR"
        # - She Who Became the Sun -> "SHWHBE" instead of "SWBTSU")
        # I'm going to be chasing this kind of thing down forever but at some point it'll be Good Enough
      }
      
      return(ID_result)
    }
    
  } else if(length(title) > len) {
    ## Title has too many words
    rm_words <- str_detect(title, removable_regex)
    use_words <- title[!rm_words]
    
    if(length(use_words) >= len) {
      ID_result <- extract_n_letters(use_words, len = len)
      return(ID_result)
    }
    # **TO DO: could restructure this to cut down on repeated steps like above?
    # e.g., calculate `use_words` at the beginning, & change the first else-if to
    # (`title` OR `use_words` == `ID_title_length`)
    
    if(length(use_words) < len) {
      print("title too long - use_words too short")
    }
    
    # **TO DO: for too-short-with-removed-words, split into pieces made into functions?
    # might be able to simplify/reduce loops that way
    # e.g., count instances of each removable word & use cumsum() to determine which are needed
    # --> also allows for quick check of "there aren't enough total characters"
    
    # TBD
    
    # ID_result <- x
    # return(ID_result)
  }
}

generate_title_ID(books_sample$title[8]) # Thud!
generate_title_ID(books_sample$title[10]) # TDitW
generate_title_ID("A Sun")
generate_title_ID("The Test")
generate_title_ID(books_sample$title[2]) # PotS
generate_title_ID(books_sample$title[11]) # AJ
generate_title_ID(books_sample$title[3]) # Falchester
generate_title_ID(books_sample$title[12]) # ASatCoG
generate_title_ID("In the Labyrinth of A Drake")
# generate_title_ID("13 Candles on a Shelf") # for later testing

# mutate(books, title_id = map(title, generate_title_ID)) %>% View()

#### Test cases - authors ####
# Should be fairly straightforward, but what about multi-author works? Just use first author?
