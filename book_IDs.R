library(tidyverse)

books <- read_csv("book_list.csv")

set.seed(2022)
books_sample <- slice_sample(books, n = 20)
