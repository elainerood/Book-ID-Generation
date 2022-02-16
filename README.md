# Generating unique IDs for books

## Background

As part of tracking my reading habits, I assign each book an ID number. I've been using the Goodreads ID for the edition being read, but a string of numbers isn't super meaningful in isolation. Now, if I only had a handful of books to change, this would be easy to do manually. But I have *more than 600 books* that would need to be changed. So I thought, why not try to generate them systematically?

## Desired ID format

(subject to change as the process continues)

**LLFF-TTTTT**

**L** = author last name  
**F** = author first name  
**T** = story title