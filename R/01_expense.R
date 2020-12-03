# Day 1
# 20201201

# Libraries

library(purrr)
library(dplyr)


# Get Data ####

# Apparently HTTPS is stymieing my normal readLines() from "https://adventofcode.com/2020/day/1/input"
# Just download the text instead

i <- as.integer(readLines("data/1-input.txt"))



# Part A ####

purrr::walk(i, function(x){
  z <- x + i
  if(2020 %in% z){
    m <- i[match(2020, z)]
    print(paste("Found", x, "and", m, "which multiply to", x*m))
  }
  })


# Part B ####

# This could probably be refactored into one step. Meh.

# First create a list of vectors summing up each element of i with each other
l <- purrr::map(i, ~ .x + i)

# Subtract those sums from 2020: the difference is the remaining element. Find that in i, discard the rest. Get Product
purrr::map(l, function(x){
  z <- 2020 - x
  if(any(c(z) %in% i)){
    m <- x[match(z, i)] - i[match(z, i)]
    m[!is.na(m)]
  }
}) %>%
  purrr::discard(is.null) %>%
  unlist() %>%
  unique() %>%
  cumprod()







