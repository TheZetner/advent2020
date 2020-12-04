# Day 3
# 20201203

# Libraries and Setup####

library(utilitarian)
libraries(dplyr, readr, tidyr, stringr, purrr)

# Get Data ####

i <- read_csv("data/3-input.txt", col_names = F)

# Part A ####

l <- map(i$X1, ~ str_replace_all(.x, "\\.", "0") %>%
  str_replace_all("\\#", "1") %>%
  strsplit("") %>%
  unlist() %>%
  as.numeric())

x <- 1
y <- 1
trees <- 0

for (s in 1:3) {
  for (ss in x:x+3) {
    if(x%%31 != 0){
      trees <- sum(trees + l[[s]][x%%31])
    } else{
      trees <- sum(trees + l[[s]][31])
    }
  }
  x <- x+3
}


# Part B ####
