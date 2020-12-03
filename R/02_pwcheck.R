# Day 2
# 20201202

# Libraries and Setup####

library(utilitarian)
libraries(dplyr, readr, tidyr, stringr)



# Get Data ####

i <- read_csv("data/2-input.txt", col_names = F)


# Part A ####

a <-
  i %>%
  tidyr::separate(X1, into = c("Rule", "Password"), sep = ": ") %>%
  tidyr::separate(Rule, into = c("Num", "Char"), sep = " ") %>%
  tidyr::separate(Num, into = c("Min", "Max"), sep = "-", convert = TRUE) %>%
  mutate(Count = str_count(Password, Char),
         Pass = eval(str_count(Password, Char) >= Min & str_count(Password, Char) <= Max)) %>%
  select(Min, Count, Max, Pass, everything()) %>% arrange(Min, Max)

count(a, Pass)


# Part b ####

# Rowwise required because this function doesn't play well with dplyr

checkPass <- function(x, char, pos){
  x <- unlist(strsplit(as.character(x), ""))
  sum(x[pos] == char) == 1
}

b <-
  i %>%
  tidyr::separate(X1, into = c("Rule", "Password"), sep = ": ") %>%
  tidyr::separate(Rule, into = c("Num", "Char"), sep = " ") %>%
  tidyr::separate(Num, into = c("Min", "Max"), sep = "-", convert = TRUE) %>%
  rowwise() %>%
  mutate(Pass = checkPass(Password, Char, c(Min, Max)))

count(b, Pass)
