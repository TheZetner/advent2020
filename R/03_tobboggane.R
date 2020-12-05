# Day 3
# 20201203


# 3 over, 1 down
# ....#..#.................#..#..
# #..#.#.#..#.###.#..#...#..#....
# .#....#......#.#.#..##...#...#.
# .............#.#..#........#.#.
# ............##.#..#...##.###...
# .....#..#......#......##.......
# ........##........#...........#


# Libraries and Setup####

library(utilitarian)
libraries(dplyr, readr, tidyr, stringr, purrr)

# Get Data ####

# Raw input as previewed above
raw <- read_csv("data/3-input.txt", col_names = F)
# raw <- read_csv("data/3-example.txt", col_names = F)

# List of each pattern
l <- map(raw$X1, ~ str_replace_all(.x, "\\.", "0") %>%
           str_replace_all("\\#", "1") %>%
           strsplit("") %>%
           unlist() %>%
           as.numeric())

pat <- as.numeric(length(l[[1]]))

# Matrix 323x31 of pattern
m <- data.frame(matrix(unlist(l), ncol = pat, byrow = TRUE))

# Positions defined by 3/1 slope
df <-
  tibble(ypos = 1:length(l),
         xpos = 1 + 3*(ypos-1)) %>%
  mutate(across(everything(), as.numeric))

# Part A ####

dat <-
  df %>%
  tail(-1) %>%
  mutate(xpat = if_else(xpos %% pat == 0, pat, xpos %% pat)) %>%
  rowwise() %>%
  mutate(val = m[ypos, xpat])

sum(dat$val) # 237

# Part B ####

# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

slopes <- tibble(rise = c(1, 1, 1, 1, 2),
                 run = c(1, 3, 5, 7, 1))


slopelist <-
  pmap(slopes, ~ tibble(ypos = seq(1, length(l), by = ..1)) %>%  # Shorthand notation for columns in slopes
         mutate(xpos = c(1, rep.int(..2, nrow(.)-1)),
                xpos = cumsum(xpos)) %>%
         mutate(across(everything(), as.numeric)))


datlist <-
  map(slopelist, ~ .x %>%
        mutate(xpat = if_else(xpos %% pat == 0, pat, xpos %% pat)) %>%
        rowwise() %>%
        mutate(val = m[ypos, xpat]))

# Sanity check. Ungroup to turn off rowwise
datlist[[2]] %>% ungroup() %>% summarise(trees = sum(val))


bind_rows(datlist, .id = "slope") %>%
  group_by(slope) %>%
  summarise(trees = sum(val)) %>%
  summarise(prod(trees))

sum(dat$val)

