# Load packages and data --------------------------------------------------

library(tidyverse)

rucksacks_raw <- read_lines("data/d03.txt")

rucksacks <-
  rucksacks_raw %>%
  enframe(name = NULL, value = "items")

# Define helpers ----------------------------------------------------------

priority <-
  c(letters, LETTERS) %>%
  set_names(x = seq_along(.), nm = .)

find_common <- function(x, y){
  uniq_x <- unique(str_split_1(x, pattern = ""))
  uniq_y <- unique(str_split_1(y, pattern = ""))
  res <- intersect(uniq_x, uniq_y)
  return(res)
}
find_common <- Vectorize(find_common)

find_common_revised <- function(x, y, z){
  uniq_x <- unique(str_split_1(x, pattern = ""))
  uniq_y <- unique(str_split_1(y, pattern = ""))
  uniq_z <- unique(str_split_1(z, pattern = ""))
  int_xy <- intersect(uniq_x, uniq_y)
  res <- intersect(int_xy, uniq_z)
  return(res)
}
find_common_revised <- Vectorize(find_common_revised)

# PART I ------------------------------------------------------------------

# Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?

rucksacks %>%
  mutate(
    items_count = nchar(items),
    first_compartment = str_sub(items, start = 1, end = (items_count / 2)),
    second_compartment = str_sub(items, start = (items_count / 2) + 1, end = -1),
    common = find_common(first_compartment, second_compartment),
    priority_value = recode(common, !!!priority)
  ) %>%
  count(wt = priority_value)

# PART II -----------------------------------------------------------------

# Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?

rucksacks %>%
  mutate(
    group = rep(seq.int(from = 1, to = NROW(.) / 3), each = 3),
    rucksack = rep(c("first", "second", "third"), length.out = NROW(.))
    ) %>%
  pivot_wider(
    names_from = rucksack,
    values_from = items
  ) %>%
  mutate(
    common = find_common_revised(first, second, third),
    priority_value = recode(common, !!!priority)
  ) %>%
  count(wt = priority_value)
