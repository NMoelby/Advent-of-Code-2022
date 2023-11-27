# Load Packages and data --------------------------------------------------

library(tidyverse)
library(unglue)

assignments_raw <- read_lines("data/d04.txt")

assignments <-
  assignments_raw %>%
  unglue_data(patterns = "{first_elf_start}-{first_elf_end},{second_elf_start}-{second_elf_end}", convert = TRUE) %>%
  as_tibble()

# Define helpers ----------------------------------------------------------

fully_contained <- function(a, b, x, y) {
  res <-
    if_else(
    # first elf contains second elf
    a <= x && b >= y,
    TRUE,
    # second elf contains first elf
    if_else(
      a >= x && b <= y,
      TRUE,
      FALSE
    ),
    FALSE
  )
  return(res)

}
fully_contained <- Vectorize(fully_contained)

overlap <- function(a, b, x, y) {
  res <-
    if_else(
      a >= x && a <= y || b >= x && b <= y,
      TRUE,
      if_else(
        x >= a && x <= b || y >= a && y <= b,
        TRUE,
        FALSE
      ),
      FALSE
    )
  return(res)

}
overlap <- Vectorize(overlap)

# PART I ------------------------------------------------------------------

# In how many assignment pairs does one range fully contain the other?

assignments %>%
  mutate(
    fully_contained = fully_contained(first_elf_start, first_elf_end, second_elf_start, second_elf_end)
  ) %>%
  count(wt = fully_contained)

# PART II -----------------------------------------------------------------

# In how many assignment pairs do the ranges overlap?

assignments %>%
  mutate(
    overlap = overlap(first_elf_start, first_elf_end, second_elf_start, second_elf_end)
  ) %>%
  count(wt = overlap)
