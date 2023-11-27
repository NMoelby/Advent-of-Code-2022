# Load packages and data --------------------------------------------------
library(tidyverse)


secret_guide_raw <- read_lines("data/d02.txt")

secret_guide <-
  secret_guide_raw %>%
  enframe(name = NULL) %>%
  separate(
    col = value,
    into = c("op", "me")
  )

# Define helpers ----------------------------------------------------------

op_ref <- c("A" = "rock", "B" = "paper", "C" = "scissor")
me_ref <- c("X" = "rock", "Y" = "paper", "Z" = "scissor")

shape_score <- c("rock" = 1, "paper" = 2, "scissor" = 3)
outcome_score <- c("lose" = 0, "draw" = 3, "win" = 6)

outcome <- function(op, me) {
  res <- "win"
  if (op == me) res <- "draw"
  #rock > paper > scissor
  if (op == "rock" && me == "scissor") res <- "lose"
  if (op == "scissor" && me == "paper") res <- "lose"
  if (op == "paper" && me == "rock") res <- "lose"
  return(res)
}
outcome <- Vectorize(outcome)

me_ref_revised <- c("X" = "lose", "Y" = "draw", "Z" = "win")

me_shape <- function(op, me) {
  if (me == "draw") res <- op
  if (op == "rock" && me == "lose") res <- "scissor"
  if (op == "scissor" && me == "lose") res <- "paper"
  if (op == "paper" && me == "lose") res <- "rock"
  if (op == "rock" && me == "win") res <- "paper"
  if (op == "scissor" && me == "win") res <- "rock"
  if (op == "paper" && me == "win") res <- "scissor"
  return(res)
}
me_shape <- Vectorize(me_shape)

get_shape <- function(op, outcome) {
  #rock > paper > scissor
  assigned_values <- c("rock" = 3, "scissor" = 2, "paper" = 1)
  if (outcome == "draw") inc <- 0
  if (outcome == "win") {
    inc <- 1
    if (op == "rock") {
      assigned_values[["paper"]] <- 4
    }
  }
  if (outcome == "lose") {
    inc <- -1
    if (op == "paper") {
      assigned_values[["paper"]] <- 4
    }
  }
  op_value <- recode(op, !!!assigned_values)
  my_value <- op_value + inc
  res <- names(assigned_values[assigned_values == my_value])
  return(res)
}
get_shape <- Vectorize(get_shape)

# PART I ------------------------------------------------------------------
# What would your total score be if everything goes exactly according to your strategy guide?

secret_guide %>%
  mutate(
    op = recode(op, !!!op_ref),
    me = recode(me, !!!me_ref),
    outcome = outcome(op, me),
    outcome_score = recode(outcome, !!!outcome_score),
    shape_score = recode(me, !!!shape_score),
    total_score = shape_score + outcome_score
  ) %>%
  count(wt = total_score, name = "total_score", sort = TRUE)

# PART II -----------------------------------------------------------------
# Following the Elf's instructions for the second column, what would your total score be if everything goes exactly according to your strategy guide?

# v1
secret_guide %>%
  mutate(
    op = recode(op, !!!op_ref),
    outcome = recode(me, !!!me_ref_revised),
    me = me_shape(op, outcome),
    outcome_score = recode(outcome, !!!outcome_score),
    shape_score = recode(me, !!!shape_score),
    total_score = shape_score + outcome_score
  ) %>%
  count(wt = total_score, name = "total_score", sort = TRUE)

# v2
secret_guide %>%
  mutate(
    op = recode(op, !!!op_ref),
    outcome = recode(me, !!!me_ref_revised),
    me = get_shape(op, outcome),
    outcome_score = recode(outcome, !!!outcome_score),
    shape_score = recode(me, !!!shape_score),
    total_score = shape_score + outcome_score
  ) %>%
  count(wt = total_score, name = "total_score", sort = TRUE)
