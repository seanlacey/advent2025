# Libraries ---------------------------------------------------------------
library(stringr)
library(dplyr)
library(collections)
library(R6)
library(purrr)

# Load Data ---------------------------------------------------------------
s <- Sys.time()
data_name <- "input"

in_data <- readLines(paste0("advent2025/day10/data/", data_name, ".txt"), warn = FALSE)

###Define Button Object
Machine <- R6Class(
  classname = "Machine",
  public = list(
    indicators_goal = NULL,
    jolt_state = NULL,
    indicators_state = NULL,
    jolt_goal = NULL,
    buttons = NULL,
    num_ind = NULL,
    num_buttons = NULL,
    q = NULL,
    
    initialize = function(machine_string) {
      indicator_split <- regexpr("]", machine_string)
      jolt_split <- regexpr("\\{", machine_string)
      
      indicators_char <- substr(machine_string, 2, indicator_split - 1)
      buttons_char <- substr(machine_string, indicator_split + 2, jolt_split - 2)
      
      ###Convert Indicator Lights to logical vectors
      self$num_ind <- nchar(indicators_char) |> as.numeric()
      self$indicators_goal <- str_split_1(indicators_char, "") == "#"
      
      ### Initialize all lights to off
      self$indicators_state <- rep(FALSE, self$num_ind)

      buttons_sep <- str_split_1(buttons_char, " ")
      
      self$buttons <- map(buttons_sep, self$parse_buttons)
   
      self$num_buttons <- length(self$buttons) |> as.numeric()
      
      button_combinations <- do.call(c, lapply(seq_along(1:self$num_buttons), combn, x = 1:self$num_buttons, simplify = FALSE))
      self$q <- queue(button_combinations)
    },
    ###Convert Buttons to Matrix Layout
    parse_buttons = function(button_in) {
      button_nums <- substr(button_in, 2, nchar(button_in) - 1) |>
        str_split_1(",") |>
        as.numeric() + 1 ###Adjust to R indexing
      
      button_vec <- rep(FALSE, self$num_ind)
      button_vec[button_nums] <- TRUE
      
      return(button_vec)
    },
    ###Press a button
    press_buttons = function(button_press) {
      buttons_to_press <- self$buttons[button_press]
      start_state <- self$indicators_state
      
      Reduce(xor, buttons_to_press, init = start_state)
    },
    find_min_press = function() {
      ###If the goal is to have all false, we're already there
      if (all(self$indicators_goal == self$indicators_state)) {
        return(0)
      } else {
        ### The loop will break as soon as we find a match and
        ### return the presses.
        while (self$q$size() > 0) {
          cur_press <- self$q$pop()
          result <- self$press_buttons(cur_press)
          
          if (all(result == self$indicators_goal)) {
            return(length(cur_press))
          }
        }
        return(NULL)
      }
    }
  )
)

machines <- map(in_data, \(x) Machine$new(x))

answer_vec <- map_vec(machines, \(x) x$find_min_press())

print(paste0("Part 1 Answer: ", sum(answer_vec)))
Sys.time() - s
