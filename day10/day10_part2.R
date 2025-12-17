## *****************************************************************************
## # Day 10 Part 2                                                   ----
##
## Description: This is going to use the advice of a reddit post to solve it
## It will largely use the logic from part one with adjustments based on the 
## reddit post
## https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
##
## Conclusion: I ended up needing a lot of help understanding what was going on but
## I've learned a ton through the process. This day is definitely where I hit a wall
## but proud I stuck through it and got to an answer despite all the help
## *****************************************************************************

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
    zero_state = NULL,
    jolt_goal = NULL,
    buttons = NULL,
    num_ind = NULL,
    num_buttons = NULL,
    button_combos = NULL,
    cache = NULL,
    effect_to_buttons = NULL,
    
    initialize = function(machine_string) {
      indicator_split <- regexpr("]", machine_string)
      jolt_split <- regexpr("\\{", machine_string)
      
      jolt_char <- substr(machine_string, jolt_split + 1, nchar(machine_string) - 1)
      buttons_char <- substr(machine_string, indicator_split + 2, jolt_split - 2)
      
      ###Convert Joltage to integer vector
      self$jolt_goal <- str_split_1(jolt_char, ",") |> as.integer()
      self$num_ind <- length(self$jolt_goal)
      
      ### All False state for easy reference
      self$zero_state <- rep(FALSE, self$num_ind)

      buttons_sep <- str_split_1(buttons_char, " ")
      
      self$buttons <- map(buttons_sep, self$parse_buttons)
      
      self$num_buttons <- length(self$buttons) |> as.numeric()
      
      ### Store button combinations
      button_combos <- do.call(c, lapply(seq_along(1:self$num_buttons), 
                                              combn, x = 1:self$num_buttons, 
                                              simplify = FALSE))
      
      button_effects <- lapply(button_combos, self$press_buttons)
      
      ### Initialize button hashes
      self$effect_to_buttons <- new.env(hash = TRUE)
      
      for (i in seq_along(button_combos)) {
        combo <- button_combos[[i]]
        effect <- button_effects[[i]] %% 2

        effect_key <- paste(effect, collapse = ",")

        if (exists(effect_key, envir = self$effect_to_buttons)) {
          self$effect_to_buttons[[effect_key]] <- c(self$effect_to_buttons[[effect_key]], list(combo))
        } else {
          self$effect_to_buttons[[effect_key]] <- list(combo)
        }
      }
      
      ### Initialize Cache for Memoisation
      self$cache <- new.env(hash = TRUE)
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
      start_state <- self$zero_state
      
      Reduce('+', buttons_to_press, init = start_state)
    },
    find_jolt_min = function(joltages = self$jolt_goal) {
      ###Generating the key for hash table (will be used in the memoisation)
      key <- paste(joltages, collapse = ",")
      
      ###Checks if key is in the hash, if so, return that result
      if (exists(key, envir = self$cache)) {
        return(self$cache[[key]])
      }
      
      ###If the joltages are all 0, then nothing to be done
      if (all(joltages == 0)) {
        self$cache[[key]] <- 0
        return(0)
      }
      
      ###Get solve list for binary joltages
      solve_list <- self$effect_to_buttons[[paste(joltages %% 2, collapse = ",")]]
      
      min_result <- Inf
      
      ### If all are even, we need to do special checks
      if (all((joltages %% 2) == 0)) {
        ###In either case, we want to first check the case where we just divide by 2
        new_joltage <- joltages / 2
        
        min_result <- 2 * self$find_jolt_min(new_joltage)
      }
      
      ### If there are qualifying button presses, we process those here
      if (!is.null(solve_list)) {
        for (current_press in solve_list) {
          button_effects <- Reduce('+', self$buttons[current_press], init = rep(0, self$num_ind))
          new_joltage_raw <- joltages - as.integer(button_effects)
          
          # Check if all are even (divisible by 2)
          if (any(new_joltage_raw %% 2 != 0)) {
            ###Shouldn't happen but want to see it printed if it does
            print(paste0("Not all even: ", paste(new_joltage_raw, collapse = ",")))
            next  # Skip this combo
          }
          
          ###Skip if any of the button presses bring us into negatives
          if (any(new_joltage_raw < 0)) {
            next
          } 
          
          new_joltage <- new_joltage_raw / 2
          
          result <- (2 * self$find_jolt_min(new_joltage)) + length(current_press)
          
          min_result <- min(min_result, result)
        }
      }
      
      self$cache[[key]] <- min_result
      
      return(min_result)
    }
  )
)

machines <- map(in_data, \(x) Machine$new(x))
min_list <- map(machines, \(x) x$find_jolt_min())

part2answer <- Reduce(`+`, min_list, init = 0)

print(paste0("Part 2 Answer: ", part2answer))
Sys.time() - s
