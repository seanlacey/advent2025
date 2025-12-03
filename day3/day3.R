# Libraries ---------------------------------------------------------------
library(stringr)
library(purrr)

# Read in and process Data ------------------------------------------------
data_name <- "input"

my_data <- readLines(paste0("advent2025/day3/data/", data_name, ".txt"), warn = FALSE)

# Part 1 ------------------------------------------------------------------
jolt_parse <- function(string) {
  ### Split string into a vector one character each
  string_vec <- str_split_1(string, '')
  
  ### Get first instance of max digit location (cannot be from end of string)
  first_max_loc <- which.max(string_vec[1:(length(string_vec) - 1)])
  
  ###Starting from that location, find the next maximum
  second_max_loc <- which.max(string_vec[(first_max_loc + 1):length(string_vec)]) + first_max_loc
  
  jolt_value <- paste0(string_vec[first_max_loc],string_vec[second_max_loc])
  
  return(as.numeric(jolt_value))
}

jolt_values_1 <- map_vec(my_data,jolt_parse)

part1answer <- sum(jolt_values_1)

print(paste0("Part 1 Answer: ", part1answer))

# Part 2 ------------------------------------------------------------------
jolt_parse_12 <- function(string) {
  ### Split string into a vector one character each
  string_vec <- str_split_1(string, '')
  
  ### Initialize total digits, vector to hold digits, and current digit vec
  total_digits <- 12
  digit_vec <- c()
  current_vec <- string_vec
  
  ### Loop, we will do while on digit
  digit <- 12
  
  while (digit > 0) {
    ###If current_vec is exactly the length of the remaining digits, we shortcut
    if (length(current_vec) == digit) {
      digit_vec <- c(digit_vec, current_vec)
      digit <- 0
    } else {
      ###Get location of the max digit (note we need to leave space to ensure we 
      ### have enough remaining digits)
      max_loc <- which.max(current_vec[1:(length(current_vec) - (digit - 1))])
      
      ###Add digit to digit_vec
      digit_vec <- c(digit_vec, current_vec[max_loc])
      
      ###Update current_vec
      current_vec <- current_vec[(max_loc+1):length(current_vec)]
      
      digit <- digit - 1
    }
  }
  
  digit_num <- as.numeric(paste(digit_vec, collapse = ''))
  
  return(digit_num)
}

jolt_values_2 <- map_vec(my_data,jolt_parse_12)

part2answer <- sum(jolt_values_2)

print(paste0("Part 2 Answer: ", part2answer))
