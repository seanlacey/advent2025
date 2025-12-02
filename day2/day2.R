options(scipen = 999)

# Libraries ---------------------------------------------------------------
library(stringr)
library(purrr)

# Read in Data ------------------------------------------------------------
puzzle_input <- readLines("advent2025/day2/data/input.txt", warn=FALSE)

# Function: Parse Input ---------------------------------------------------
parse_input <- function(input) {
  split1 <- str_split(input, ",")[[1]]
  
  split2 <- map(split1, \(x) str_split(x,"-")[[1]])
  
  final_list <- list()
  
  for (i in 1:length(split2)){
    new_list <- list()
    
    new_list[["lower"]] <- as.numeric(split2[[i]][1])
    new_list[["upper"]] <- as.numeric(split2[[i]][2])
    new_list[["lower_length"]] <- nchar(split2[[i]][1])
    new_list[["upper_length"]] <- nchar(split2[[i]][2])
    new_list[["lower_mod2"]] <- new_list$lower_length %% 2
    new_list[["upper_mod2"]] <- new_list$upper_length %% 2
    
    ###If they're both the same length and that length is odd they can't contain a valid number
    if ((new_list$lower_length == new_list$upper_length) && (new_list$lower_mod2 != 0)) {
      new_list[["check"]] = FALSE
    } else {
      new_list[["check"]] = TRUE
    }
    
    final_list[[i]] <- new_list
  }
  
  return(final_list)
}

my_input <- parse_input(puzzle_input)


# Part 1 ------------------------------------------------------------------
part1_data <- list()

### Only keep valid ranges for checking
j <- 1

for (i in 1:length(my_input)) {
  if (my_input[[i]]$check == TRUE) {
    part1_data[[j]] <- my_input[[i]]
    j <- j + 1
  }
}

### First thing, I want to clean up the ranges. Numbers need to have an even number
### of digits to qualify.
for (i in 1:length(part1_data)) {
  if (part1_data[[i]]$lower_mod2 != 0) {
    part1_data[[i]][["lower_round"]] <- 10 ** part1_data[[i]]$lower_length
  } else {
    part1_data[[i]][["lower_round"]] <- part1_data[[i]]$lower
  }
  
  if (part1_data[[i]]$upper_mod2 != 0) {
    part1_data[[i]][["upper_round"]] <- 10 ** (part1_data[[i]]$upper_length - 1) - 1
  } else {
    part1_data[[i]][["upper_round"]] <- part1_data[[i]]$upper
  }
}

###Function to generate a list of potentials
potential_gen <- function(lower, upper) {
  ### Split the numbers
  lower_char <- as.character(lower)
  upper_char <- as.character(upper)
  
  lower1 <- as.numeric(substr(lower_char, 1, nchar(lower_char)/2))
  upper1 <- as.numeric(substr(upper_char, 1, nchar(upper_char)/2))
  
  ### Get range of first halves
  range1 <- lower1:upper1
  
  ### Create potential numbers for that range
  map_vec(range1, \(x) as.numeric(paste0(as.character(x),as.character(x))))
}

###Function that checks the range and returns list of invalid ids
check_range <- function(list_in) {
  potentials <- potential_gen(list_in$lower_round, list_in$upper_round)
  invalid_ids <- c()
  
  for (p in potentials) {
    if (p >= list_in$lower && p <= list_in$upper) {
      invalid_ids <- c(invalid_ids, p)
    }
  }
  
  return(invalid_ids)
}

invalid_ids <- c()

for (list_in in part1_data) {
  invalid_ids <- c(invalid_ids, check_range(list_in))
}

part1_answer <- sum(invalid_ids)

# Part 2 ------------------------------------------------------------------
### Unfortunately I can no longer discount odd lengths
part2_data <- my_input

### Function to create array of repeatable sections
lengthen_num <- function(num) {
  num_char <- as.character(num)
  
  num_list <- c()
  
  for (i in 1:nchar(num_char)) {
    num_list <- c(num_list, as.numeric(substr(num_char, 1, i)))  
  }
  
  return(num_list)
}

###Function to generate a list of potentials
invalid_gen <- function(list_in) {
  lower <- list_in$lower
  upper <- list_in$upper
  
  if (lower < 10) {
    lower <- min(10, upper)
  }
  
  ### Split the numbers
  lower_char <- as.character(lower)
  upper_char <- as.character(upper)
  
  nchar_range <- nchar(lower_char):nchar(upper_char)
  
  lower1 <- as.numeric(substr(lower_char, 1, floor(nchar(lower_char)/2)))
  upper1 <- as.numeric(substr(upper_char, 1, ceiling(nchar(upper_char)/2)))
  
  ### Get range of first halves
  range1 <- lower1:upper1
  
  invalid_ids <- c()
  
  for (val in range1) {
    num_list <- lengthen_num(val)
    
    for(num in num_list) {
      num_char <- as.character(num)
      
      for(nc in nchar_range) {
        num_nchar <- nchar(num_char)
        
        ### Can it go into the range evenly?
        reps <- nc / num_nchar
        if (floor(reps) == reps) {
          potential <- as.numeric(paste(rep(as.character(num), reps), collapse = ""))
          
          if (potential >= lower && potential <= upper) {
            invalid_ids <- c(invalid_ids, potential)
          }
          
        }
      }
    }
  }

  return(invalid_ids)
}

invalid_ids <- c()

for (list_in in part2_data) {
  invalid_ids <- c(invalid_ids, invalid_gen(list_in))
}

### I should deduplicate just in case
invalid_ids <- invalid_ids[!duplicated(invalid_ids)]

part2answer <- sum(invalid_ids)
