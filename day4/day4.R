# Libraries ---------------------------------------------------------------
library(readr)
library(purrr)

# Load Data ---------------------------------------------------------------

### Get number of columns
data_name <- "input"

num_col <- nchar(readLines(paste0("advent2025/day4/data/", data_name, ".txt"), n = 1)) |>
  as.numeric()

in_data <- read_fwf(paste0("advent2025/day4/data/", data_name, ".txt"), col_positions = fwf_widths(rep(1, num_col))) |>
  as.matrix()

num_row <- nrow(in_data
                )
# Part 1 ------------------------------------------------------------------

### Define move space
move_space <- c()

for (i in -1:1) {
  for (j in -1:1) {
    if (!(i == 0 && j == 0)) {
      move_space <- c(move_space, complex(real = i, imaginary = j)) 
    }
  }
}

### Transform all paper rolls into coordinates
paper_rolls <- c()

### Not sure if I'll need later, let's capture now
empty_space <- c()

for (x in 1:num_col) {
  for (y in 1:num_row) {
    if (in_data[y,x] == "@") {
      paper_rolls <- c(paper_rolls, complex(real = x, imaginary = y))     
    } else if (in_data[x,y] == ".") {
      empty_space <- c(empty_space, complex(real = x, imaginary = y))
    }
  }
}

### Function to get the neighbor numbers from a coordinate (and ensure they are on the grid)
neighbor_gen <- function(roll) {
  neighbors <- roll + move_space
  
  ###Remove any that fall out of bounds
  neighbors <- neighbors[which((Re(neighbors) >= 1) & 
                                 (Re(neighbors) <= num_col) &
                                 (Im(neighbors) >= 1) &
                                 (Im(neighbors) <= num_row))]
  
  return(neighbors)
}

### Function, take a paper roll and see if it is accessible
determine_access <- function(roll) {
  neighbors <- neighbor_gen(roll)
  
  if (sum(neighbors %in% paper_rolls) >= 4) {
    return(FALSE)
  } else {
    return(TRUE)
  } 
}

# part1answer <- sum(map_vec(paper_rolls, determine_access))
# 
# print(paste0("Part 1 Answer: ", part1answer))

# Part 2 ------------------------------------------------------------------

total_removed <- 0
iteration <- 0
remove_zero <- 0

while (remove_zero == 0) {
  iteration <- iteration + 1
  
  removal_candidates <- map_vec(paper_rolls, determine_access)
  
  removed <- sum(removal_candidates)
  
  if (removed == 0) {
    remove_zero <- 1
    
    print(paste0("Iteration ", iteration, ": No more to be removed"))
  } else {
    total_removed <- total_removed + removed
    
    paper_rolls <- paper_rolls[!(removal_candidates)]
    
    print(paste0("Iteration ", iteration, ": ", removed, " removed"))
  }
}

print(paste0("Part 2 Answer: ", total_removed))

