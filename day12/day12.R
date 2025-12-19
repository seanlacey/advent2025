### This solution was mostly me but I saw some spoilers on the subreddit
### that convinced me to stop and try to use the answer I had for the
### will_it_fit step, and it turned out right. As you can see, I was trying
### to set up for a much larger and involved solve

# Libraries ---------------------------------------------------------------
library(stringr)
library(dplyr)
library(purrr)

# Load Data ---------------------------------------------------------------
s <- Sys.time()
data_name <- "input"

in_data <- readLines(paste0("advent2025/day12/data/", data_name, ".txt"), warn = FALSE)

###Parse Data
in_splits <- which(in_data == "")

present_vecs <- list()

for (i in seq_along(in_splits)) {

  if (i == 1) {
    present_vecs[[i]] <- in_data[1:(in_splits[i] - 1)]
  } else {
    present_vecs[[i]] <- in_data[(in_splits[i - 1] + 1):(in_splits[i] - 1)]
  }
}

tree_vecs <- in_data[(in_splits[length(in_splits)] + 1):length(in_data)]

## *****************************************************************************
## # Parsing Presents                                                 ----
##
## *****************************************************************************
###Present parse functions
shape_func <- function(shape_row) {
  (str_split_1(shape_row, "") == "#") |> as.integer()
}

parse_presents <- function(cur_present) {
  ###I'm incrementing it by 1 to match R indexing
  present_name <- paste0("P", as.numeric(substr(cur_present[1], 1, nchar(cur_present[1]) - 1)) + 1)
  shape <- unname(t(sapply(cur_present[-1], shape_func)))
  
  area <- sum(shape == 1)
  
  ###Create rotations. Feel we should pre allocate
  shapes_list <- list(rotate0 = shape,
                      rotate90 = t(shape[nrow(shape):1,]),
                      rotate180 = shape[nrow(shape):1, ncol(shape):1],
                      rotate270 = t(shape)[ncol(shape):1,])

  shape_hash[[present_name]] <- shapes_list
  area_hash[[present_name]] <- area
}

###For right now I'm separating these out. ID's the same but feels
###like it will be more readable/understandable to reference the different
### hashes
shape_hash <- new.env(hash = TRUE)
area_hash <- new.env(hash = TRUE)

walk(present_vecs, parse_presents)

present_names <- ls(envir = area_hash)
num_presents <- length(present_names)

## *****************************************************************************
## # Parsing Trees                                                  ----
## *****************************************************************************
cur_tree <- tree_vecs[1]

parse_trees <- function(cur_tree, idx) {
  tree_name <- paste0("T",idx)
  
  ###Split into vector
  tree_split <- str_split_1(cur_tree, " ")
  
  ###Separate out present numbers from grid numbers
  grid_part <- substr(tree_split[1], 1, nchar(tree_split[1]) - 1)
  present_part <- tree_split[-1]
  
  ###Parse Grid
  num_row <- str_split_i(grid_part, "x", 1) |> as.numeric()
  num_col <- str_split_i(grid_part, "x", 2) |> as.numeric()
  
  tree_grid <- matrix(0, nrow = num_row, ncol = num_col)
  tree_area <- num_row * num_col
  
  tree_list <- list(grid = tree_grid,
                    area = tree_area)
  
  presents <- c()
  
  ###Parse Present Counts
  for (i in seq_along(present_names)) {
    tree_list[[present_names[i]]] <- present_part[i] |> as.numeric()
    
    if (tree_list[[present_names[i]]] > 0) {
      presents <- c(presents, present_names[i])
    }
  }
  
  tree_list[["presents"]] <- presents
  
  tree_hash[[tree_name]] <- tree_list
}

tree_hash <- new.env(hash = TRUE)
iwalk(tree_vecs, parse_trees)

tree_names <- ls(envir = tree_hash)

#### First lets check how many trees can fit all the presents under them just by area
### Present area function
area_calc <- function(present, tree_name) {
  area <- area_hash[[present]]
  amount <- tree_hash[[tree_name]][[present]]
  
  return(area * amount)
}

### Tree Area Calc
tree_area_calc <- function(tree_name) {
  tree_presents <- tree_hash[[tree_name]]$presents
  
  total_present_area <- sum(sapply(tree_presents, area_calc, tree_name = tree_name))
  
  if (total_present_area > tree_hash[[tree_name]]$area) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

will_it_fit <- sapply(tree_names, tree_area_calc)

print(paste0("Part 1 Answer: ", sum(will_it_fit)))

