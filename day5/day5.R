# Libraries ---------------------------------------------------------------
library(bit64)
library(R6)
library(tibble)
library(dplyr)
library(stringr)
library(collections)

# Load Data ---------------------------------------------------------------
data_name <- "input"

in_data <- readLines(paste0("advent2025/day5/data/", data_name, ".txt"), warn = FALSE)

intervals_raw <- in_data[1:which(in_data == "") - 1]
ingredients <- in_data[(which(in_data == "") + 1):length(in_data)]

### Split intervals into two pieces
intervals_df <- data.frame(raw = intervals_raw) |>
  mutate(start = as.integer64(str_split_i(raw, "-", 1)),
         end = as.integer64(str_split_i(raw, "-", 2))) |>
  arrange(start,end) |>
  mutate(id <- row_number())

# Part 1 ------------------------------------------------------------------
s <- Sys.time()

##Function I found on stack overflow to give nodes unique names
nodeNamer <- function() {
  i <- 0
  function(node) sprintf("N%g", (i <<- i + 1))
}

name_node <- nodeNamer()

### Create Node Logic for our Tree
Node <- R6Class(
  classname = "Node",
  public = list(
    start = NULL,
    end = NULL,
    left = NULL,
    right = NULL,
    id = NULL,
    
    initialize = function(start = NULL, end = NULL, left = NULL, right = NULL) {
      self$start <- start
      self$end <- end
      self$left <- left
      self$right <- right
      self$id <- name_node()
    }
  )
)

### Interval Tree
IntervalTree <- R6Class(
  classname = "IntervalTree",
  public = list(
    root = NULL,
    fresh_counts = dict(),
    insert = function(start, end) {
      if (is.null(self$root)) {
        self$root <- Node$new(start = start, end = end)
        
        self$fresh_counts$set(self$root$id, (end - start + 1))
      } else {
        self$insert_child(start, end, self$root)
      }
    },
    insert_child = function(start, end, cur_node) {
      if (start > cur_node$end) {
        ### If start is after the end of the current node, move right
        if (is.null(cur_node$right)) {
          ### If there is no right node, create it
          cur_node$right <- Node$new(start = start, end = end)
          
          self$fresh_counts$set(cur_node$right$id, (end - start + 1))
        } else {
          ### Otherwise, move to next node
          self$insert_child(start, end, cur_node$right)
        }
      } else if (end < cur_node$start) {
        ### If end is before the start of the current node, move left
        if (is.null(cur_node$left)) {
          ### If there is no left node, create it
          cur_node$left <- Node$new(start = start, end = end)
          
          self$fresh_counts$set(cur_node$left$id, (end - start + 1))
        } else {
          ### Otherwise move to next node)
          self$insert_child(start, end, cur_node$left)
        }
      } else {
        ### Handle ranges that overlap
        if (start < cur_node$start || end > cur_node$end) {
          ###Added the above condition check to  control update of the counts
          if (start < cur_node$start) {
            ####If start is less than the current start, update current start
            cur_node$start <- start
          }
          
          if (end > cur_node$end) {
            ###If end is greater than current end, update current end.
            ###Note: I'm not using else here in the case we have a range that
            ###subsumes the previous
            cur_node$end <- end
          }
          
          self$fresh_counts$set(cur_node$id, (cur_node$end - cur_node$start + 1))
        }
      }
    },
    point_search = function(val, cur_node = self$root) {
      if (val < cur_node$start) {
        ### If val is less than the start value, move left
        ### If left doesn't exist, return FALSE
        if (is.null(cur_node$left)) {
          return(FALSE)
        } else {
          self$point_search(val, cur_node = cur_node$left)
        }
      } else if (val > cur_node$end) {
        ### If val is greater than end value, move right
        ### If right doesn't exist, return FALSE
        if (is.null(cur_node$right)) {
          return(FALSE)
        } else {
          self$point_search(val, cur_node = cur_node$right)
        }
      } else {
        ### If none of the above, it's in the range. Return TRUE
        return(TRUE)
      }
    },
    fresh_count_total = function() {
      total_count <- 0
      
      for (val in self$fresh_counts$values()) {
        total_count <- total_count + val
      }
      
      return(total_count)
    }
  )
)

### Create vector that starts at median and proceeds middle out
branch_path <- c(floor(median(intervals_df$id)))

branch_path <- c(branch_path, (branch_path[1] - 1):1, (branch_path[1] + 1):nrow(intervals_df))

interval_tree <- IntervalTree$new()

for (branch in branch_path) {
  interval_tree$insert(intervals_df[branch, "start"], intervals_df[branch, "end"])
}

ingredient_fresh <- vector(mode = "logical", length = length(ingredients))

for (i in 1:length(ingredients)) {
  ingredient_fresh[i] <- interval_tree$point_search(val = as.integer64(ingredients[i]))
}

print(paste0("Part 1 Answer: ", sum(ingredient_fresh)))

# Part 2 ------------------------------------------------------------------
print(paste0("Part 2 Answer: ", interval_tree$fresh_count_total()))

Sys.time() - s #1.308248 secs
