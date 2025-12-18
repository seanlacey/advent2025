## *****************************************************************************
## Day 11 Part 2                                                   ----
##
## I split my solution in 2 since part 2, while ver similar to part 1, requires
## a different starting point and some updated count logic
##
## Additional note: This is not fully my work. I did part 1 myself but got stuck
## on part 2 and heavily used reddit and AI to help figure out the problems. Specifically
## on memoisation, there were small optimizations to my initial attempt but that was
## by far the largest help. 
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

in_data <- readLines(paste0("advent2025/day11/data/", data_name, ".txt"), warn = FALSE)

### Server Rack Object
ServerRack <- R6Class(
  classname = "ServerRack",
  public = list(
    root = NULL,
    input_hash = NULL,
    tree_memo = NULL,
    cache_hits = NULL,
    cache_misses = NULL,
    
    initialize = function(in_data) {
      ###First let's parse the connections into a hash. 
      ###This will make building the tree faster
      self$input_hash <- new.env(hash = TRUE)
      
      walk(in_data, self$parse_input)
      
      ###Initialize memoisation hash
      self$tree_memo <- new.env(hash = TRUE)
      
      ###Memoisation trackers. I'm seeing the difference memoisation makes
      self$cache_hits <- 0
      self$cache_misses <- 0
    },
    parse_input = function(vec) {
      vec_split <- trimws(str_split_1(vec, ":"))
      parent <- vec_split[1]
      children <- str_split_1(vec_split[2], " ")
      
      self$input_hash[[parent]] <- children
    },
    build_tree = function(id = "svr", parent = "none", visited = c(), fft = FALSE, dac = FALSE) {
      ###If we've visited this id before in this path, back up)
      if (id %in% visited) {
        return(0)
      }
      
      memo_key <- paste(id, fft, dac, sep = "_")
      
      if (exists(memo_key, envir = self$tree_memo)) {
        self$cache_hits <- self$cache_hits + 1
        return(self$tree_memo[[memo_key]])
      }
      
      self$cache_misses <- self$cache_misses + 1
      
      children <- self$input_hash[[id]]
      
      out_count <- 0 
      
      ### Check if child is fft or dac
      if (id == "fft") {
        fft <- TRUE
      }
      
      if (id == "dac") {
        dac <- TRUE
      }
      
      for (child in children) {
        ###Handling for end of the line. I beleive that all of the nodes with "out" children
        ###have only the singular child but just in case.
        if (child == "out") {
          if (fft && dac) {
            out_count <- out_count + 1
          }
          next
        }
        
        out_count <- out_count + self$build_tree(id = child, parent = id, visited = c(visited, id), fft, dac)
      }
      
      ###Save to memo
      self$tree_memo[[memo_key]] <- out_count
      
      return(out_count)
    }
  )
)

server <- ServerRack$new(in_data)
part2answer <- server$build_tree()

print(paste0("Part 2 Answer: ", part2answer))
print(paste0("Cache Hits: ", server$cache_hits))
print(paste0("Cache Misses: ", server$cache_misses))
Sys.time() - s
