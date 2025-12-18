# Libraries ---------------------------------------------------------------
library(stringr)
library(dplyr)
library(collections)
library(R6)
library(purrr)

# Load Data ---------------------------------------------------------------
s <- Sys.time()
data_name <- "example"

in_data <- readLines(paste0("advent2025/day11/data/", data_name, ".txt"), warn = FALSE)

### Device Object (will function as nodes)
Device <- R6Class(
  classname = "Device",
  public = list(
    id = NULL,
    parent = NULL,
    children = NULL,
    
    initialize = function(id, parent = NULL, children) {
      self$id <- id
      self$parent <- parent
      self$children <- vector(mode = "list", length = length(children))
      names(self$children) <- children
    }
  )
)

### Server Rack Object
ServerRack <- R6Class(
  classname = "ServerRack",
  public = list(
    root = NULL,
    input_hash = NULL,
    
    initialize = function(in_data) {
      ###First let's parse the connections into a hash. 
      ###This will make building the tree faster
      self$input_hash <- new.env(hash = TRUE)
      
      walk(in_data, self$parse_input)
    },
    parse_input = function(vec) {
      vec_split <- trimws(str_split_1(vec, ":"))
      parent <- vec_split[1]
      children <- str_split_1(vec_split[2], " ")
      
      self$input_hash[[parent]] <- children
    },
    build_tree = function(id = "you", parent = "none", cur_node = NULL) {
      children <- self$input_hash[[id]]
      
      out_count <- 0 
      
      if (id == "you") {
        ###Placing this as a separate level to catch all the "you" that might
        ###occur. Theoretically this shouldn't be an issue but just in case
        ###I want to prevent circular references
        if (is.null(self$root)) {
          self$root <- Device$new(id, NULL, children)
        }
      } else {
        if (is.null(cur_node$children[[id]])) {
          cur_node$children[[id]] <- Device$new(id, cur_node$id, children)
        }
      }
      
      for (child in children) {
        print(child)
        print(parent)
        ###Skip parent node connection
        if (child == parent) {
          next
        }
        
        ###Handling for end of the line
        if (child == "out") {
          return(1)
        }
        
        if (id == "you") {
          out_count <- out_count + self$build_tree(id = child, parent = id, cur_node = self$root)
        } else {
          out_count <- out_count + self$build_tree(id = child, parent = id, cur_node = cur_node$children[[id]])
        }
        
      }
      
      return(out_count)
    }
  )
)

server <- ServerRack$new(in_data)
part1answer <- server$build_tree()

print(paste0("Part 1 Answer: ", part1answer))
Sys.time() - s
