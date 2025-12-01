# Load Libraries ----------------------------------------------------------
library(R6)

# Load Data ---------------------------------------------------------------
in_data <- readLines("advent2025/day1/data/input.txt", warn=FALSE)

# Part 1 ------------------------------------------------------------

### Define Object
dialClass <- R6Class("dialClass",
 private = list(
   start_position = NULL,
   current_position = NULL,
   zero_count = NULL
 ),
 public = list(
   initialize = function(startPos) {
     private$start_position <- startPos
     private$current_position <- startPos
     private$zero_count <- 0
   },
   
   get_current_position = function() {
     private$current_position
   },
   
   get_zero_count = function() {
     private$zero_count
   },
   
   turn_dial = function(rotation) {
     direction <- substr(rotation,1,1)
     distance <- as.numeric(substr(rotation,2,nchar(rotation)))
     
     if (direction == "L") {
       new_position <- (private$current_position - distance) %% 100
     } else if (direction == "R") {
       new_position <- (private$current_position + distance) %% 100
     } else {
       print("Undefined Direction")
     }
     
     private$current_position <- new_position
   },
   
   process_turns = function(rotations) {
     for (rotation in rotations) {
       self$turn_dial(rotation)
       
       if (private$current_position == 0) {
         private$zero_count <- private$zero_count + 1
       }
     }
   }
 )
)

### Initialize Dial
dial <- dialClass$new(50)

dial$process_turns(in_data)

print(paste0("Part 1 Answer: ", dial$get_zero_count()))


# Part 2 ------------------------------------------------------------------
### Define Object
dialClass2 <- R6Class("dialClass2",
                     private = list(
                       start_position = NULL,
                       current_position = NULL,
                       zero_count = NULL
                     ),
                     public = list(
                       initialize = function(startPos) {
                         private$start_position <- startPos
                         private$current_position <- startPos
                         private$zero_count <- 0
                       },
                       
                       get_current_position = function() {
                         private$current_position
                       },
                       
                       get_zero_count = function() {
                         private$zero_count
                       },
                       
                       turn_dial = function(rotation) {
                         direction <- substr(rotation,1,1)
                         distance <- as.numeric(substr(rotation,2,nchar(rotation)))
                         
                         if (direction == "L") {
                           ###Convert to R (mod 100 in case position is 0)
                           position_norm <- (100 - private$current_position) %% 100
                           
                           new_position <- (private$current_position - distance) %% 100
                         } else if (direction == "R") {
                           position_norm <- private$current_position
                           
                           new_position <- (private$current_position + distance) %% 100
                         } else {
                           print("Undefined Direction")
                         }
                         
                         zeros <- floor((position_norm + distance) / 100)
                        
                         private$zero_count <-  private$zero_count + zeros
                         
                         private$current_position <- new_position
                       },
                       
                       process_turns = function(rotations) {
                         for (rotation in rotations) {
                           self$turn_dial(rotation)
                         }
                       }
                     )
)

### Initialize Dial
dial <- dialClass2$new(50)

dial$process_turns(in_data)

print(paste0("Part 2 Answer: ", dial$get_zero_count()))
