# Libraries ---------------------------------------------------------------
library(stringr)
library(dplyr)
library(purrr)
library(collections)

# Load Data ---------------------------------------------------------------
s <- Sys.time()

data_name <- "input"

if (data_name == "example") {
  n_iter <- 10
} else if (data_name == "input") {
  n_iter <- 1000
}

in_data <- readLines(paste0("advent2025/day8/data/", data_name, ".txt"), warn = FALSE)

split_nums <- do.call(rbind, strsplit(in_data, ","))


delta_mat <- function(vec) {
  mat <- replicate(length(vec), vec)
  t_mat <- t(mat)
  
  mat - t_mat
}

xdelta <- delta_mat(as.numeric(split_nums[,1]))
ydelta <- delta_mat(as.numeric(split_nums[,2]))
zdelta <- delta_mat(as.numeric(split_nums[,3]))

d_square <- xdelta ** 2 + ydelta ** 2 + zdelta ** 2
d_square[lower.tri(d_square)] <- 0

num_row <- nrow(d_square)
num_col <- ncol(d_square)

rankd <- rank(d_square, ties.method = "min")
rankd <- rankd[which(rankd != 1)]

###Fill Queue, Priority will be based on the rank, with lowest going first
q <- priority_queue()

row_i <- 1
col_i <- 2

for (i in 1:length(rankd)) {
  row <- row_i
  col <- col_i

  q$push(c(row, col), priority = (rankd[i] * -1))
  
  ###Iterate Row Tracker
  row_i <- row_i + 1
  
  if (row_i == col_i) {
    col_i <- col_i + 1
    row_i <- 1
  }
}

circuit_dict <- dict()

###Initialize all the circuits
for (i in 1:length(in_data)) {
  circuit_dict$set(paste0("C",i),i)
}

###Function to get the circuit name that the point is in
circuit_name_get <- function(val) {
  unlist(circuit_dict$keys()[which(sapply(circuit_dict$values(), function(x) val %in% x))])
}

### Circuit Join Function
circuit_join <- function(points) {
  point1circuit <- circuit_name_get(points[1])
  point2circuit <- circuit_name_get(points[2])
  
  if (point1circuit != point2circuit) {
    ### Combine into min circuit
    circuit_dict$set(point1circuit, c(circuit_dict$get(point1circuit), 
                                      circuit_dict$get(point2circuit)))
    
    ### Remove max circuit
    circuit_dict$remove(point2circuit) 
    
    return(1)
  } else {
    return(0)
  }
}

for (i in 1:n_iter) {
  circuit_join(q$pop())
}

three_biggest <- sort(sapply(circuit_dict$values(), function(x) length(x)), decreasing = TRUE)[1:3]

print(paste0("Part 1 Answer: ", prod(three_biggest)))

Sys.time() - s

# Part 2 ------------------------------------------------------------------
s <- Sys.time()

###Reset Queue
q <- priority_queue()

row_i <- 1
col_i <- 2

for (i in 1:length(rankd)) {
  row <- row_i
  col <- col_i
  
  q$push(c(row, col), priority = (rankd[i] * -1))
  
  ###Iterate Row Tracker
  row_i <- row_i + 1
  
  if (row_i == col_i) {
    col_i <- col_i + 1
    row_i <- 1
  }
}

###Reset the dictionary
circuit_dict <- dict()

###Initialize all the circuits
for (i in 1:length(in_data)) {
  circuit_dict$set(paste0("C",i),i)
}
  
num_circuits <- circuit_dict$size()

while (num_circuits > 1) {
  current_points <- q$pop()
  circuit_join(current_points)
  
  num_circuits <- circuit_dict$size()
}

x1 <- split_nums[current_points[1], 1] |> as.numeric()
x2 <- split_nums[current_points[2], 1] |> as.numeric()

print(paste0("Part 2 Answer: ", x1 * x2))

Sys.time() - s
