# Libraries ---------------------------------------------------------------
library(stringr)
library(dplyr)

# Load Data ---------------------------------------------------------------
data_name <- "input"

in_data <- readLines(paste0("advent2025/day7/data/", data_name, ".txt"), warn = FALSE)

rows <- do.call(rbind, strsplit(in_data, ""))

beam_row <- map_vec(rows[1,], \(x) ifelse(x == ".", 0, 1))

total_split <- 0

for (i in 2:nrow(rows)) {
  cur_row <- map_vec(rows[i,], \(x) ifelse(x == ".", 0, -1)) * beam_row
  
  total_split <- total_split + sum(cur_row != 0)
  
  offset_L <- c(cur_row[2:length(cur_row)], 0) * -1
  
  offset_R <- c(0, cur_row[1:(length(cur_row) - 1)]) * -1
  
  wiggle_row <- offset_L + offset_R + cur_row
  
  beam_row <- beam_row + wiggle_row
}

print(paste0("Part A Answer: ", total_split))

print(paste0("Part B Answer: ", sum(beam_row)))
