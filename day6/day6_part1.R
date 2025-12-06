# Libraries ---------------------------------------------------------------
library(stringr)
library(purrr)
library(dplyr)

# Load Data ---------------------------------------------------------------

### Get number of columns
data_name <- "input"

in_data <- readLines(paste0("advent2025/day6/data/", data_name, ".txt"), warn = FALSE)

# Part 1 ------------------------------------------------------------------
s <- Sys.time()

### count number of "words" to get row length
row_length <- str_count(trimws(in_data[1]), "\\S+")

my_data <- tibble(id = 1:row_length)
col_names <- vector(mode = "character", length = length(in_data) - 1)

for (i in 1:length(in_data)) {
  if (i == length(in_data)) {
    my_data["operator"] <- str_split(trimws(in_data[i]), "\\s+")[[1]]
  } else {
    col_names[i] <- paste0("COL",i)
    
    my_data[col_names[i]] <- as.numeric(str_split(trimws(in_data[i]), "\\s+")[[1]])
  }
}

eq_eval <- function(COL1, COL2, COL3, COL4, operator, ...) {
  Reduce(operator, c(COL1, COL2, COL3, COL4))
}

answers <- unlist(pmap(my_data, eq_eval))

print(paste0("Part 1 Answer: ", sum(answers)))

Sys.time() - s
