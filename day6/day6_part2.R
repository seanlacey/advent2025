# Libraries ---------------------------------------------------------------
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

# Load Data ---------------------------------------------------------------

### Get number of columns
data_name <- "input"

in_data <- readLines(paste0("advent2025/day6/data/", data_name, ".txt"), warn = FALSE)

# Part 2 ------------------------------------------------------------------
s <- Sys.time()

###Extract Numbers into one column per digit
for (i in 1:(length(in_data)-1)) {
  if (i == 1) {
    split_data <- str_split_1(in_data[i], "")
  } else {
    split_data <- rbind(split_data, str_split_1(in_data[i], ""))
  }
}

###Transpose
t_split_data <- as_tibble(t(split_data))

### Combine Numbers
t_split_data <- t_split_data |>
  unite(number_char, remove = FALSE, na.rm = TRUE, sep = "") |>
  mutate(number = as.numeric(number_char)) |> 
  select(number)

###Create Group ID to help with pivot
groupn <- 1
coln <- 1

for (i in 1:nrow(t_split_data)) {
  t_split_data[i,"groupid"] <- groupn
  t_split_data[i,"colname"] <- paste0("COL",coln)
  
  coln <- coln + 1
  
  if (is.na(t_split_data[i, "number"])) {
    groupn <- groupn + 1
    coln <- 1
  }
}

###Remove empty rows
t_split_data <- t_split_data |>
  filter(!is.na(number)) |>
  group_by(groupid) |>
  pivot_wider(names_from = colname, values_from = number) |>
  ungroup() |>
  select(-groupid)

operators <- str_split(trimws(in_data[length(in_data)]), "\\s+")[[1]]

t_split_data$operator <- operators

eq_eval <- function(row_in) {
  number_vector <- as.vector(row_in)
  number_vector <- number_vector[!is.na(number_vector)]
  
  operator <- as.character(number_vector[length(number_vector)])
  
  equation <- ""
  
  for (i in 1:(length(number_vector) - 1)) {
    cur_col <- as.character(number_vector[i])
    
    if (equation == "") {
      equation <- cur_col
    } else {
      equation <- paste0(equation, " ", operator, " ", cur_col)
    }
  }
  
  answer <- eval(parse(text = equation))
  
  return(answer)
}

answer_vector <- vector(mode = "numeric", length = nrow(t_split_data))

for (i in 1:nrow(t_split_data)) {
  answer_vector[i] <- eq_eval(t_split_data[i,])
}

print(paste0("Part 2 Answer: ", sum(answer_vector)))

Sys.time() - s
