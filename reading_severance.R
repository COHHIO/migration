# read severance file from WS

library(data.table)
library(here)
library(tidyverse)


# Get all csv files into dfs ----------------------------------------------

files <- list.files(path = "random_data/corrected_severance/", pattern = ".csv")

make_dfs <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("random_data/corrected_severance/", file)), 
                col_types = cols())
  x <- data.frame(x)
  assign(str_remove(file, ".csv"), x, envir = .GlobalEnv)
}

data.frame(file = files) %>%
  purrr::pwalk(make_dfs)


