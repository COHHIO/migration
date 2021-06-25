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

client_cohort <- sp_entry_exit %>%
  filter(active == TRUE &
           !client_id %in% c(5, 4216) &
           !provider_id %in% c(1695, 1680) &
           (ymd_hms(exit_date) >= ymd("20140601") |
              is.na(exit_date))) %>%
  pull(client_id) %>% unique()

projects_orgs <- sp_provider %>%
  filter(active == TRUE) %>%
  select("ProjectID" = provider_id, 
         "ProjectName" = name, 
         "AgencyID" = hud_organization_id) %>%
  left_join(sp_provider %>% select("AgencyID" = provider_id, "AgencyName" = name),
            by = "AgencyID")
