# read severance file from WS

library(data.table)
library(here)
library(tidyverse)
library(lubridate)


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

id_cross <- read_csv(here("id_crosswalk.csv"))

sp_projects_orgs <- sp_provider %>%
  filter(active == TRUE) %>%
  select("SP_ProjectID" = provider_id, 
         "SP_ProjectName" = name, 
         "SP_AgencyID" = hud_organization_id) %>%
  left_join(sp_provider %>% select("SP_AgencyID" = provider_id, "AgencyName" = name),
            by = "SP_AgencyID")

clarity_projects_orgs <- sp_projects_orgs %>%
  left_join(id_cross, by = c("SP_ProjectID" = "Legacy_ProgramID")) %>%
  filter(!is.na(Clarity_program_ID)) %>%
  select(
    SP_ProjectID,
    SP_ProjectName,
    SP_AgencyID,
    "SP_AgencyName" = AgencyName,
    "Clarity_ProjectID" = Clarity_program_ID,
    "Clarity_ProjectName" = Clarity_program_name,
    "Clarity_AgencyID" = Clarity_Agency_ID,
    "Clarity_AgencyName" = Clarity_Agency_name
  )
