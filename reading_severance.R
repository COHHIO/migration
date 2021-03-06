# read severance file from WS

library(data.table)
library(here)
library(tidyverse)
library(lubridate)


# Get all csv files into dfs ----------------------------------------------

files <- list.files(path = "random_data/final_severance/", pattern = ".csv")

make_dfs <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("random_data/final_severance/", file)), 
                col_types = cols())
  x <- data.frame(x)
  assign(str_remove(file, ".csv"), x, envir = .GlobalEnv)
}

data.frame(file = files) %>%
  purrr::pwalk(make_dfs)

# Create Crosswalk --------------------------------------------------------
# get tree moves
tree_moves <- read_csv(here("random_data/tree_moves.csv"))

# from Clarity (may need updating)
live_clarity <- read_csv(here("data_from_Clarity/agencies_programs_in_Clarity.csv")) %>%
  select(Clarity_AgencyID:Clarity_ProgramName)

# from ServicePoint (may need updating)
in_provider_group <- read_csv(here("random_data/projects_in_sp_provider_group_final.csv"))

all_projects_from_sp <- sp_provider %>%
  filter(active == TRUE) %>%
  left_join(tree_moves, by = c("provider_id" = "Legacy_ProgramID")) %>%
  mutate(
    name = case_when(
      provider_id == 85 ~ "Columbiana - Family Recovery Center - Fleming House - TH",
      provider_id == 416 ~ "zzScioto - Counseling Center - Transitional Living - TH", 
      TRUE ~ name
    ),
    hud_organization_id = 
      case_when(
        !is.na(correct_Legacy_AgencyID) ~ correct_Legacy_AgencyID,
        TRUE ~ hud_organization_id)
    ) %>%
  select("Legacy_ProgramID" = provider_id, 
         "Legacy_ProgramName" = name, 
         "Legacy_AgencyID" = hud_organization_id) %>%
  left_join(
    sp_provider %>%
      select(
        "Legacy_AgencyID" = provider_id,
        "Legacy_AgencyName" = name
      ),
    by = "Legacy_AgencyID"
  ) %>%
  mutate(
    Legacy_AgencyName = case_when(
      Legacy_AgencyID == 2999 ~ "COORDINATED ENTRY - BoSCoC (OH-507)",
      Legacy_AgencyID == 3000 ~ "COORDINATED ENTRY - MCHCOC (OH-504)",
      TRUE ~ Legacy_AgencyName
    )
  )

data_coming_from_sp <- all_projects_from_sp %>%
  right_join(in_provider_group, by = c("Legacy_ProgramID", "Legacy_ProgramName")) %>%
  select(starts_with("Legacy"))

all_projects <- all_projects_from_sp %>%
  inner_join(live_clarity, by = c("Legacy_ProgramName" = "Clarity_ProgramName"))

clarity_projects_orgs <- data_coming_from_sp %>%
  left_join(live_clarity, by = c("Legacy_ProgramName" = "Clarity_ProgramName")) %>%
  mutate(
    Clarity_ProgramName = Legacy_ProgramName,
    Legacy_AgencyName = case_when(
      Legacy_AgencyID == 2999 ~
        "COORDINATED ENTRY - BoSCoC (OH-507)",
      Legacy_AgencyID == 3000 ~ "COORDINATED ENTRY - MCHCOC (OH-504)",
      TRUE ~ Legacy_AgencyName
    )
  ) %>%  
  relocate(Legacy_AgencyID:Legacy_AgencyName, .before = Legacy_ProgramID)

project_cohort <- clarity_projects_orgs$Legacy_ProgramID 

cat(clarity_projects_orgs %>%
  filter(is.na(Clarity_ProgramID)) %>% nrow() == 0) # YOU WANT TRUE

# Getting Client Cohort ---------------------------------------------------

client_cohort <- sp_entry_exit %>%
  filter(active == TRUE &
           !client_id %in% c(5, 4216) &
           provider_id %in% c(project_cohort) &
           (ymd_hms(exit_date) >= ymd("20140601") |
              is.na(exit_date))) %>%
  anti_join(sp_client %>%
               filter(active == FALSE) %>%
               select(client_id), by = "client_id") %>%
  pull(client_id) %>% unique()

other_funding_source_crosswalk <- tibble(
  ReferenceNo = c(1:19),
  OtherFundingSource = c(
  "Bezos Day One",
  "CDBG",
  "church",
  "community",
  "ODH",
  "ODSA",
  "OHFA",
  "Ohio Department of Health- Youth Initiative",
  "OSDA",
  "Pandemic Emergency Fund",
  "TANF",
  "Unknown",
  "CSBG",
  "Local",
  "OHFA ERA",
  "OVW Transitional Housing",
  "United Way",
  "COVID-19 Deconcentration",
  "Risk Mitigation")
)

# for testing -------------------------------------------------------------

enrollments_we_gave_them <-
  read_csv("data_to_Clarity/final_csv/Enrollment.csv") %>%
  pull(EnrollmentID)

clients_we_gave_them <-
  read_csv("data_to_Clarity/final_csv/Client.csv") %>%
  pull(PersonalID)


