library(readxl)
library(tidyverse)
library(here)

current_cross <- read_xlsx(here("id_crosswalk.xlsx"), sheet = 1)

source(here("reading_severance.R"))

sp_projects_orgs <- sp_projects_orgs %>%
  rename("Legacy_OrganizationName" = SP_AgencyName,
         "Legacy_OrganizationID" = SP_AgencyID,
         "Legacy_ProgramID" = SP_ProjectID,
         "Legacy_ProgramName" = SP_ProjectName)

# Clarity Agencies to Add to Crosswalk ------------------------------------

clarity_agencies_not_on_crosswalk <- read_xlsx(here("id_crosswalk.xlsx"), 
                                               sheet = 2) %>%
  rename("Clarity_AgencyID" = AgencyID,
         "Clarity_AgencyName" = AgencyName,
         "Clarity_ProgramID" = ProjectID,
         "Clarity_ProgramName" = ProjectName)

empty_agencies <- clarity_agencies_not_on_crosswalk %>%
  filter(is.na(Clarity_ProgramID))

get_sp_equivalents_empty <- empty_agencies %>%
  left_join(sp_projects_orgs %>%
              select(Legacy_ProgramID, Legacy_ProgramName), 
            by = c("Clarity_AgencyName" = "Legacy_ProgramName")) %>%
  mutate(Legacy_OrganizationName = Clarity_AgencyName,
         Legacy_OrganizationID = Legacy_ProgramID,
         Legacy_ProgramName = "none",
         Legacy_ProgramID = "none") %>%
  select(
    Legacy_OrganizationName, 
    Legacy_OrganizationID,   
    Legacy_ProgramName,
    Legacy_ProgramID,        
    Clarity_AgencyID,        
    Clarity_AgencyName,
    Clarity_ProgramID,
    Clarity_ProgramName
  )

real_agencies <- clarity_agencies_not_on_crosswalk %>%
  filter(!is.na(Clarity_ProgramID))

get_sp_equivalents_real <- real_agencies %>%
  left_join(sp_projects_orgs, 
            by = c("Clarity_ProgramName" = "Legacy_ProgramName")) %>%
  mutate(Legacy_ProgramName = Clarity_ProgramName) %>%
  select(
    Legacy_OrganizationName, 
    Legacy_OrganizationID,   
    Legacy_ProgramName,
    Legacy_ProgramID,        
    Clarity_AgencyID,        
    Clarity_AgencyName,
    Clarity_ProgramID,
    Clarity_ProgramName
  )

# Projects to Add to the Crosswalk ----------------------------------------

clarity_projects_not_on_crosswalk <- read_xlsx(here("id_crosswalk.xlsx"), 
                                               sheet = 4) %>%
  rename("Clarity_AgencyID" = AgencyID,
         "Clarity_AgencyName" = AgencyName,
         "Clarity_ProgramID" = ProjectID,
         "Clarity_ProgramName" = ProjectName)

get_sp_equivalents <- clarity_projects_not_on_crosswalk %>%
  left_join(sp_projects_orgs, 
            by = c("Clarity_ProgramName" = "Legacy_ProgramName")) %>%
  mutate(Legacy_ProgramName = Clarity_ProgramName) %>%
  select(
    Legacy_OrganizationName, 
    Legacy_OrganizationID,   
    Legacy_ProgramName,
    Legacy_ProgramID,        
    Clarity_AgencyID,        
    Clarity_AgencyName,
    Clarity_ProgramID,
    Clarity_ProgramName
  )

add_to_crosswalk <- rbind(
  get_sp_equivalents,
  get_sp_equivalents_empty,
  get_sp_equivalents_real
) %>% unique() 

new_crosswalk <- rbind(current_cross, add_to_crosswalk)


# Taking AP move into account ---------------------------------------------

affected_aps <- read_csv(here("random_data/aps_move.csv")) %>%
  rename("Clarity_AgencyID" = 1,
         "Clarity_AgencyName" = 2,
         "Clarity_ProgramID" = 3,
         "Clarity_ProgramName" = 4,
         "New_Clarity_AgencyID" = 5)

adjusted_new_crosswalk <- new_crosswalk %>%
  left_join(
    affected_aps,
    by = c(
      "Clarity_AgencyID",
      "Clarity_AgencyName",
      "Clarity_ProgramID",
      "Clarity_ProgramName"
    )
  ) %>%
  mutate(
    Clarity_AgencyName = case_when(
      New_Clarity_AgencyID == 299 ~ "COORDINATED ENTRY - BoSCoC (OH-507)",
      New_Clarity_AgencyID == 300 ~ "COORDINATED ENTRY - MCHCOC (OH-504)",
      TRUE ~ Clarity_AgencyName
    ),
    Clarity_AgencyID = if_else(is.na(New_Clarity_AgencyID),
                               Clarity_AgencyID,
                               New_Clarity_AgencyID)
  ) %>%
  select(-New_Clarity_AgencyID)

write_csv(adjusted_new_crosswalk, "id_crosswalk.csv")
