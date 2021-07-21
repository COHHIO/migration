library(readxl)
library(tidyverse)
library(here)


# Get lists ---------------------------------------------------------------

source(here("reading_severance.R"))

live_clarity <- read_csv(here("data_from_Clarity/agencies_programs_in_Clarity.csv"))

in_provider_group <- read_csv(here("random_data/projects_in_sp_provider_group_final.csv"))

bf_likes <- read_csv(here("random_data/live_BF_likes_projects.csv"))

bf_does_not_like <- read_csv(here("random_data/live_BF_does_not_like_projects.csv"))


# Verify what they like ---------------------------------------------------

non_matching_org_names <- bf_likes %>%
  filter(Legacy_organization_name != Clarity_Agency_name) %>%
  select(Legacy_organization_ID,
         Legacy_organization_name,
         Clarity_Agency_ID,
         Clarity_Agency_name)

non_matching_project_names <- bf_likes %>%
  filter(Legacy_programName != Clarity_program_name) %>%
  select(Legacy_ProgramID,
         Legacy_programName,
         Clarity_program_ID,
         Clarity_program_name)


# Fix what they don't like ------------------------------------------------

guessing <- bf_does_not_like %>%
  select(Legacy_programName) %>%
  left_join(live_clarity, by = c("Legacy_programName" = "ProjectName"))


# Clarity Agencies to Add to Crosswalk ------------------------------------

# current_cross <- read_xlsx(here("id_crosswalk.xlsx"), sheet = 1)

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


write_csv(new_crosswalk, "id_crosswalk.csv")


# Just some checking ------------------------------------------------------

final_csv <- read_csv(here("random_data/final_csv_project_list.csv")) %>%
  mutate(Legacy_ProgramID = as.character(Legacy_ProgramID)) %>%
  select(-OperatingEnd, -ProjectType)

crosswalk_with_benefits <- adjusted_new_crosswalk %>%
  left_join(final_csv, by = c("Legacy_ProgramID", "Legacy_ProgramName")) %>%
  mutate(GettingData = if_else(is.na(OperatingStart), "No", "Yes")) %>%
  select(-OperatingStart)

write_csv(crosswalk_with_benefits, "id_crosswalk_w_benefits.csv")






