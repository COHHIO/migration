# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(here)
library(tidyverse)
library(readxl)
library(lubridate)

source(here("reading_severance.R"))

projects_orgs <- sp_provider %>%
  filter(active == TRUE) %>%
  select("ProjectID" = provider_id, 
         "ProjectName" = name, 
         "AgencyID" = hud_organization_id) %>%
  left_join(sp_provider %>% select("AgencyID" = provider_id, "AgencyName" = name),
            by = "AgencyID")

# Notes.xlsx comes from an ART report in public > Sys Admin > Clarity > Notes

# Notes attached to Goals -------------------------------------------------

notes_goals <- sp_goal_casenote %>%
  filter(active == TRUE) %>%
  select("PersonalID" = client_id,
         "DateCreated" = date_added,
         "DateUpdated" = date_updated,
         "Note" = note,
         "Date" = note_date,
         "ProjectID" = provider_id) %>%
  left_join(projects_orgs %>%
              select(ProjectID, AgencyID), by = "ProjectID") %>%
  mutate(Title = "imported from Goals",
         EnrollmentID = "",
         ServicesID = "") %>%
  select(-ProjectID)

# Client Notes ------------------------------------------------------------

notes_clients <- sp_client_note %>%
  filter(active == TRUE) %>%
  select("PersonalID" = client_id,
         "DateCreated" = date_added,
         "DateUpdated" = date_updated,
         "Note" = note,
         "Date" = note_date,
         provider_creating_id) %>%
  left_join(
    projects_orgs %>%
      select(ProjectID, AgencyID),
    by = c("provider_creating_id" = "ProjectID")
  ) %>%
  mutate(Title = "imported from Client",
         EnrollmentID = "",
         ServicesID = "") %>%
  select(-provider_creating_id)



# Notes attached to Services ----------------------------------------------

notes_services <- sp_need_service %>% 
  filter(!is.na(service_note) & active == TRUE) %>% 
  mutate(Title = "imported from ServicePoint Service",
         ProjectID = as.numeric(provide_provider_id),
         EnrollmentID = "") %>%
  left_join(projects_orgs %>%
              select(ProjectID, AgencyID), by = c("provide_provider_id" = "ProjectID")) %>%
  select("PersonalID" = client_id,
         AgencyID,
         EnrollmentID,
         "ServicesID" = need_service_id,
         Title,
         "Date" = provide_start_date,
         "Note" = service_note,
         "DateCreated" = date_added,
         "DateUpdated" = date_updated)

# Notes attached to Needs -------------------------------------------------

notes_needs <- sp_need %>% 
  filter(!is.na(note) & active == TRUE) %>% 
  mutate(Title = "imported from ServicePoint Need",
         note = paste("Note:", note, 
                      "\nStatus:", status, 
                      "\nOutcome:", outcome, 
                      "\nReason Unmet:", reason_unmet),
         ProjectID = as.numeric(provider_id),
         EnrollmentID = "",
         ServicesID = "") %>%
  left_join(projects_orgs %>%
              select(ProjectID, AgencyID), by = c("provider_id" = "ProjectID")) %>%
  select("PersonalID" = client_id, 
         AgencyID, 
         EnrollmentID,
         ServicesID,
         Title,
         "Date" = date_set,
         "Note" = note,
         "DateCreated" = date_added, 
         "DateUpdated" = date_updated)

# Exit Notes --------------------------------------------------------------

# agencies <- read_csv("frozen/Agencies.csv") %>% pull(id)

notes_exits <- sp_entry_exit %>%
  filter(!is.na(notes) & active == TRUE & ymd_hms(exit_date) >= ymd("20140601")) %>%
  mutate(Title = paste("Exit note: Exited Project ID", 
                       provider_id, 
                       "on", 
                       format.Date(exit_date, "%m-%d-%Y")),
         ProjectID = as.numeric(provider_id),
         ServicesID = "") %>%
  left_join(projects_orgs %>% 
              select(ProjectID, AgencyID), by = c("provider_id" = "ProjectID")) %>%
  # filter(AgencyID %in% c(agencies)) %>%
  select("PersonalID" = client_id, 
         AgencyID,
         ServicesID,
         "EnrollmentID" = entry_exit_id, # some are null
         Title,
         "Date" = exit_date, 
         "Note" = notes,
         "DateCreated" = date_added, 
         "DateUpdated" = date_updated)

# All together ------------------------------------------------------------

All_Notes <- rbind(
  notes_clients,
  notes_exits,
  notes_goals,
  notes_needs,
  notes_services
) %>%
  filter(PersonalID %in% c(client_cohort)) %>% # edit this for final run
  mutate(NoteID = row_number()) %>%
  select(
    NoteID,
    PersonalID,
    AgencyID,
    EnrollmentID,
    ServicesID,
    Title,
    Date,
    Note,
    DateCreated,
    DateUpdated
  ) %>%
  arrange(AgencyID)

# WARNING: this excludes data from any agencies not already in Clarity. On your
# final run, edit line 141 so you're pulling from all agencies you want to come
# over.

# Writing it out to csv ---------------------------------------------------

write_csv(All_Notes, here("data_to_Clarity/Notes_Custom.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(
      Date = format.Date(Date, "%Y-%m-%d"),
      DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
      DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T")
    )
  
  fwrite(x,
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Notes_Custom")








