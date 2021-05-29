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

# can't find where OrganizationID is in the severance file (they have parent 
# provider) so pulling from RMisc2 like we do in Funding_Sources.R

projects_orgs <- read_xlsx(
  here("data_to_Clarity/RMisc2.xlsx"),
  sheet = 3,
  col_types = c("numeric", replicate(16, "text"))
) %>%
  filter(!is.na(OrganizationName) &
           OrganizationName != "Coalition on Homelessness and Housing in Ohio(1)") %>%
  mutate(
    AgencyID = str_extract(OrganizationName, "\\(?[0-9]+\\)?"),
    AgencyID = str_remove(AgencyID, "[(]"),
    AgencyID = str_remove(AgencyID, "[)]"),
    AgencyID = as.numeric(AgencyID),
    AgencyName = str_remove(OrganizationName, "\\(.*\\)")
  ) %>%
  select(ProjectID, ProjectName, AgencyID, AgencyName)

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
  mutate(Title = "imported from Goals") %>%
  select(-ProjectID)

# Client Notes ------------------------------------------------------------

# not currently in severance file, but this should be fixed soon. til then,
# getting it from ART

raw_notes_client <- read_xlsx("random_data/Notes.xlsx", sheet = 2) %>%
  select(-NoteID, -UserID) %>% rename("ProjectID" = Program) %>%
  mutate(Date = ymd(as.Date(Date, origin = "1899-12-30")),
         DateCreated = ymd(as.Date(DateCreated, origin = "1899-12-30")),
         DateUpdated = ymd(as.Date(DateUpdated, origin = "1899-12-30")))

notes_client <- raw_notes_client %>%
  mutate(Title = "imported from Clients",
         ProjectID = str_extract(ProjectID, "[0-9]+"),
         ProjectID = as.numeric(ProjectID)) %>%
  left_join(projects_orgs %>%
              select(ProjectID, AgencyID), by = "ProjectID") %>%
  select(-ProjectID)

# Notes attached to Services ----------------------------------------------

service_notes <- sp_need_service %>% 
  filter(!is.na(service_note) & active == TRUE) %>% 
  mutate(Title = "imported from ServicePoint Service") %>%
  left_join(projects_orgs %>%
              select(ProjectID, AgencyID), by = c("provide_provider_id" = "ProjectID")) %>%
  select("PersonalID" = client_id,
         AgencyID,
         "ServicesID" = need_service_id,
         Title,
         "Date" = provide_start_date,
         "Note" = service_note,
         "DateCreated" = date_added,
         "DateUpdated" = date_updated)

# Notes attached to Needs -------------------------------------------------

need_notes <- sp_need %>% 
  filter(!is.na(note) & active == TRUE) %>% 
  mutate(Title = "imported from ServicePoint Need",
         note = paste("Note:", note, 
                      "\nStatus:", status, 
                      "\nOutcome:", outcome, 
                      "\nReason Unmet:", reason_unmet)) %>%
  left_join(projects_orgs %>%
              select(ProjectID, AgencyID), by = c("provider_id" = "ProjectID")) %>%
  select("PersonalID" = client_id, 
         AgencyID, 
         Title,
         "Date" = date_set,
         "Note" = note,
         "DateCreated" = date_added, 
         "DateUpdated" = date_updated)

# Exit Notes --------------------------------------------------------------

exit_notes <- sp_entry_exit %>%
  filter(!is.na(notes) & active == TRUE & ymd_hms(exit_date) >= ymd("20140601") &
           AgencyID %in% c()) %>%
  mutate(Title = paste("Exit note: Exited Project ID", 
                       provider_id, 
                       "on", 
                       format.Date(exit_date, "%m-%d-%Y"))) %>%
  left_join(projects_orgs %>% 
              select(ProjectID, AgencyID), by = c("provider_id" = "ProjectID")) %>%
  select("PersonalID" = client_id, 
         AgencyID,
         "EnrollmentID" = entry_exit_id, 
         Title,
         "Date" = exit_date, 
         "Note" = notes,
         "DateCreated" = date_added, 
         "DateUpdated" = date_updated)

# All together ------------------------------------------------------------
agencies <- read_csv("frozen/Agencies.csv") %>% pull(id)

All_Notes <- notes_client %>%
  full_join(notes_goals) %>%
  full_join(service_notes) %>%
  full_join(need_notes) %>%
  full_join(exit_notes) %>%
  filter(AgencyID %in% c(agencies)) %>%
  mutate(NoteID = row_number()) %>%
  relocate(NoteID, .before = PersonalID)

# WARNING: this excludes data from any agencies not already in Clarity. On your
# final run, edit line 140 so you're pulling from all agencies you want to come
# over.










