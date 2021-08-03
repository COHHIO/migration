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

# Notes attached to Goals -------------------------------------------------

notes_goals <- sp_goal_casenote %>%
  filter(active == TRUE & client_id %in% c(client_cohort)) %>%
  select("PersonalID" = client_id,
         "DateCreated" = date_added,
         "DateUpdated" = date_updated,
         "Note" = note,
         "Date" = note_date,
         "Legacy_ProgramID" = provider_id) %>%
  left_join(all_projects, by = "Legacy_ProgramID") %>%
  mutate(Title = "imported from Goals",
         EnrollmentID = "",
         ServicesID = "",
         Date = format.Date(Date, "%Y-%m-%d"),
         Note = str_squish(gsub("\r?\n|\r", " ", Note))) %>%
  select(PersonalID,
         "AgencyID" = Clarity_AgencyID,
         Legacy_ProgramID,
         EnrollmentID,
         ServicesID,
         Title,
         Date,
         Note,
         DateCreated,
         DateUpdated)

# Client Notes ------------------------------------------------------------

notes_clients <- sp_client_note %>%
  filter(active == TRUE & client_id %in% c(client_cohort)) %>%
  select("PersonalID" = client_id,
         "DateCreated" = date_added,
         "DateUpdated" = date_updated,
         "Note" = note,
         "Date" = note_date,
         "Legacy_ProgramID" = provider_creating_id) %>%
  left_join(all_projects, by = "Legacy_ProgramID") %>%
  mutate(Title = "imported from Client",
         EnrollmentID = "",
         ServicesID = "",
         Date = format.Date(Date, "%Y-%m-%d"),
         Note = str_squish(gsub("\r?\n|\r", " ", Note))) %>%
  select(PersonalID,
         "AgencyID" = Clarity_AgencyID,
         Legacy_ProgramID,
         EnrollmentID,
         ServicesID,
         Title,
         Date,
         Note,
         DateCreated,
         DateUpdated)

# Notes attached to Services ----------------------------------------------

notes_services <- sp_need_service %>% 
  filter(!is.na(service_note) & client_id %in% c(client_cohort) & active == TRUE) %>% 
  mutate(Title = "imported from ServicePoint Service",
         Legacy_ProgramID = as.numeric(provide_provider_id),
         EnrollmentID = "",
         provide_start_date = format.Date(provide_start_date, "%Y-%m-%d"),
         service_note = str_squish(gsub("\r?\n|\r", " ", service_note))) %>%
  left_join(all_projects, by = "Legacy_ProgramID") %>%
  select("PersonalID" = client_id,
         "AgencyID" = Clarity_AgencyID,
         Legacy_ProgramID,
         EnrollmentID,
         "ServicesID" = need_service_id,
         Title,
         "Date" = provide_start_date,
         "Note" = service_note,
         "DateCreated" = date_added,
         "DateUpdated" = date_updated)

# Notes attached to Needs -------------------------------------------------

notes_needs <- sp_need %>% 
  filter(!is.na(note) & active == TRUE & client_id %in% c(client_cohort)) %>% 
  mutate(Title = "imported from ServicePoint Need",
         note = paste("Note:", note, 
                      "\rStatus:", status, 
                      "\rOutcome:", outcome, 
                      "\rReason Unmet:", reason_unmet),
         Legacy_ProgramID = as.numeric(provider_id),
         EnrollmentID = "",
         ServicesID = "",
         date_set = format.Date(date_set, "%Y-%m-%d"),
         note = str_squish(gsub("\r?\n|\r", " ", note))) %>%
  left_join(all_projects, by = "Legacy_ProgramID") %>%
  select("PersonalID" = client_id, 
         "AgencyID" = Clarity_AgencyID, 
         Legacy_ProgramID,
         EnrollmentID,
         ServicesID,
         Title,
         "Date" = date_set,
         "Note" = note,
         "DateCreated" = date_added, 
         "DateUpdated" = date_updated)

# Exit Notes --------------------------------------------------------------

notes_exits <- sp_entry_exit %>%
  left_join(all_projects, by = c("provider_id" = "Legacy_ProgramID")) %>%
  filter(!is.na(notes) & 
           active == TRUE & 
           ymd_hms(exit_date) >= ymd("20140601") &
           client_id %in% c(client_cohort)) %>%
  mutate(Title = paste("Exit note: Exited", 
                       Legacy_ProgramName, 
                       "on", 
                       format.Date(exit_date, "%Y-%m-%d %T")),
         Legacy_ProgramID = as.numeric(provider_id),
         ServicesID = "", 
         exit_date = format.Date(exit_date, "%Y-%m-%d"),
         notes = str_squish(gsub("\r?\n|\r", " ", notes))) %>%
  select("PersonalID" = client_id, 
         Legacy_ProgramID,
         "AgencyID" = Clarity_AgencyID,
         ServicesID,
         "EnrollmentID" = entry_exit_id, # some are null
         Title,
         "Date" = exit_date, 
         "Note" = notes,
         "DateCreated" = date_added, 
         "DateUpdated" = date_updated)

# All together ------------------------------------------------------------

All_Notes_prep <- rbind(
  notes_clients,
  notes_exits,
  notes_goals,
  notes_needs,
  notes_services
) %>%
  filter(PersonalID %in% c(client_cohort) & 
           !is.na(Note)) %>%
  left_join(all_projects, by = "Legacy_ProgramID")

notes_proper_agency <- All_Notes_prep %>%
  mutate(
    AgencyID = case_when(
      Legacy_ProgramID == 1139 ~ 134,
      Legacy_ProgramID == 594 ~ 28,
      Legacy_ProgramID == 1775 ~ 299,
      Legacy_ProgramID == 220 ~ 58,
      Legacy_ProgramID == 1355 ~ 4,
      Legacy_ProgramID == 592 ~ 91,
      Legacy_ProgramID == 12 ~ 9,
      Legacy_ProgramID == 417 ~ 84,
      Legacy_ProgramID == 865 ~ 299,
      Legacy_ProgramID == 1808 ~ 299,
      Legacy_ProgramID == 1316 ~ 9,
      Legacy_ProgramID == 855 ~ 99,
      Legacy_ProgramID == 1775 ~ 299,
      Legacy_ProgramID == 1775 ~ 299,
      Legacy_ProgramID == 590 ~ 163, # there were others but these have higher counts
      TRUE ~ AgencyID
    )
  )

notes_missing_agency <- notes_proper_agency %>%
  filter(is.na(AgencyID)) %>%
  count(Legacy_ProgramID) %>%
  left_join(sp_provider %>% select(provider_id, name), 
            by = c("Legacy_ProgramID" = "provider_id"))

All_Notes <- notes_proper_agency %>%
  filter(!is.na(AgencyID)) %>%
  mutate(NoteID = row_number()) %>%
  select(
    NoteID,
    PersonalID,
    "AgencyID" = Clarity_AgencyID,
    EnrollmentID,
    ServicesID,
    Title,
    Date,
    Note,
    DateCreated,
    DateUpdated
  )

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








