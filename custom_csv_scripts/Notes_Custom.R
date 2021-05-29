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

raw_notes_goals <- read_xlsx("random_data/Notes.xlsx", sheet = 1)

notes_goals <- raw_notes_goals %>%
  mutate(Title = "imported from Goals",
         Program = str_extract(Program, "[0-9]+"),
         UserID = str_extract(UserID, "[0-9]+"))

# Client Notes ------------------------------------------------------------

raw_notes_client <- read_xlsx("random_data/Notes.xlsx", sheet = 2)

notes_client <- raw_notes_client %>%
  mutate(Title = "imported from Clients",
         Program = str_extract(Program, "[0-9]+"),
         UserID = str_extract(UserID, "[0-9]+"))

# Notes attached to Services ----------------------------------------------

service_notes <- sp_need_service %>% 
  filter(!is.na(service_note) & active == TRUE) %>% 
  select(client_id, date_added, provide_provider_id, service_note) %>%
  mutate(Title = "imported from ServicePoint Service")

# Notes attached to Needs -------------------------------------------------

need_notes <- sp_need %>% 
  filter(!is.na(note) & active == TRUE) %>% 
  select(client_id, date_added, provider_id, note) %>%
  mutate(Title = "imported from ServicePoint Need")

# Exit Notes --------------------------------------------------------------

exit_notes <- sp_entry_exit %>%
  filter(!is.na(notes) & active == TRUE) %>%
  select(client_id, 
         date_added, 
         date_updated, 
         entry_exit_id, 
         exit_date,
         provider_id, 
         notes) %>%
  mutate(Title = paste("Exit note: Exited Project ID", 
                       provider_id, 
                       "on", 
                       format.Date(exit_date, "%m-%d-%Y")))












