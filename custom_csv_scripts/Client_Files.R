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

library(data.table)
library(tidyverse)
library(lubridate)
library(here)
library(janitor)

source(here("reading_severance.R"))

files_client <- sp_file_attachment %>%
  filter(entity_name != "ROI" & 
           client_id %in% c(client_cohort) &
           active == TRUE) %>%
  select(
    "FileID" = server_filename,
    "PersonalID" = client_id,
    "ProjectID" = provider_creating_id,
    "DateCreated" = date_created,
    "DateUpdated" = date_updated,
    "UserID" = user_creating_id,
    file_name
  ) %>%
  group_by(FileID) %>% # can't assign a single file to multiple ClientIDs
  slice_min(PersonalID) %>% # affects very few clients
  ungroup() %>%
  mutate(
    Category = 13,
    FileName = "Other",
    OtherName = file_name,
    ExportID = as.numeric(format.Date(today(), "%Y%m%d"))
  ) %>%
  left_join(clarity_projects_orgs, by = c("ProjectID" = "SP_ProjectID")) %>%
  select(
    FileID,
    PersonalID,
    "AgencyID" = Clarity_AgencyID,
    Category,
    FileName,
    OtherName,
    DateCreated,
    DateUpdated,
    UserID,
    ExportID
  )

# Writing it out to csv ---------------------------------------------------

write_csv(files_client, here("data_to_Clarity/Client_Files.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(DateCreated = 
             format.Date(DateCreated, "%Y-%m-%d %T"),
           DateUpdated = 
             format.Date(DateUpdated, "%Y-%m-%d %T"))
  
  fwrite(x, 
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Client_Files")






