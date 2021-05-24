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

if(!exists("da_answer")) {
  source(here("reading_severance.R"))
}


# get ees together --------------------------------------------------------

ees <- sp_entry_exit %>%
  left_join(entry_exit_answer_link, by = "entry_exit_id") %>%
  select(entry_exit_id, client_id, answer_id, point_in_time_type)

# County Served -----------------------------------------------------------

county_served <- da_answer %>% 
  filter(question_code == "COUNTY" & active == TRUE) %>%
  rename("date_answer_added" = date_added) %>%
  left_join(ees, by = c("client_id", "answer_id")) %>%
  filter(!is.na(entry_exit_id)) %>%
  select("EnrollmentID" = entry_exit_id,
         "PersonalID" = client_id,
         "InformationDate" = date_effective,
         "DataCollectionStage" = point_in_time_type,
         "DateUpdate" = date_added,
         "UserID" = user_id,
         "county_served" = val
         ) %>%
  mutate(DataCollectionStage = case_when(
    DataCollectionStage == "Entry" ~ 1,
    DataCollectionStage == "Interim Review" ~ 2,
    DataCollectionStage == "Exit" ~ 3
  ))

county_prior <- da_answer %>% 
  filter(question_code == "COUNTYOFRESIDENCEPRIOR" & active == TRUE) %>%
  rename("date_answer_added" = date_added) %>%
  left_join(ees, by = c("client_id", "answer_id")) %>%
  filter(!is.na(entry_exit_id)) %>%
  select("EnrollmentID" = entry_exit_id,
         "PersonalID" = client_id,
         "InformationDate" = date_effective,
         "DataCollectionStage" = point_in_time_type,
         "DateUpdate" = date_added,
         "UserID" = user_id,
         "county_prior" = val
  ) %>%
  mutate(DataCollectionStage = case_when(
    DataCollectionStage == "Entry" ~ 1,
    DataCollectionStage == "Interim Review" ~ 2,
    DataCollectionStage == "Exit" ~ 3
  ))

Enrollment_Custom <- county_served %>%
  full_join(county_prior, by = c("EnrollmentID",
                                 "PersonalID",
                                 "InformationDate",
                                 "DataCollectionStage",
                                 "DateUpdate",
                                 "UserID")) %>%
  mutate(ExportID = as.numeric(today()),
         EnrollmentCustomID = row_number()) %>%
  relocate(EnrollmentCustomID, .before = EnrollmentID) %>%
  relocate(ExportID, .after = UserID)


# Checks ------------------------------------------------------------------

art_counties <- read_xlsx("data_to_Clarity/RMisc2.xlsx", sheet = 1) %>% 
  select(EnrollmentID, CountyPrior, CountyServed) %>%
  filter(!is.na(CountyPrior) | !is.na(CountyServed))

severance_counties <- Enrollment_Custom %>%
  select(EnrollmentID, county_prior, county_served)

mutual_ee_ids <- art_counties %>%
  left_join(severance_counties, by = "EnrollmentID")

