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
library(janitor)

source(here("reading_severance.R"))


# Getting filtered data sets ----------------------------------------------

dose_subs <- da_recordset %>%
  filter(active == TRUE & 
           question == "COVID-19 Vaccine Doses" &
           client_id %in% c(client_cohort)) %>% 
  rename("subassessment_name" = question,
         "sub_is_active" = active,
         "sub_date_added" = date_added,
         "sub_provider_created" = provider_creating_id,
         "sub_user_created" = user_creating_id) 

dose_answers <- da_recordset_answer %>%
  filter(
    active == TRUE &
      question %in% c(
        "Client Contact Information",
        "COVID-19 Vaccine Manufacturer",
        "Date Received Vaccine Dose",
        "Vaccine Documentation"
      )
  ) %>% 
  rename(
    "question_name" = question,
    "answer_active" = active,
    "answer_date_added" = date_added,
    "answer_provider_created" = provider_creating_id,
    "answer_user_created" = user_creating_id
  ) %>%
  semi_join(dose_subs,
            by = "recordset_id")

# Deduplicating subs and then answers based on correct date_effective -----

deduplicated <- dose_subs %>%
  left_join(dose_answers, by = "recordset_id") %>%
  group_by(recordset_id, question_name) %>%
  slice_max(answer_date_added) %>% 
  ungroup() %>%
  select(-recordset_answer_id, -sub_date_added, -answer_date_added) %>% 
  unique() %>%
  pivot_wider(names_from = question_name, values_from = val) %>%
  rename("c_covid19_client_contact_info" = "Client Contact Information",
         "c_covid19_vaccine_manufacturer" = "COVID-19 Vaccine Manufacturer",
         "c_covid19_date_vaccine_administered" = "Date Received Vaccine Dose",
         "c_covid19_vaccine_documentation" = "Vaccine Documentation")

dose_data <- deduplicated %>%
  select("PersonalID" = client_id,
         "Legacy_ProgramID" = sub_provider_created, 
         "InformationDate" = date_effective,
         c_covid19_client_contact_info,
         c_covid19_vaccine_manufacturer,
         c_covid19_date_vaccine_administered,
         c_covid19_vaccine_documentation
  ) %>%
  left_join(sp_provider %>% select(provider_id, hud_organization_id),
            by = c("Legacy_ProgramID" = "provider_id")) %>%
  mutate(
    AssessmentID = 195,
    AssessmentName = "COVID-19 Vaccine Doses",
    InformationDate = format.Date(InformationDate, "%Y-%m-%d"),
    c_covid19_vaccine_manufacturer = recode(
      c_covid19_vaccine_manufacturer,
      "Client doesn't know and data could not be obtained from other source" = 4,
      "Johnson & Johnson" = 1,
      "Pfizer" = 3,
      "Moderna" = 2
    ), 
    c_covid19_vaccine_documentation = recode(
      c_covid19_vaccine_documentation,
      "Self-report" = 2,
      "Healthcare provider" = 1,
      "Vaccine card" = 3
    ),
    AssessmentCustomID = row_number()
  ) %>% 
  left_join(clarity_projects_orgs %>% 
              select(Legacy_AgencyID, Clarity_AgencyID) %>%
              unique(), 
            by = c("hud_organization_id" = "Legacy_AgencyID")) %>%
  filter(!is.na(Clarity_AgencyID)) %>%
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         PersonalID,
         "AgencyID" = Clarity_AgencyID,
         InformationDate,
         "assessment_date" = c_covid19_date_vaccine_administered,
         c_covid19_client_contact_info,
         c_covid19_vaccine_manufacturer,
         c_covid19_vaccine_documentation) 

# Writing it out to csv ---------------------------------------------------

write_csv(dose_data, here("data_to_Clarity/Assessment_Custom_doses.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(InformationDate = 
             format.Date(InformationDate, "%Y-%m-%d"),
           assessment_date = 
             format.Date(assessment_date, "%Y-%m-%d"))
  
  fwrite(x, 
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Assessment_Custom_doses")


