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

offer_subs <- da_recordset %>%
  filter(active == TRUE & 
           question == "Offers of Permanent Housing" &
           client_id %in% c(client_cohort)) %>% 
  rename("subassessment_name" = question,
         "sub_is_active" = active,
         "sub_date_added" = date_added,
         "sub_provider_created" = provider_creating_id,
         "sub_user_created" = user_creating_id) 

offer_answers <- da_recordset_answer %>%
  filter(
    active == TRUE &
      question %in% c(
        "Date of accept or decline",
        "Date of PH Intervention Offer",
        "Offer Accepted?",
        "Type of PH Intervention Offered"
      )
  ) %>% 
  rename(
    "question_name" = question,
    "answer_active" = active,
    "answer_date_added" = date_added,
    "answer_provider_created" = provider_creating_id,
    "answer_user_created" = user_creating_id
  ) %>%
  semi_join(offer_subs,
            by = "recordset_id")

# Deduplicating subs and then answers based on correct date_effective -----

deduplicated <- offer_subs %>%
  left_join(offer_answers, by = "recordset_id") %>%
  group_by(recordset_id, question_name) %>%
  slice_max(answer_date_added) %>% 
  ungroup() %>%
  select(-recordset_answer_id, -sub_date_added, -answer_date_added) %>% 
  unique() %>%
  pivot_wider(names_from = question_name, values_from = val) %>% 
  rename("offer_accept_decline_date" = 12,
         "offer_date" = 13,
         "offer_accepted" = 14,
         "offer_type" = 15)

offer_data <- deduplicated %>%
  select("PersonalID" = client_id,
         "Legacy_ProgramID" = sub_provider_created, 
         "InformationDate" = date_effective,
         offer_accept_decline_date,
         offer_date,
         offer_accepted,
         offer_type
  ) %>%
  left_join(sp_provider %>% select(provider_id, hud_organization_id),
            by = c("Legacy_ProgramID" = "provider_id")) %>%
  mutate(
    offer_date = case_when(
      is.na(ymd_hms(offer_date)) ~ InformationDate,
      TRUE ~ ymd_hms(offer_date)),
    AssessmentID = 194,
    AssessmentName = "Offers of Permanent Housing",
    InformationDate = format.Date(InformationDate, "%Y-%m-%d"),
    offer_accepted = recode(offer_accepted,
                            'N' = 0,
                            'Y' = 1),
    offer_type = recode(
      offer_type,
      "HUD VASH" = 1,
      "Other PH" = 2,
      "Other RRH" = 3,
      "SSVF RRH" = 4
    ),
    AssessmentCustomID = row_number()
  ) %>% 
  left_join(clarity_projects_orgs %>% 
              select(Legacy_AgencyID, Clarity_AgencyID) %>%
              unique(), 
            by = c("hud_organization_id" = "Legacy_AgencyID")) %>%
  mutate(Clarity_AgencyID = case_when(
    is.na(Clarity_AgencyID) ~ 299,
    TRUE ~ Clarity_AgencyID)) %>% 
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         PersonalID,
         "AgencyID" = Clarity_AgencyID,
         InformationDate,
         "assessment_date" = offer_date,
         "c_offer_type" = offer_type,
         "c_offer_accepted" = offer_accepted,
         "c_offer_accept_decline_date" = offer_accept_decline_date) 

# Writing it out to csv ---------------------------------------------------

write_csv(offer_data, here("data_to_Clarity/Assessment_Custom_offers.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(InformationDate = 
             format.Date(InformationDate, "%Y-%m-%d"),
           assessment_date = 
             format.Date(assessment_date, "%Y-%m-%d"),
           c_offer_accept_decline_date = 
             format.Date(c_offer_accept_decline_date, "%Y-%m-%d"))
  
  fwrite(x, 
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Assessment_Custom_offers")


