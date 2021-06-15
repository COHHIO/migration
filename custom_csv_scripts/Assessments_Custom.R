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

vispdat_subs <- da_recordset %>%
  filter(
    active == TRUE &
      question %in% c("VI-SPDAT v2.0", "VI-FSPDAT v2.0", "TAY-VI-SPDAT v1.0")
  ) %>% 
  rename("subassessment_name" = question,
         "sub_is_active" = active,
         "sub_date_added" = date_added,
         "sub_provider_created" = provider_creating_id,
         "sub_user_created" = user_creating_id) 

vispdat_answers <- da_recordset_answer %>%
  filter(active == TRUE & question %in% c("GRAND TOTAL", "Start Date")) %>%
  rename("question_name" = question,
         "answer_active" = active,
         "answer_date_added" = date_added,
         "answer_provider_created" = provider_creating_id,
         "answer_user_created" = user_creating_id) %>%
  semi_join(vispdat_subs,
            by = "recordset_id") %>%
  mutate(corrected_date_effective = if_else(question_name == "Start Date",
                                            val, NULL))

# Correcting date_effective in severance file with Start Date -------------

get_start_dates <- vispdat_answers %>%
  left_join(vispdat_subs, by = "recordset_id") %>%
  select(recordset_id, date_effective, corrected_date_effective) %>%
  filter(!is.na(corrected_date_effective)) %>%
  select(-date_effective) %>% unique()

corrected_subs <- vispdat_subs %>%
  left_join(get_start_dates, by = "recordset_id") %>%
  mutate(date_effective = format.Date(corrected_date_effective, "%Y-%m-%d")) %>%
  group_by(client_id, date_effective, subassessment_name) %>%
  slice_max(sub_date_added) %>%
  ungroup() %>%
  select(-corrected_date_effective) %>% unique()

# Deduplicating subs and then answers based on correct date_effective -----

deduplicated <- corrected_subs %>%
  left_join(vispdat_answers, by = "recordset_id") %>%
  filter(question_name == "GRAND TOTAL") %>%
  group_by(recordset_id, question_name) %>%
  slice_max(answer_date_added) %>% 
  ungroup() %>%
  rename("Score" = val) %>%
  select(-question_name, -corrected_date_effective)

# Translators -------------------------------------------------------------



# Projects to Organizations -----------------------------------------------

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


# building vi-spdat -------------------------------------------------------

agencies <- read_csv("frozen/Agencies.csv") %>% pull(id)

spdat_data <- deduplicated %>%
  select("PersonalID" = client_id,
         "xProjectIDx" = sub_provider_created, # waiting on new severance file
         "InformationDate" = date_effective,
         "c_vispdat_score" = Score, 
         "assessment_date" = date_effective,
         "c_vispdat_type" = subassessment_name,
         "c_vispdat_program_name" = sub_provider_created) %>%
  mutate(
    AssessmentID = 193,
    AssessmentName = "ServicePoint VI-SPDAT Assessment",
    InformationDate = format.Date(InformationDate, "%Y-%m-%d"),
    assessment_type = 1,
    assessment_level = 2,
    assessment_location = 0,
    c_vispdat_type = recode(
      c_vispdat_type,
      'VI-SPDAT v2.0' = 1,
      'VI-FSPDAT v2.0' = 2,
      'TAY-VI-SPDAT v1.0' = 3
    )
  ) %>%
  left_join(projects_orgs, by = c("c_vispdat_program_name" = "ProjectID")) %>%
  mutate(AgencyID = case_when(xProjectIDx %in% c(2372, 1695) ~ xProjectIDx,
                              TRUE ~ AgencyID),
         c_vispdat_program_name = ProjectName, 
         AssessmentCustomID = row_number()) %>% 
  filter(!is.na(ProjectName) & (AgencyID %in% c(agencies) |
                                  AgencyID == 2372)) %>%
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         PersonalID,
         AgencyID,
         InformationDate,
         assessment_date,
         assessment_type,
         assessment_level,
         assessment_location,
         c_vispdat_type,
         c_vispdat_program_name,
         c_vispdat_score) 

# Offers ------------------------------------------------------------------

offers_subs <- da_recordset %>%
  filter(question == "Offers of Permanent Housing" & active == TRUE)

offers_answers <- da_recordset_answer %>%
  filter(active == TRUE) %>%
  semi_join(offers_subs, by = "recordset_id") %>%
  pivot_wider(names_from = question, values_from = val)

