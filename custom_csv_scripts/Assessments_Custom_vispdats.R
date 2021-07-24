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
      question %in% c("VI-SPDAT v2.0", "VI-FSPDAT v2.0", "TAY-VI-SPDAT v1.0") &
      client_id %in% c(client_cohort)
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



# # Projects to Organizations -----------------------------------------------
# 
# sp_projects_orgs <- sp_provider %>%
#   filter(active == TRUE) %>%
#   select("SP_ProjectID" = provider_id, 
#          "SP_ProjectName" = name, 
#          "SP_AgencyID" = hud_organization_id) %>%
#   left_join(sp_provider %>% select("SP_AgencyID" = provider_id, "AgencyName" = name),
#             by = "SP_AgencyID")
# 
# clarity_projects_orgs <- sp_projects_orgs %>%
#   left_join(id_cross, by = c("SP_ProjectID" = "Legacy_ProgramID")) %>%
#   filter(!is.na(Clarity_ProgramID)) %>%
#   select(
#     SP_ProjectID,
#     SP_ProjectName,
#     SP_AgencyID,
#     "SP_AgencyName" = AgencyName,
#     "Clarity_ProjectID" = Clarity_ProgramID,
#     "Clarity_ProjectName" = Clarity_ProgramName,
#     "Clarity_AgencyID" = Clarity_AgencyID,
#     "Clarity_AgencyName" = Clarity_AgencyName
#   )

# building vi-spdat -------------------------------------------------------

# agencies <- read_csv("frozen/Agencies.csv") %>% pull(id)

spdat_data <- deduplicated %>%
  select(
    "PersonalID" = client_id,
    "Legacy_ProgramID" = sub_provider_created,
    "InformationDate" = date_effective,
    "c_vispdat_score" = Score,
    "assessment_date" = date_effective,
    "c_vispdat_type" = subassessment_name,
    "c_vispdat_program_name" = sub_provider_created
  ) %>%
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
  left_join(
    sp_provider %>% select(provider_id, hud_organization_id),
    by = c("Legacy_ProgramID" = "provider_id")
  ) %>%
  left_join(all_projects, by = "Legacy_ProgramID") %>%
  mutate(
    c_vispdat_program_name = Legacy_ProgramName,
    AssessmentCustomID = row_number(),
    Clarity_AgencyID = if_else(is.na(Clarity_AgencyID), 299, Clarity_AgencyID)
  ) %>% 
  filter(!is.na(InformationDate)) %>% # servicepoint bug
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         PersonalID,
         "AgencyID" = Clarity_AgencyID,
         InformationDate,
         assessment_date,
         assessment_type,
         assessment_level,
         assessment_location,
         c_vispdat_type,
         c_vispdat_program_name,
         c_vispdat_score) 

# Writing it out to csv ---------------------------------------------------

write_csv(spdat_data, here("data_to_Clarity/Assessment_Custom_spdats.csv"))

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

fix_date_times("Assessment_Custom_spdats")

