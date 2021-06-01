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


what_are_the_questions <- da_recordset %>%
  filter(question %in% c("VI-SPDAT v2.0", "VI-FSPDAT v2.0", "TAY-VI-SPDAT v1.0")) %>%
  rename("subassessment_name" = question) %>%
  left_join(da_recordset_answer, by = "recordset_id") %>%
  rename("question_name" = question) %>%
  select(subassessment_name, question_name) %>% unique() %>%
  filter(
    !question_name %in% c(
      "PRE-SURVEY",
      "A. HISTORY OF HOUSING AND HOMELESSNESS",
      "B. RISKS",
      "C. SOCIALIZATION & DAILY FUNCTIONS",
      "D. WELLNESS",
      "E. FAMILY UNIT",
      "GRAND TOTAL"
    )
  )

deduplicated_subs <- da_recordset %>%
  filter(active == TRUE) %>%
  rename("subassessment_name" = question,
         "sub_is_active" = active,
         "sub_date_added" = date_added,
         "sub_provider_created" = provider_creating_id,
         "sub_user_created" = user_creating_id) %>%
  mutate(sub_date_added = as.Date(format.Date(sub_date_added, "%Y-%m-%d")),
         date_effective = as.Date(format.Date(date_effective, "%Y-%m-%d"))) %>%
  group_by(client_id, date_effective, subassessment_name) %>%
  mutate(group_id = cur_group_id()) %>%
  slice_max(ymd(sub_date_added))

deduplicated_answers <- da_recordset_answer %>%
  filter(active == TRUE) %>%
  rename("question_name" = question,
         "answer_active" = active,
         "answer_date_added" = date_added,
         "answer_provider_created" = provider_creating_id,
         "answer_user_created" = user_creating_id) %>%
  semi_join(deduplicated_subs %>%
               filter(
                 subassessment_name %in% c("VI-SPDAT v2.0", "VI-FSPDAT v2.0", "TAY-VI-SPDAT v1.0")
               ),
             by = "recordset_id") %>%
  mutate(
    question_name = str_trunc(question_name, 65),
    question_name = make_clean_names(question_name)) %>%
  group_by(recordset_id, question_name) %>%
  slice_max(ymd_hms(answer_date_added)) %>% 
  ungroup()
  
  # left_join(da_recordset_answer %>%
  #             filter(active == TRUE), by = "recordset_id") %>% 
  # mutate(question_name = if_else(question_name == "Start Date", 
  #                                "StartDate",
  #                                question_name)) %>%
  # filter(question_name == "StartDate") %>%
  # pivot_wider(names_from = question_name, values_from = val) %>%
  # mutate(StartDate = as.Date(format.Date(StartDate, "%Y-%m-%d"))) %>%
  # group_by(client_id, date_effective, sub_provider_created, StartDate, subassessment_name) %>%
  # slice_max(ymd_hms(sub_date_added)) %>%
  # ungroup() 
  
  
  # question %in% c("VI-SPDAT v2.0", "VI-FSPDAT v2.0", "TAY-VI-SPDAT v1.0") &

  
  
  
  # select(client_id, date_effective, StartDate, "provider_creating_id" = sub_provider_created,
  #        subassessment_name, question_name, val) 
  
    

fspdat_data <- deduplicated %>%
  filter(
    subassessment_name == "VI-FSPDAT v2.0" &
      !question_name %in% c(
        "PRE-SURVEY",
        "A. HISTORY OF HOUSING AND HOMELESSNESS",
        "B. RISKS",
        "C. SOCIALIZATION & DAILY FUNCTIONS",
        "D. WELLNESS",
        "E. FAMILY UNIT",
        "GRAND TOTAL"
      )
  ) %>% 

  select("PersonalID" = client_id,
         provider_creating_id, 
         "InformationDate" = date_effective,
         question_name, 
         val) %>%
  pivot_wider(
    names_from = question_name,
    values_from = val) %>%  
  mutate(
    AssessmentID = "87",
    AssessmentName = "VI-F-SPDAT Prescreen for Families [V2]",
    InformationDate = format.Date(InformationDate, "%Y-%m-%d")
  )


# VI-SPDAT Prescreen for Single Adults [V2] (86)


# VI-Y-SPDAT Prescreen for Transition Age Youth (103)

