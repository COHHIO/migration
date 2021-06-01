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


fspdat_data <- da_recordset %>%
  filter(question == "VI-FSPDAT v2.0" & active == TRUE) %>%
  rename("subassessment_name" = question) %>%
  left_join(da_recordset_answer %>%
              filter(active == TRUE), by = "recordset_id") %>%
  rename("question_name" = question) %>%
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
  ) %>%
  select("PersonalID" = client_id,
         "provider_creating" = provider_creating_id.x, 
         "InformationDate" = date_effective,
         question_name, val) %>%
  group_by(PersonalID, provider_creating, InformationDate) %>%
  mutate(group_id = cur_group_id()) %>%
  ungroup() %>%
  pivot_wider(names_from = question_name, 
              values_from = val) %>%  
  mutate(
    AssessmentID = "87",
    AssessmentName = "VI-F-SPDAT Prescreen for Families [V2]",
    InformationDate = format.Date(InformationDate, "%Y-%m-%d")
  )




