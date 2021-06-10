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
  mutate(corrected_date_effective = corrected_date_effective) %>%
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

translator_yn_dkr <- tibble(
  ReferenceNo = c(0, 1, 8, 9, 9, 9),
  Value = c(
    "No",
    "Yes",
    "Client doesn't know",
    "Client refused",
    "N/A or Refused",
    "Refused"
  )
)

translator_where <- tibble(
  ReferenceNo = c(1, 2, 3, 4, 5, 9),
  Value = c(
    "Shelters",
    "Transitional Housing",
    "Safe Haven",
    "Outdoors",
    "Other (Specify)",
    "Refused"
  )
)

translator_when <- tibble(
  ReferenceNo = c(3, 5, 8, 9),
  Value = c(
    "Less than 1 year",
    "One year or more",
    "Currently in stable housing",
    "Refused"
  )
)

translator_times <-
  tibble(
    ReferenceNo = c(0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 5, 9),
    Vaue = c(
      "0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "Greater than 10",
      "Refused"
    )
  )

# building vi-spdat -------------------------------------------------------

spdat_data <- deduplicated %>%
  select("PersonalID" = client_id,
         "xProjectIDx" = sub_provider_created, # have to translate to Agency
         "InformationDate" = date_effective,
         "c_vispdat_score" = Score, 
         "c_vispdat_date" = date_effective,
         "c_vispdat_type" = subassessment_name,
         "c_vispdat_project" = sub_provider_created) %>%
  mutate(
    AssessmentID = case_when(
      c_vispdat_type == "VI-FSPDAT v2.0" ~ "87",
      c_vispdat_type == "TAY-VI-SPDAT v1.0" ~ "103",
      c_vispdat_type == "VI-SPDAT v2.0" ~ "86"),
    AssessmentName = case_when(
      c_vispdat_type == "VI-FSPDAT v2.0" ~ "VI-F-SPDAT Prescreen for Families [V2]",
      c_vispdat_type == "TAY-VI-SPDAT v1.0" ~ "VI-Y-SPDAT Prescreen for Transition Age Youth",
      c_vispdat_type == "VI-SPDAT v2.0" ~ "VI-SPDAT Prescreen for Single Adults [V2]"),
    InformationDate = format.Date(InformationDate, "%Y-%m-%d")
  )


