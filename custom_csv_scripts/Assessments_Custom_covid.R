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

covid_data_raw <- da_answer %>%
  filter(
    active == TRUE &
      question_code %in% c("IFYESDATEOFCONTACTWIT", #c_covid19_confirmed_contact_date
                      "COHCOV19CONTACTPERSON_1", #c_covid19_contact_with_confirmed
                      "COHCOV19NONCONFIDENTI", #c_covid19_notes
                      "HASAMEDICALPROFESSION_1", #c_covid19_under_investigation
                      "COHCOV19CONTACTPERSON", #c_covid19_contact_with_investigated
                      "IFYESDATEOFCONTACTWIT_1", #c_covid19_investigated_contact_date
                      "DATEOFDETERMINATION", #c_covid19_investigation_determination_date
                      "COHCOV19CHRONICILLNESS", # c_covid19_risk_chronic_illness
                      "COHCOV19HISTORYRESPIR", # c_covid19_risk_history_of_respiratory_illness
                      "AREYOUIMMUNOCOMPROMIS", # c_covid19_risk_immunocompromised
                      "DOYOUHAVECHRONICKIDNE", # c_covid19_risk_kidney_disease
                      "COHCOV1960OLDER", # c_covid19_risk_over_65
                      "HAVEYOUSMOKEDTOBACCOI", # c_covid19_risk_smoker
                      "COHCOV19ASSESSMENTDATE", # c_covid19_screening_date
                      "HASAMEDICALPROFESSION", # c_covid19_tested
                      "DATEOFCOVID19DIAGNOSIS", # c_covid19_test_date
                      "COVID19TESTRESULTS", # c_covid19_test_results
                      "COHCOV19DIFFICULTYBRE", # c_symptom_breathing_difficult
                      "COHCOV19CHILLS", # c_symptom_chills
                      "DOYOUHAVECONGESTIONOR", # c_symptom_congestion
                      "COHCOV19COUGHTODAY", # c_symptom_cough
                      "DOYOUHAVEDIARRHEA", # c_symptom_diarrhea
                      "COHCOV19FEVERPASTDAY", # c_symptom_fever
                      "COHCOV19HEADACHE", # c_symptom_headache
                      "COHCOV19NEWLOSSTASTEA", # c_symptom_lost_taste_smell
                      "COHCOV19MUSCLEPAIN", # c_symptom_muscle_pain
                      "DOYOUHAVENAUSEAORVOMI", # c_symptom_nausea
                      "COHCOV19SORETHROATTOD", # c_symptom_sore_throat
                      "AREYOUFEELINGTOOWEAKT") & # c_symptom_weak
      client_id %in% c(clients_we_gave_them)
  ) 

covid_data <- covid_data_raw %>%
  mutate(InformationDate = format.Date(date_effective, "%Y-%m-%d")) %>%
  group_by(client_id, InformationDate, provider_id, user_id) %>%
  slice_max(date_effective) %>% #taking the most recent answer in a day
  ungroup() %>%
  pivot_wider(id_cols = c(client_id, InformationDate, provider_id, user_id),
              names_from = question_code,
              values_from = val) %>%
  left_join(all_projects, by = c("provider_id" = "Legacy_ProgramID")) %>%
  mutate(
    AssessmentID = 178,
    AssessmentName = "COVID-19 Screening Tool",
    AssessmentCustomID = row_number(),
    Clarity_AgencyID = if_else(is.na(Clarity_AgencyID), 299, Clarity_AgencyID)
  ) %>% 
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         "PersonalID" = client_id,
         "AgencyID" = Clarity_AgencyID,
         InformationDate,
         "UserID" = user_id,
         "c_covid19_investigated_contact_date" = IFYESDATEOFCONTACTWIT_1, 
         "c_covid19_contact_with_confirmed" = COHCOV19CONTACTPERSON_1, 
         "c_covid19_confirmed_contact_date" = IFYESDATEOFCONTACTWIT,
         "c_covid19_notes" = COHCOV19NONCONFIDENTI, 
         "c_covid19_under_investigation" = HASAMEDICALPROFESSION_1, 
         "c_covid19_contact_with_investigated" = COHCOV19CONTACTPERSON, 
         "c_covid19_investigation_determination_date" = DATEOFDETERMINATION, 
         "c_covid19_risk_chronic_illness" = COHCOV19CHRONICILLNESS, 
         "c_covid19_risk_history_of_respiratory_illness" = COHCOV19HISTORYRESPIR,
         "c_covid19_risk_immunocompromised" = AREYOUIMMUNOCOMPROMIS, 
         "c_covid19_risk_kidney_disease" = DOYOUHAVECHRONICKIDNE, 
         "c_covid19_risk_over_65" = COHCOV1960OLDER, 
         "c_covid19_risk_smoker" = HAVEYOUSMOKEDTOBACCOI, 
         "c_covid19_screening_date" = COHCOV19ASSESSMENTDATE, 
         "c_covid19_tested" = HASAMEDICALPROFESSION, 
         "c_covid19_test_date" = DATEOFCOVID19DIAGNOSIS, 
         "c_covid19_test_results" = COVID19TESTRESULTS, 
         "c_symptom_breathing_difficult" = COHCOV19DIFFICULTYBRE, 
         "c_symptom_chills" = COHCOV19CHILLS, 
         "c_symptom_congestion" = DOYOUHAVECONGESTIONOR, 
         "c_symptom_cough" = COHCOV19COUGHTODAY, 
         "c_symptom_diarrhea" = DOYOUHAVEDIARRHEA, 
         "c_symptom_fever" = COHCOV19FEVERPASTDAY, 
         "c_symptom_headache" = COHCOV19HEADACHE, 
         "c_symptom_lost_taste_smell" = COHCOV19NEWLOSSTASTEA, 
         "c_symptom_muscle_pain" = COHCOV19MUSCLEPAIN, 
         "c_symptom_nausea" = DOYOUHAVENAUSEAORVOMI, 
         "c_symptom_sore_throat" = COHCOV19SORETHROATTOD, #
         "c_symptom_weak" = AREYOUFEELINGTOOWEAKT) %>%
  mutate(
    across(starts_with("c_covid19_risk"), ~case_when(.x == "N" ~ 0, .x == "Y" ~ 1)),
    across(starts_with("c_symptom"), ~case_when(.x == "N" ~ 0, .x == "Y" ~ 1)),
    c_covid19_under_investigation = case_when(
      c_covid19_under_investigation == "N" ~ 0,
      c_covid19_under_investigation == "Y" ~ 1
    ),
    c_covid19_contact_with_investigated = case_when(
      c_covid19_contact_with_investigated == "N" ~ 0,
      c_covid19_contact_with_investigated == "Y" ~ 1),
    c_covid19_contact_with_confirmed = case_when(
      c_covid19_contact_with_confirmed == "N" ~ 0,
      c_covid19_contact_with_confirmed == "Y" ~ 1
    ),
    c_covid19_test_results = case_when(
      c_covid19_test_results == "Negative" ~ 0,
      c_covid19_test_results == "Positive" ~ 1,
      c_covid19_test_results == "Unknown" ~ 2
    ),
    c_covid19_tested = case_when(
      c_covid19_tested == "N" ~ 0,
      c_covid19_tested == "Y" ~ 1
    )
  )

# Writing it out to csv ---------------------------------------------------

write_csv(covid_data, here("data_to_Clarity/Assessment_Custom_covid.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(InformationDate = 
             format.Date(InformationDate, "%Y-%m-%d"),
           c_covid19_investigated_contact_date = 
             format.Date(c_covid19_investigated_contact_date, "%Y-%m-%d"),
           c_covid19_investigation_determination_date = 
             format.Date(c_covid19_investigation_determination_date, "%Y-%m-%d"),
           c_covid19_screening_date = 
             format.Date(c_covid19_screening_date, "%Y-%m-%d"),
           c_covid19_test_date = 
             format.Date(c_covid19_test_date, "%Y-%m-%d"))
  
  fwrite(x, 
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Assessment_Custom_covid")

