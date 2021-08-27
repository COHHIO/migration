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

# covid -------------------------------------------------------------------

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
    assessment_date = InformationDate,
    Clarity_AgencyID = if_else(is.na(Clarity_AgencyID), 299, Clarity_AgencyID)
  ) %>% 
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         "PersonalID" = client_id,
         "AgencyID" = Clarity_AgencyID,
         InformationDate,
         assessment_date,
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

# spdats ------------------------------------------------------------------

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

# building vi-spdat -------------------------------------------------------

spdat_data <- deduplicated %>%
  select(
    "PersonalID" = client_id,
    "Legacy_ProgramID" = sub_provider_created,
    "InformationDate" = date_effective,
    "UserID" = sub_user_created,
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
         UserID,
         assessment_date,
         assessment_type,
         assessment_level,
         assessment_location,
         c_vispdat_type,
         c_vispdat_program_name,
         c_vispdat_score) 

mahoning_providers <- c(2384, 1641, 697, 1639, 1739, 1704, 1638, 1640,1392, 696, 
                        1738, 2103, 2339, 2322, 2091, 1773, 110, 2335, 2364,
                        2351, 2333, 2365, 2383, 2344, 2317, 2316, 2326, 2332,
                        2358, 2352, 2341, 2373, 2371, 2105, 2331, 2334, 2329,
                        2325, 2323, 2346, 2349, 2338, 2385, 2437, 1330, 2376,
                        2382, 2375, 2328, 2359, 2374, 2348, 2362, 2378, 2380, 
                        1331, 2342, 1327, 2353, 2379, 2372, 2354, 2340, 2355,
                        2356, 2363, 2381, 2370, 2357, 2366, 2377, 2345, 2360,  
                        2318, 2367, 2336, 2343, 2368, 2319, 2369, 2444, 2445
)

spdat_data2 <- deduplicated %>%
  select(
    "PersonalID" = client_id,
    "Legacy_ProgramID" = sub_provider_created,
    "InformationDate" = date_effective,
    "UserID" = sub_user_created,
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
    )) %>%
  left_join(all_projects, by = "Legacy_ProgramID") %>%
  mutate(
    c_vispdat_program_name = Legacy_ProgramName,
    AssessmentCustomID = row_number(),
    Clarity_AgencyID = if_else(Legacy_ProgramID %in% c(mahoning_providers), 300, 299)
  ) %>% 
  filter(!is.na(InformationDate)) %>% # servicepoint bug
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         UserID,
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

# offers ------------------------------------------------------------------
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
         "UserID" = sub_user_created,
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
    assessment_date = format.Date(offer_date, "%Y-%m-%d"),
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
         assessment_date,
         UserID,
         "c_offer_type" = offer_type,
         "c_offer_accepted" = offer_accepted,
         "c_offer_accept_decline_date" = offer_accept_decline_date) 

# doses -------------------------------------------------------------------

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
         "UserID" = sub_user_created,
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
    assessment_date = format.Date(c_covid19_date_vaccine_administered, "%Y-%m-%d"),
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
  mutate(Clarity_AgencyID = case_when(Legacy_ProgramID %in% c(1793, 1695) ~ 299,
                                      TRUE ~ Clarity_AgencyID)) %>%
  select(AssessmentCustomID,
         AssessmentID,
         AssessmentName,
         PersonalID,
         "AgencyID" = Clarity_AgencyID,
         InformationDate,
         UserID,
         assessment_date,
         c_covid19_client_contact_info,
         c_covid19_vaccine_manufacturer,
         c_covid19_vaccine_documentation) 

# combined ----------------------------------------------------------------

final_dose_data <- dose_data %>% select(-AssessmentCustomID)
final_covid_data <- covid_data %>% select(-AssessmentCustomID)
final_offer_data <- offer_data %>% select(-AssessmentCustomID)
final_spdat_data <- spdat_data2 %>% select(-AssessmentCustomID)

common_columns <- c(
  "AssessmentID",                                 
  "AssessmentName",                               
  "PersonalID",                                   
  "AgencyID",  
  "InformationDate",
  "assessment_date",                              
  "UserID" 
)

common_columns %in% colnames(final_covid_data)
common_columns %in% colnames(final_dose_data)
common_columns %in% colnames(final_offer_data)
common_columns %in% colnames(final_spdat_data)


combined <- final_covid_data %>%
  full_join(final_dose_data, by = c(common_columns)) %>%
  full_join(final_offer_data, by = c(common_columns)) %>%
  full_join(final_spdat_data, by = c(common_columns)) %>%
  mutate(AssessmentCustomID = row_number(),
         InformationDate = ymd(InformationDate),
         assessment_date = ymd(assessment_date),
         c_covid19_investigated_contact_date = ymd_hms(c_covid19_investigated_contact_date),
         c_covid19_investigation_determination_date = ymd_hms(c_covid19_investigation_determination_date),
         c_covid19_screening_date = ymd_hms(c_covid19_screening_date),
         c_covid19_test_date = ymd_hms(c_covid19_test_date),
         c_covid19_confirmed_contact_date = ymd_hms(c_covid19_confirmed_contact_date),
         c_offer_accept_decline_date = ymd_hms(c_offer_accept_decline_date)) %>%
  filter(PersonalID != 243321)

# write it out ------------------------------------------------------------

write_csv(combined, here("data_to_Clarity/Assessment_Custom_combined.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = 
                  "ncnnDDnTnTcnnTnnnnnnTnTnnnnnnnnnnnnncnnnnTnnnncnn") %>%
    mutate(
      InformationDate =
        format.Date(InformationDate, "%Y-%m-%d"),
      assessment_date =
        format.Date(assessment_date, "%Y-%m-%d"),
      c_covid19_investigated_contact_date =
        format.Date(ymd_hms(c_covid19_investigated_contact_date), "%Y-%m-%d"),
      c_covid19_investigation_determination_date =
        format.Date(c_covid19_investigation_determination_date, "%Y-%m-%d"),
      c_covid19_screening_date =
        format.Date(c_covid19_screening_date, "%Y-%m-%d"),
      c_covid19_test_date =
        format.Date(c_covid19_test_date, "%Y-%m-%d"),
      c_covid19_confirmed_contact_date =
        format.Date(c_covid19_confirmed_contact_date, "%Y-%m-%d"),
      c_offer_accept_decline_date =
        format.Date(c_offer_accept_decline_date, "%Y-%m-%d")
    )
  
  fwrite(x,
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Assessment_Custom_combined")

# ^^ this throws a warning but it's ok because it's just upset about the columns
# that are being assumed to be of type logical because the first however many rows
# are null. It all starts with column 8, which makes sense and is expected.


x <- read_csv(here("data_to_Clarity/Assessment_Custom_combined.csv"),
              col_types = 
                "ncnnDDnTnTcnnTnnnnnnTnTnnnnnnnnnnnnncnnnnTnnnncnn") %>%
  mutate(
    InformationDate =
      format.Date(InformationDate, "%Y-%m-%d"),
    assessment_date =
      format.Date(assessment_date, "%Y-%m-%d"),
    c_covid19_investigated_contact_date =
      format.Date(c_covid19_investigated_contact_date, "%Y-%m-%d"),
    c_covid19_investigation_determination_date =
      format.Date(c_covid19_investigation_determination_date, "%Y-%m-%d"),
    c_covid19_screening_date =
      format.Date(c_covid19_screening_date, "%Y-%m-%d"),
    c_covid19_test_date =
      format.Date(c_covid19_test_date, "%Y-%m-%d"),
    c_covid19_confirmed_contact_date =
      format.Date(c_covid19_confirmed_contact_date, "%Y-%m-%d"),
    c_offer_accept_decline_date =
      format.Date(c_offer_accept_decline_date, "%Y-%m-%d")
  )

