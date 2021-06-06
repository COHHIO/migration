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

sp_question_codes <- c(
  "PERMANENTHOUSINGTRACK",
  "EXPECTEDPERMANENTHOUS",
  "WOULDTHECLIENTCONSENT",
  "IFNOTWHATARETHECONCER",
  "HOMESID",
  "LISTSTATUS",
  "SSVFINELIGIBLE",
  "VAELIGIBLE",
  "DATEVETERANIDENTIFIED"
)

answers_ph_track <- tibble(
  ReferenceNo = c(0:6),
  Value = c("None", "HUD - VASH", "PH", "PSH", "RRH", "Self Resolve", "SSVF-RRH")
)

answers_enhanced_yes_no <- tibble(
  ReferenceNo = c(0, 1, 8, 9, 99),
  Value = c("No (HUD)", "Yes (HUD)", "Client doesn't know (HUD)", 
            "Client refused (HUD)", "Data not collected (HUD)")
)

answers_list_status <- tibble(
  ReferenceNo = c(1:5),
  Value = c("Active - ES/TH",
            "Active - Unsheltered",
            "Inactive (Non-Permanent Housing)",
            "Inactive (Permanently Housed)",
            "Inactive (Uknown/Missing)")
)

answers_ssvf_ineligible <- tibble(
  ReferenceNo = c(1:3),
  Value = c("Max TFA", "Not VA eligible", "Over income")
)

answers_va_eligible <- tibble(
  ReferenceNo = c(1:4),
  Value = c("VA eligibility unknown",
            "Veteran eligible for all VA homeless services",
            "Veteran eligible for SSVF/GPD only",
            "Veteran not eligible for VA services")
)

client_level <- da_answer %>%
  filter(question_code %in% c(sp_question_codes) &
           active == TRUE) %>%
  group_by(client_id, question_code) %>%
  slice_max(date_effective) %>%
  slice_max(date_added) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(client_id),
    names_from = question_code,
    values_from = val
  ) %>%
  mutate(
    DATEVETERANIDENTIFIED = format.Date(DATEVETERANIDENTIFIED, "%Y-%m-%d"),
    EXPECTEDPERMANENTHOUS = format.Date(EXPECTEDPERMANENTHOUS, "%Y-%m-%d"),
    ClientCustomID = row_number(),
         ExportID = as.numeric(format.Date(today(), "%Y%m%d"))) %>%
  
  rename(
    "PersonalID" = client_id,
    "c_ph_track" = PERMANENTHOUSINGTRACK,
    "c_expected_ph_date" =	EXPECTEDPERMANENTHOUS,
    "c_covid19_consent_to_vaccine" =	WOULDTHECLIENTCONSENT,
    "c_covid19_vaccine_concerns" =	IFNOTWHATARETHECONCER,
    "c_homes_id" =	HOMESID,
    "c_list_status" =	LISTSTATUS,
    "c_ssvf_ineligible" =	SSVFINELIGIBLE,
    "c_va_eligible" =	VAELIGIBLE,
    "c_date_veteran_identified" =	DATEVETERANIDENTIFIED
  ) %>%
  relocate(ClientCustomID, .before = "PersonalID") %>%
  relocate(ExportID, .after = "PersonalID")

client_custom <- client_level %>%
  left_join(answers_enhanced_yes_no, by = c("c_covid19_consent_to_vaccine" = "Value")) %>%
  mutate(c_covid19_consent_to_vaccine = ReferenceNo,
         ReferenceNo = NULL) %>%
  left_join(answers_list_status, by = c("c_list_status" = "Value")) %>%
  mutate(c_list_status = ReferenceNo,
         ReferenceNo = NULL)  %>%
  left_join(answers_ph_track, by = c("c_ph_track" = "Value")) %>%
  mutate(c_ph_track = ReferenceNo,
         ReferenceNo = NULL)  %>%
  left_join(answers_ssvf_ineligible, by = c("c_ssvf_ineligible" = "Value")) %>%
  mutate(c_ssvf_ineligible = ReferenceNo,
         ReferenceNo = NULL)  %>%
  left_join(answers_va_eligible, by = c("c_va_eligible" = "Value")) %>%
  mutate(c_va_eligible = ReferenceNo,
         ReferenceNo = NULL) 

# Writing it out to csv ---------------------------------------------------

write_csv(client_custom, here("data_to_Clarity/Client_Custom.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(
      date_veteran_identified = format.Date(c_date_veteran_identified, "%Y-%m-%d"),
      expected_ph_date = format.Date(c_expected_ph_date, "%Y-%m-%d")
    )
  
  fwrite(x,
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Client_Custom")


