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

client_level <- da_answer %>%
  filter(question_code %in% c(sp_question_codes) &
           active == TRUE) %>%
  group_by(client_id, question_code) %>%
  slice_max(date_effective) %>%
  slice_max(date_added) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(client_id, date_effective, user_id),
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
    "DateUpdated" = date_effective,
    "UserID" = user_id,
    "ph_track" = PERMANENTHOUSINGTRACK,
    "expected_ph_date" =	EXPECTEDPERMANENTHOUS,
    "covid19_consent_to_vaccine" =	WOULDTHECLIENTCONSENT,
    "covid19_vaccine_concerns" =	IFNOTWHATARETHECONCER,
    "homes_id" =	HOMESID,
    "list_status" =	LISTSTATUS,
    "ssvf_ineligible" =	SSVFINELIGIBLE,
    "va_eligible" =	VAELIGIBLE,
    "date_veteran_identified" =	DATEVETERANIDENTIFIED
  ) %>%
  relocate(ClientCustomID, .before = "PersonalID") %>%
  relocate(ExportID, .after = "UserID")

# Writing it out to csv ---------------------------------------------------

write_csv(client_level, here("data_to_Clarity/Client_Custom.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(
      DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T"),
      date_veteran_identified = format.Date(date_veteran_identified, "%Y-%m-%d"),
      expected_ph_date = format.Date(expected_ph_date, "%Y-%m-%d")
    )
  
  fwrite(x,
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Client_Custom")


