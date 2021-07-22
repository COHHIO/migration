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

library(tidyverse)
library(lubridate)
library(here)

# from ServicePoint:
source(here("reading_severance.R"))

# from Clarity live (may need updating):
clarity_service_items <- read_csv(here("data_from_Clarity/service_item_ids.csv")) %>%
  mutate(
    ServiceItemName = case_when(
      ServiceItemName %in% c("Case management", "Case management services") ~
        "Case Management",
      ServiceItemName == "Moving costs" ~ "Moving Cost Assistance",
      ServiceItemName == "Utility payments" ~ "Utility Payments",
      ServiceItemName %in% c("Security deposit", "Security deposits") ~
        "Security Deposit",
      ServiceItemName %in% c("Utility deposit", "Utility deposits") ~
        "Utility Deposit",
      TRUE ~ ServiceItemName
    )
  )

all_service_items <- clarity_service_items %>%
  select(ServiceItemName) %>%
  unique()

nrow(all_service_items) == 122 # YOU WANT TRUE! If FALSE, check service_translator

# connector
service_translator <- tibble(
  sp_code = c(
    "PH-1000",
    "BH-3800.7000",
    "BH-3800.7250",
    "BV-8900.9300",
    "BV-8900.9150",
    "BH-1800.8500-300",
    "BH-3800.5150"
  ),
  sp_desc = c(
    "Case/Care Management",
    "Rent Payment Assistance",
    "Rental Deposit Assistance",
    "Utility Service Payment Assistance",
    "Utility Deposit Assistance",
    "Homeless Motel Vouchers",
    "Moving Expense Assistance"
  ),
  clarity_desc = c(
    "Case Management",
    "Rental Assistance",
    "Security Deposit",
    "Utility Payments",
    "Utility Deposit",
    "Motel and Hotel Vouchers",
    "Moving Cost Assistance"
  )
)

# from ServicePoint:

cohort_services <- sp_need_service %>%
  left_join(
    sp_provider %>% select(provider_id, program_type_code),
    by = c("provide_provider_id" = "provider_id")
  ) %>%
  filter(active == TRUE &
           client_id %in% client_cohort &
           is.na(hopwa_service_type) &
           is.na(path_service_type) &
           is.na(rhy_service_type) &
           is.na(ssvf_service_type) &
           is.na(ssvf_fin_assist_type) &
           ((
             program_type_code == "Homelessness Prevention (HUD)" &
               code %in% c(
                 "PH-1000",
                 "BH-3800.7000",
                 "BH-3800.7250",
                 "BV-8900.9150",
                 "BV-8900.9300"
               )
           ) |
             (
               program_type_code == "PH - Rapid Re-Housing (HUD)" &
                 code %in% c(
                   "PH-1000",
                   "BH-3800.7000",
                   "BH-3800.7250",
                   "BV-8900.9150",
                   "BV-8900.9300",
                   "BH-3800.5150",
                   "BH-1800.8500-300"
                 )
             ) |
             (
               program_type_code == "Other (HUD)" &
                 code %in% c(
                   "BH-3800.7000",
                   "BH-3800.7250",
                   "BV-8900.9150",
                   "BV-8900.9300"
                 )
             ) |
             (
               program_type_code == "Emergency Shelter (HUD)" &
                 code == "BH-1800.8500-300"
             )
           ))

# mostly ServicePoint + connector data:

prep <- cohort_services %>%
  select(
    "ServicesID" = need_service_id,
    "PersonalID" = client_id,
    "Legacy_ProgramID" = provide_provider_id,
    "DateProvided" = provide_start_date,
    "DateCreated" = date_added,
    "DateUpdated" = date_updated,
    "UserID" = user_creating_id,
    "sp_code" = code
  ) %>%
  left_join(clarity_projects_orgs,
            by = "Legacy_ProgramID") %>%
  relocate(Clarity_AgencyID, .after = PersonalID) %>%
  left_join(sp_entry_exit %>%
              filter(active == TRUE) %>%
              select(client_id, entry_exit_id, entry_date, exit_date, provider_id),
            by = c("PersonalID" = "client_id")) %>%
  mutate(
    exit_adjust = if_else(is.na(exit_date), now(), exit_date),
    enrollment_interval = interval(ymd_hms(entry_date), ymd_hms(exit_adjust))
  ) %>%
  filter(ymd_hms(DateProvided) %within% enrollment_interval &
           provider_id == Legacy_ProgramID) %>%
  rename("EnrollmentID" = entry_exit_id) %>%
  relocate(EnrollmentID, .after = Clarity_AgencyID) %>%
  select(ServicesID:Clarity_ProgramName)

service_items <- prep %>%
  left_join(service_translator, by = "sp_code") %>%
  left_join(clarity_service_items, by = c("clarity_desc" = "ServiceItemName",
                                          "Clarity_ProgramName", 
                                          "Clarity_ProgramID")) %>%
  unique()

projects_not_done <- service_items %>% 
  filter(is.na(ServiceItemID)) %>% 
  count(Legacy_ProgramName, sp_desc, clarity_desc) %>%
  mutate(ok = case_when(
    str_detect(Legacy_ProgramName, "SSVF") |
      str_detect(Legacy_ProgramName, "YHDP") |
      str_detect(Legacy_ProgramName, "RHY") ~ "ok", 
    TRUE ~ "not ok"))

services_not_connecting <- service_items %>% 
  filter(is.na(ServiceItemID))

fund_amounts <- sp_need_service_group_fund %>%
  filter(active == TRUE &
           last_action %in% c("Modified", "Submitted")) %>%
  select(need_service_group_id, cost, source) %>%
  left_join(sp_need_service %>%
              select(need_service_group_id,
                     need_service_id), by = "need_service_group_id") %>%
  select("ServicesID" = need_service_id, source, "FAAmount" = cost)

sp_service_fund_types <- fund_amounts$source %>% unique() %>% sort()

hud_funding_sources <- read_csv(here("random_data/HUDSpecs.csv")) %>%
  filter(DataElement == "FundingSource") %>%
  mutate(ReferenceNo = as.numeric(ReferenceNo))

fund_translator <- tibble(
  SPServiceFundingSource = c(sp_service_fund_types),
  funding_source = c(
    46, 46, 46, 46, 46, 46, 47, 46, 46, 46, 15, 16, 17, 46, 46, 46, 2, 2, 46, 
    34, 46, 46, 46, 46, 2, 46, 46, 2, 46, 46, 2, 46, 46, 33, 33, 43, 46
  ),
  funding_source_other = c(
    14, 14, 14, 2, 18, 13, NA, 0, 0, 6, 0, 0, 0, 6, 6, 6, NA, NA, 14, NA, 5, 7,
    7, 10, NA, 19, 14, NA, 14, 6, NA, 11, 14, NA, NA, NA, 14
  )
) %>%
  left_join(hud_funding_sources, by = c("funding_source" = "ReferenceNo")) %>%
  left_join(other_funding_source_crosswalk, 
            by = c("funding_source_other" = "ReferenceNo"))


  
  
  
  

