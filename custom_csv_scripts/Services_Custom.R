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

source(here("reading_severance.R"))

clarity_service_items <- read_csv(here("data_from_Clarity/service_item_ids.csv")) %>%
  filter(!is.na(ServiceID))

all_service_items <- clarity_service_items %>%
  select(ServiceItemName) %>%
  unique()

service_translator <- tibble(
  sp_code = c(
    "PH-1000",
    "BH-3800.7000",
    "BH-3800.7250",
    "BV-8900.9300",
    "BV-8900.9150",
    "BH-1800.8500-300",
    "BH-3800.5150",
    "PH-1000",
    "PH-1000",
    "BH-3800.5150",
    "BH-3800.7250",
    "BH-3800.7250",
    "BV-8900.9150",
    "BV-8900.9150",
    "BV-8900.9300"
  ),
  sp_desc = c(
    "Case/Care Management",
    "Rent Payment Assistance",
    "Rental Deposit Assistance",
    "Utility Service Payment Assistance",
    "Utility Deposit Assistance",
    "Homeless Motel Vouchers",
    "Moving Expense Assistance",
    "Case/Care Management",
    "Case/Care Management",
    "Moving Expense Assistance",
    "Rental Deposit Assistance",
    "Rental Deposit Assistance",
    "Utility Deposit Assistance",
    "Utility Deposit Assistance",
    "Utility Service Payment Assistance"
  ),
  clarity_desc = c(
    "Case Management",
    "Rental Assistance",
    "Security Deposit",
    "Utility Payments",
    "Utility Deposit",
    "Motel and Hotel Vouchers",
    "Moving Cost Assistance",
    "Case management",
    "Case management services",
    "Moving costs",
    "Security deposit",
    "Security deposits",
    "Utility deposit",
    "Utility deposits",
    "Utility payments"
  )
)

cohort_services <- sp_need_service %>%
  left_join(
    sp_provider %>% select(provider_id, program_type_code),
    by = c("provide_provider_id" = "provider_id")
  ) %>%
  filter(active == TRUE &
           client_id %in% client_cohort &
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

projects_orgs <- sp_provider %>%
  filter(active == TRUE) %>%
  select("ProjectID" = provider_id, 
         "ProjectName" = name, 
         "AgencyID" = hud_organization_id) %>%
  left_join(sp_provider %>%
              select("AgencyID" = provider_id, "AgencyName" = name),
            by = "AgencyID")

prep <- cohort_services %>%
  select(
    "ServicesID" = need_service_id,
    "PersonalID" = client_id,
    "ProjectID" = provide_provider_id,
    "DateProvided" = provide_start_date,
    "DateCreated" = date_added,
    "DateUpdated" = date_updated,
    "UserID" = user_creating_id,
    "sp_code" = code
  ) %>%
  left_join(projects_orgs %>% select(ProjectID, AgencyID),
            by = "ProjectID") %>%
  relocate(AgencyID, .after = PersonalID) %>%
  left_join(sp_entry_exit %>%
              filter(active == TRUE) %>%
              select(client_id, entry_exit_id, entry_date, exit_date),
            by = c("PersonalID" = "client_id")) %>%
  mutate(
    exit_adjust = if_else(is.na(exit_date), now(), exit_date),
    enrollment_interval = interval(ymd_hms(entry_date), ymd_hms(exit_adjust))
  ) %>%
  filter(DateProvided %within% enrollment_interval) %>%
  rename("EnrollmentID" = entry_exit_id) %>%
  relocate(EnrollmentID, .after = AgencyID) %>%
  left_join(
    id_cross %>%
      rename("AgencyID" = Legacy_OrganizationID,
             "ProjectID" = Legacy_ProgramID),
    by = c("AgencyID", "ProjectID")
  ) %>%
  select(-entry_date, -exit_date, -enrollment_interval, -exit_adjust)

service_items <- prep %>%
  left_join(service_translator, by = "sp_code")

fund_amounts <- sp_need_service_group_fund %>%
  filter(active == TRUE &
           last_action %in% c("Modified", "Submitted")) %>%
  select(need_service_group_id, cost, source) %>%
  left_join(sp_need_service %>%
              select(need_service_group_id,
                     need_service_id), by = "need_service_group_id")

# some of these ^^ should have multiple Service Items in Clarity bc they have 
# multiple Funding Sources in SP



# get list of ServiceItemIDs from Clarity (once that exists) and connect these
# Services to them based on Funding Source

# Also pull in the amounts



