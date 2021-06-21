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

cohort_services <- sp_need_service %>%
  left_join(
    sp_provider %>% select(provider_id, program_type_code),
    by = c("provide_provider_id" = "provider_id")
  ) %>%
  filter(active == TRUE &
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
  left_join(sp_provider %>% select("AgencyID" = provider_id, "AgencyName" = name),
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
    code
  ) %>%
  left_join(projects_orgs %>% select(ProjectID, AgencyID),
            by = "ProjectID")

# work out a way to eliminate stray services from the dataset and connect each
# Service to an EnrollmentID.

# get list of ServiceItemIDs from Clarity (once that exists) and connect these
# Services to them based on Funding Source

# Also pull in the amounts



