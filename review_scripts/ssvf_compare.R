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
library(janitor)
library(arsenal)

# Client file comparison --------------------------------------------------

client_legacy <-
  read_csv(here("random_data/talberthousessvf_legacy/Client.csv"))

client_clarity <-
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/Client.csv"))

client_comparison <- summary(comparedf(client_clarity, client_legacy))

# Client file comparison results: Veteran Status --------------------------

vets_clarity <- client_clarity %>% select(FirstName, LastName, VeteranStatus)

vets_legacy <- client_legacy %>% select(FirstName, LastName, VeteranStatus)

veteran_differences <- vets_clarity %>%
  full_join(vets_legacy, by = c("FirstName", "LastName")) %>%
  filter(VeteranStatus.x != VeteranStatus.y) 

# Client file comparison results: SSN DQ ----------------------------------

ssn_dq_clarity <- client_clarity %>% select(FirstName, LastName, SSNDataQuality)

ssn_dq_legacy <- client_legacy %>% select(FirstName, LastName, SSNDataQuality)

ssn_dq_differences <- ssn_dq_clarity %>%
  full_join(ssn_dq_legacy, by = c("FirstName", "LastName")) %>%
  filter(SSNDataQuality.x != SSNDataQuality.y)

# Client file comparison results: Discharge Status ------------------------

discharge_status_clarity <- 
  client_clarity %>% select(FirstName, LastName, DischargeStatus)

discharge_status_legacy <- 
  client_legacy %>% select(FirstName, LastName, DischargeStatus)

discharge_status_differences <- discharge_status_clarity %>%
  full_join(discharge_status_legacy, by = c("FirstName", "LastName")) 

# Disabilities file comparison --------------------------------------------

disabilities_legacy <-
  read_csv(here("random_data/talberthousessvf_legacy/Disabilities.csv")) %>%
  left_join(client_legacy %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID")

disabilities_clarity <-
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/Disabilities.csv")) %>%
  left_join(client_clarity %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID")

disabilities_comparison <- 
  summary(comparedf(disabilities_clarity, disabilities_legacy))

disability_differences <- disabilities_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)


# Disabilities file comparison: why different row counts? -----------------

disabilities_by_name_diffs <- disabilities_clarity %>%
  count(FirstName, LastName) %>%
  full_join(disabilities_legacy %>%
              count(FirstName, LastName), by = c("FirstName", "LastName")) %>%
  filter(n.x != n.y)

# Disabilities file comparison results: DisabilityType --------------------

disability_type_clarity <- 
  disabilities_clarity %>% 
  select(FirstName, LastName, DisabilityType, DisabilityResponse) %>%
  unite("Type_Response", DisabilityType, DisabilityResponse) %>%
  unique() %>% arrange(FirstName, Type_Response)

disability_type_legacy <- 
  disabilities_legacy %>% 
  select(FirstName, LastName, DisabilityType, DisabilityResponse) %>%
  unite("Type_Response", DisabilityType, DisabilityResponse) %>%
  unique() %>% arrange(FirstName, Type_Response)

disability_type_differences <- disability_type_clarity %>%
  anti_join(disability_type_legacy, 
            by = c("FirstName", "LastName", "Type_Response"))


# Disabilities file comparison results: IndefiniteAndImpairs ----------------

IndefiniteAndImpairs_clarity <- 
  disabilities_clarity %>% 
  select(FirstName, LastName, DisabilityType, DisabilityResponse, IndefiniteAndImpairs) %>%
  unite("Type_Response", DisabilityType, DisabilityResponse) %>%
  unique()

IndefiniteAndImpairs_legacy <- 
  disabilities_legacy %>% 
  select(FirstName, LastName, DisabilityType, DisabilityResponse, IndefiniteAndImpairs) %>%
  unite("Type_Response", DisabilityType, DisabilityResponse) %>%
  unique() 

IndefiniteAndImpairs_diffs <- IndefiniteAndImpairs_clarity %>%
  full_join(IndefiniteAndImpairs_legacy,
            by = c("FirstName", "LastName", "Type_Response")) %>%
  filter(IndefiniteAndImpairs.x != IndefiniteAndImpairs.y)

# Disabilities file comparison results: DataCollectionStage ---------------

disabilities_DataCollectionStage_clarity <- 
  disabilities_clarity %>% 
  select(FirstName,
         LastName,
         DisabilityType,
         DisabilityResponse,
         DataCollectionStage) %>%
  unite("Name", FirstName, LastName) %>%
  unite("Type_Response", DisabilityType, DisabilityResponse, DataCollectionStage) %>%
  unique()

disabilities_DataCollectionStage_legacy <- 
  disabilities_legacy %>% 
  select(FirstName,
         LastName,
         DisabilityType,
         DisabilityResponse,
         DataCollectionStage) %>%
  unite("Name", FirstName, LastName) %>%
  unite("Type_Response", DisabilityType, DisabilityResponse, DataCollectionStage) %>%
  unique() 

disabilities_DataCollectionStage_diffs <- disabilities_DataCollectionStage_clarity %>%
  anti_join(disabilities_DataCollectionStage_legacy,
            by = c("Name")) 


# Disabilities file comparison results: DateCreated/DateUpdated -----------

disabilities_dates_clarity <- disabilities_clarity %>%
  select(FirstName, LastName, DisabilitiesID, DateCreated, DateUpdated) %>%
  filter(FirstName != "Reece") %>%
  group_by(FirstName, LastName) %>%
  summarise(maxCreatedDate = max(ymd_hms(DateCreated)),
            minCreatedDate = min(ymd_hms(DateCreated)),
            maxUpdatedDate = max(ymd_hms(DateUpdated)),
            minUpdatedDate = min(ymd_hms(DateUpdated)))

disabilities_dates_legacy <- disabilities_legacy %>%
  select(FirstName, LastName, DisabilitiesID, DateCreated, DateUpdated) %>%
  filter(FirstName != "Reece") %>%
  group_by(FirstName, LastName) %>%
  summarise(maxCreatedDate = max(ymd_hms(DateCreated)),
            minCreatedDate = min(ymd_hms(DateCreated)),
            maxUpdatedDate = max(ymd_hms(DateUpdated)),
            minUpdatedDate = min(ymd_hms(DateUpdated)))

disabilities_dates_diffs <- disabilities_dates_legacy %>%
  full_join(disabilities_dates_clarity, by = c("FirstName", "LastName")) %>%
  mutate(
    Updated_different = if_else(
      maxUpdatedDate.x != maxUpdatedDate.y |
        minUpdatedDate.x != minUpdatedDate.y, 1, 0
    ),
    Created_different = if_else(
      maxCreatedDate.x != maxCreatedDate.y |
        minCreatedDate.x != minCreatedDate.y, 1, 0
    ),
    Max_Created_days_off = difftime(maxCreatedDate.x, maxCreatedDate.y,
                                    units = "days")
  ) 

# Services file comparison ------------------------------------------------

services_legacy <-
  read_csv(here("random_data/talberthousessvf_legacy/Services.csv")) %>%
  left_join(client_legacy %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

services_clarity <-
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/Services.csv")) %>%
  left_join(client_clarity %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

services_comparison <- summary(comparedf(services_clarity, services_legacy))

services_differences <- services_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)


# Services file comparison results: why different row counts? -------------

services_row_count <- services_legacy %>%
  select(Name, ServicesID) %>%
  count(Name) %>%
  full_join(services_clarity %>%
              select(Name, ServicesID) %>%
              count(Name),
            by = "Name") %>%
  filter(n.x != n.y)

clients_that_are_different <- services_row_count$Name

diffs_legacy <- services_legacy %>% 
  filter(Name %in% c(clients_that_are_different))

diffs_clarity <- services_clarity %>% 
  filter(Name %in% c(clients_that_are_different))


# Project file comparison -------------------------------------------------

projects_legacy <- 
  read_csv(here("random_data/talberthousessvf_legacy/Project.csv"))

project_clarity <- 
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/Project.csv"))

project_comparison <- summary(comparedf(project_clarity, projects_legacy))

project_differences <- project_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)


# IncomeBenefits file comparison ------------------------------------------

income_benefits_legacy <- 
  read_csv(here("random_data/talberthousessvf_legacy/IncomeBenefits.csv")) %>%
  left_join(client_legacy %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

income_benefits_clarity <- 
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/IncomeBenefits.csv")) %>%
  left_join(client_clarity %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

income_benefits_comparison <- 
  summary(comparedf(income_benefits_clarity, income_benefits_legacy))

income_benefits_differences <- 
  income_benefits_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)

single_record_ib_legacy <- 
  income_benefits_legacy %>%
  unite("SingleRecord", InformationDate:DataCollectionStage) %>%
  select(Name, SingleRecord)

single_record_ib_clarity <- 
  income_benefits_clarity %>%
  unite("SingleRecord", InformationDate:DataCollectionStage) %>%
  select(Name, SingleRecord)

diffs <- single_record_ib_clarity %>%
  full_join(single_record_ib_legacy, by = "Name") %>%
  filter(SingleRecord.x != SingleRecord.y)

# copied some SingleRecords that mostly match into Notepad++ to see where they
# differed, just found some very basic and fine things that are ok and good,
# everything else was just rows not matching up (since the IDs differ)

# Enrollment file comparison ----------------------------------------------

enrollments_legacy <- 
  read_csv(here("random_data/talberthousessvf_legacy/Enrollment.csv")) %>%
  left_join(client_legacy %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

enrollments_clarity <- 
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/Enrollment.csv")) %>%
  left_join(client_clarity %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

enrollments_comparison <- 
  summary(comparedf(enrollments_clarity, enrollments_legacy))

enrollments_differences <- 
  enrollments_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)

# Exit file comparison ----------------------------------------------

exits_legacy <- 
  read_csv(here("random_data/talberthousessvf_legacy/Exit.csv")) %>%
  left_join(client_legacy %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

exits_clarity <- 
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/Exit.csv")) %>%
  left_join(client_clarity %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

exits_comparison <- 
  summary(comparedf(exits_clarity, exits_legacy))

exits_differences <- 
  exits_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)

# EnrollmentCoC file comparison -------------------------------------------

enrollment_coc_legacy <- 
  read_csv(here("random_data/talberthousessvf_legacy/EnrollmentCoC.csv")) %>%
  left_join(client_legacy %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

enrollment_coc_clarity <- 
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/EnrollmentCoC.csv")) %>%
  left_join(client_clarity %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

enrollment_coc_comparison <- 
  summary(comparedf(enrollment_coc_clarity, enrollment_coc_legacy))

enrollment_coc_differences <- 
  enrollment_coc_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)


# EmploymentEducation file comparison -------------------------------------

employment_education_legacy <- 
  read_csv(here("random_data/talberthousessvf_legacy/EmploymentEducation.csv")) %>%
  left_join(client_legacy %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

employment_education_clarity <- 
  read_csv(here("data_from_Clarity/talberthousessvf_clarity/EmploymentEducation.csv")) %>%
  left_join(client_clarity %>% select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  unite("Name", FirstName, LastName)

employment_education_comparison <- 
  summary(comparedf(employment_education_clarity, employment_education_legacy))

employment_education_differences <- 
  employment_education_comparison[["diffs.byvar.table"]] %>%
  filter(n > 0)






