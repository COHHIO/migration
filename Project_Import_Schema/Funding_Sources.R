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
library(HMIS)
library(here)
library(readxl)
library(data.table)

# get necessary data ------------------------------------------------------

source(here("reading_severance.R"))

Project <- 
  read_csv(here("data_to_Clarity/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

provider_extras <- read_xlsx(
  paste0("data_to_Clarity/RMisc2.xlsx"),
  sheet = 3,
  col_types = c("numeric", replicate(16, "text"))
) %>% 
  mutate(
    OrganizationName = str_remove(OrganizationName, "\\(.*\\)")
  )

Organization <- 
  read_csv(here("data_to_Clarity/Organization.csv"),
           col_types = "ncncTTnTn") %>%
  left_join(provider_extras[c("ProjectID", "ProjectName")], 
            by = c("OrganizationID" = "ProjectID")) %>%
  mutate(OrganizationName = ProjectName, ProjectName = NULL)

Funder <- 
  read_csv("data_to_Clarity/Funder.csv",
           col_types = "nnnccDDTTcTn")

projects_orgs <- sp_provider %>%
  filter(active == TRUE) %>%
  select("ProjectID" = provider_id, 
         "ProjectName" = name, 
         "AgencyID" = hud_organization_id) %>%
  left_join(sp_provider %>% select("AgencyID" = provider_id, "AgencyName" = name),
            by = "AgencyID")

hud_specs <- read_csv("random_data/HUDSpecs.csv", col_types = "cccc") %>%
  filter(DataElement == "FundingSource") %>%
  mutate(ReferenceNo = as.double(ReferenceNo))

FundingSources <- Funder %>%
  left_join(hud_specs, by = c("Funder" = "ReferenceNo")) %>%
  left_join(projects_orgs, by = "ProjectID") %>%
  mutate(
    id = FunderID,
    name = paste(ProjectName, Description, sep = ": "), 
    ref_agency = AgencyID,
    status = if_else(is.na(EndDate) | ymd(EndDate) > today(), 1, 0), 
    funding_source = Funder,
    funding_source_non_federal = case_when(
     	OtherFunder == "Bezos Day One" ~ 1,
     	OtherFunder == "CDBG" ~ 2,
     	OtherFunder == "church" ~ 3,
     	OtherFunder %in% c("Community", "community") ~ 4,
     	OtherFunder == "ODH" ~ 5,
     	OtherFunder == "ODSA" ~ 6,
     	OtherFunder == "OHFA" ~ 7,
     	OtherFunder == "Ohio Department of Health- Youth Initiative" ~ 8,
     	OtherFunder == "OSDA" ~ 9,
     	OtherFunder == "Pandemic Emergency Fund" ~ 10,
     	OtherFunder == "TANF" ~ 11,
     	OtherFunder == "Unknown" ~ 12,
     	OtherFunder == "CSBG" ~ 13,
     	OtherFunder == "Local" ~ 14,
     	OtherFunder == "OHFA ERA" ~ 15,
     	OtherFunder == "OVW Transitional Housing" ~ 16,
     	OtherFunder == "United Way" ~ 17,
     	OtherFunder == "COVID-19 Deconcentration" ~ 18,
     	OtherFunder == "Risk Mitigation" ~ 19
    ), # comes from https://docs.google.com/spreadsheets/d/1NZLRcv4m57HBKnME-Bcqy__zKD2q-WIPnEqYYesh88U/edit?usp=sharing
    amount = 0,
    grant_identifier = if_else(GrantID == "0", "N/A", GrantID),
    start_date = StartDate,
    end_date = EndDate,
    added_date = DateCreated,
    last_updated = DateUpdated
  ) %>%
  select(id:last_updated)

# Writing it out to csv ---------------------------------------------------
write_csv(FundingSources, here("data_to_Clarity/FundingSources.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(start_date = format.Date(start_date, "%Y-%m-%d"),
           end_date = format.Date(end_date, "%Y-%m-%d"),
           added_date = format.Date(added_date, "%Y-%m-%d %T"),
           last_updated = format.Date(last_updated, "%Y-%m-%d %T"))
  
  fwrite(x, 
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("FundingSources")

