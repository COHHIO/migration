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

projects_orgs <- read_xlsx(
  here("data_to_Clarity/RMisc2.xlsx"),
  sheet = 3,
  col_types = c("numeric", replicate(16, "text"))
) %>%
  filter(!is.na(OrganizationName) &
           OrganizationName != "Coalition on Homelessness and Housing in Ohio(1)") %>%
  mutate(
    AgencyID = str_extract(OrganizationName, "\\(?[0-9]+\\)?"),
    AgencyID = str_remove(AgencyID, "[(]"),
    AgencyID = str_remove(AgencyID, "[)]"),
    AgencyName = str_remove(OrganizationName, "\\(.*\\)")
  ) %>%
  select(ProjectID, AgencyID, AgencyName)

hud_specs <- read_csv("random_data/HUDSpecs.csv", col_types = "cccc") %>%
  filter(DataElement == "FundingSource") %>%
  mutate(ReferenceNo = as.double(ReferenceNo))

FundingSources <- Funder %>%
  left_join(hud_specs, by = c("Funder" = "ReferenceNo")) %>%
  left_join(projects_orgs, by = "ProjectID") %>%
  mutate(
    id = FunderID,
    name= Description,
    ref_agency = AgencyID,
    status = 1,
    funding_source = Funder,
    funding_source_non_federal = OtherFunder,
    amount = "",
    grant_identifier = GrantID,
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

