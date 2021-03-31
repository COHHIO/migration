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
library(janitor)

# get necessary data ------------------------------------------------------

Project <- 
  read_csv(here("data_to_Clarity/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

Organization <- 
  read_csv(here("data_to_Clarity/Organization.csv"),
           col_types = "ncncTTnTn") 

ProjectCoC <- 
  read_csv(here("data_to_Clarity/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTn")

Addresses <- read_xlsx(
    here("data_to_Clarity/RMisc2.xlsx"),
    sheet = 3,
    col_types = c("numeric", replicate(16, "text"))
  ) %>% 
  mutate(OrganizationName = str_remove(OrganizationName, "\\(.*\\)"))


# Adding in any data that can come from the HUD CSV Export ----------------

# The granularity of this dataset is the Organization, 1 site:1 Organization

agency_from_export <- Organization %>%
  select("id" = OrganizationID,
         "name" = OrganizationName,
         "default_site_name" = OrganizationName,
         "victim_service_provider" = VictimServicesProvider,
         "added_date" = DateCreated,
         "last_updated" = DateUpdated
         ) %>%
  filter(!is.na(name) & id != 1) # records filtered out from this are not needed. GD

# Getting Addresses of Organizations from RMisc ---------------------------

incl_addresses <- agency_from_export %>%
  left_join(Addresses[
    c(
      "ProjectID",
      "Address1",
      "Address2",
      "City",
      "State",
      "ProjectCounty",
      "ZIP",
      "Lat",
      "Long"
    )
  ], by = c("id" = "ProjectID")) %>%
  mutate(
    geolocations.address = paste(Address1, Address2),
    geolocations.city = City,
    geolocations.state = State,
    counties.id = ProjectCounty, # will need to be converted to a number from BF
    geolocations.zipcode = substr(ZIP, 1, 5),
    geolocations.geocode = paste(Lat, Long),
    site.name = name
  ) %>%
  select(id, name, starts_with("geolocations"), counties.id)


# Find Orgs with multiple "sites" -----------------------------------------

naturally_unique_addresses <- Addresses %>%
  select(Address1, Address2, City, State, ZIP) %>%
  unique()

same_zips_different_addresses <- naturally_unique_addresses %>%
  get_dupes(ZIP, City) %>%
  left_join(Addresses[c("ProjectID", "Address1", "Address2", "City", "State", "ZIP")], 
            by = c("Address1", "Address2", "City", "State", "ZIP")) %>%
  select(-dupe_count) %>%
  relocate(ProjectID, .before = "ZIP") %>%
  arrange(ZIP, Address1) %>%
  filter(!is.na(Address1))

# used this to show duplicate addresses that need aligning in SP
# takes a human to figure out what Project needs to be aligned
# stopped at Fremont Front Street (didn't do that one yet) ^^

all_unique_addresses <- Addresses %>%
  select(Address1, Address2, City, State, ZIP) %>%
  unique() %>%
  mutate(SiteID = rownames(.)) 




# Writing it out to csv ---------------------------------------------------

write_csv(Sites, here("random_data/Sites.csv"))
