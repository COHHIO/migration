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

Project <- Project %>%
  select(-ProjectName) %>%
  left_join(provider_extras, by = "ProjectID")

Organization <- 
  read_csv(here("data_to_Clarity/Organization.csv"),
           col_types = "ncncTTnTn") %>%
  left_join(provider_extras[c("ProjectID", "ProjectName")], 
            by = c("OrganizationID" = "ProjectID")) %>%
  mutate(OrganizationName = ProjectName, ProjectName = NULL)

ProjectCoC <- 
  read_csv(here("data_to_Clarity/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTn")

Addresses <- read_xlsx(
    here("data_to_Clarity/RMisc2.xlsx"),
    sheet = 3,
    col_types = c("numeric", replicate(16, "text"))
  ) %>% 
  mutate(OrganizationName = str_remove(OrganizationName, "\\(.*\\)"))

bf_counties <- read_csv(here("data_to_Clarity/BFCounties.csv")) %>%
  rename("ProjectCounty" = 2) %>%
  mutate(ProjectCounty = str_remove(ProjectCounty, " County"))

# Adding in any data that can come from the HUD CSV Export ----------------

# The granularity of this dataset is the Organization, 1 site:1 Organization

agency_from_export <- Organization %>%
  select("id" = OrganizationID,
         "name" = OrganizationName,
         # "default_site_name" = OrganizationName,
         "victim_service_provider" = VictimServicesProvider,
         "added_date" = DateCreated,
         "last_updated" = DateUpdated
         ) %>%
  filter(!is.na(name) & id != 1) # records filtered out from this are not needed. GD

hud_geocodes <- read_csv(here("random_data/hud_geocodes.csv")) %>%
  # mutate(NAME = str_remove(NAME, " County")) %>%
  select("Geocode" = 1, "Name" = NAME)

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
      "ZIP"
    )
  ], by = c("id" = "ProjectID")) %>%
  left_join(bf_counties, by = "ProjectCounty") %>%
  left_join(hud_geocodes, by = c("City" = "Name")) %>%
  left_join(hud_geocodes %>%
              filter(str_detect(Name, " County") == TRUE) %>%
              mutate(Name = str_remove(Name, " County")), 
            by = c("ProjectCounty" = "Name")) %>%
  mutate(
    default_site_name = case_when(is.na(Address2) & !is.na(Address1) ~ Address1,
                                  is.na(Address1) & !is.na(Address2) ~ Address2,
                     !is.na(Address1) & !is.na(Address2) ~ paste(Address1, Address2),
                     is.na(Address1) & is.na(Address2) ~ paste("Confidential -",
                                                               name)),
    geolocations.address = default_site_name,
    geolocations.city = City,
    geolocations.state = State,
    counties.id = case_when(
      is.na(ID) & id == 2455 ~ 0000, # Allegheny County, PA
      TRUE ~ ID
      ),
    geolocations.zipcode = substr(ZIP, 1, 5),
    geolocations.geocode = if_else(is.na(Geocode.x), Geocode.y, Geocode.x),  
    geolocations.geocode = case_when(
      is.na(geolocations.geocode) & id == 2455 ~ 429003,
      TRUE ~ geolocations.geocode
    ),
    site.name = name,
    ID = NULL
  ) %>%
  select(-Address1, -Address2, -City, -State, -ProjectCounty, -ZIP,
         -starts_with("Geocode."))

# Check for missing addresses of Organizations ----------------------------

missing_addresses <- incl_addresses %>%
  filter(is.na(geolocations.city))

write_csv(missing_addresses, here("random_data/missing_org_addresses.csv"))

# Adding in CoC at the Org level (which is weird, some straddle CoCs) -----

# This is currently written to just take a random CoC value where there
# are two CoC's associated with an Organization

coc <- ProjectCoC %>%
  left_join(Project[c("ProjectID", "OrganizationID")], by = "ProjectID") %>%
  select(OrganizationID, CoCCode) %>%
  unique() %>%
  select("id" = OrganizationID, "coc" = CoCCode) %>%
  filter(coc %in% c("OH-504", "OH-507"))

incl_coc <- incl_addresses %>%
  left_join(coc, by = "id") %>%
  group_by(id) %>%
  slice_max(coc)

# Adding in other columns -------------------------------------------------

Agencies <- incl_coc %>%
  mutate(
    status = 1, # active = 1, inactive = 2
    navigation_profiles.id = 1, # default = 1, can edit in UI later
    screens.name = 0, # not sure what goes here
    home_screen = 1, # default = 1
    ref_looker_report_open_units = 0, # may not be required
    all_client_forms_enabled = 0, 
    department = 0, # 0 = disabled, we can turn these on individually as the need arises
    clients = 2, # system shared
    release_of_information = 1, # 1 = defaults to system policy
    ref_coordinated_entry = 0, # don't know what this means
    ref_looker_report = 0, # don't know what this means
    send_referral_notifications = 1 # default = 1
  ) %>%
  relocate(victim_service_provider, .after = ref_looker_report_open_units) %>%
  relocate(c(added_date, last_updated), .after = all_client_forms_enabled)

# Writing it out to csv ---------------------------------------------------
write_csv(Agencies, here("data_to_Clarity/Agencies.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(added_date = format.Date(added_date, "%Y-%m-%d %T"),
           last_updated = format.Date(last_updated, "%Y-%m-%d %T"))
  
  fwrite(x, 
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Agencies")

