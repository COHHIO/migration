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

# This script should return all secondary addresses associated with an Agency.

library(janitor)
library(here)

source(here("Project_Import_Schema/Agencies.R"))

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


duplicates <- same_zips_different_addresses %>%
  group_by(Address1, Address2, City, State, ZIP) %>%
  summarise(ProjectCount = n(),
            ProjectIDs = toString(ProjectID)) %>%
  ungroup()

write_csv(duplicates, here("random_data/sites_projects.csv"))

# using this to show duplicate addresses that need aligning in SP
# takes a human to figure out what Project needs to be aligned
# stopped at line 605, handing over to NB


# Get all sites -----------------------------------------------------------

# assumes data in SP has been cleaned up

all_unique_addresses <- Addresses %>%
  select(Address1, Address2, City, State, ZIP) %>%
  unique() %>%
  mutate(SiteID = rownames(.)) 

# Remove all sites already associated to an Agency ------------------------

sites <- all_unique_addresses %>%
  anti_join(agency_from_export %>%
              left_join(Addresses[
                c(
                  "ProjectID",
                  "Address1",
                  "Address2",
                  "City",
                  "State",
                  "ZIP"
                )
              ], by = c("id" = "ProjectID")), by = c(
    "Address1",
    "Address2",
    "City",
    "ProjectCounty",
    "State",
    "ZIP"
  )) %>%
  mutate(geolocations.address = if_else(is.na(Address2), Address1,
                                        paste(Address1, Address2)),
         geolocations.city = City,
         geolocations.state = State,
         geolocations.zipcode = substr(ZIP, 1, 5)) %>%
  select(-Address1, -Address2, -City, -State, -ZIP, -ProjectCounty) %>%
  left_join(
    Agencies[c(
      "id",
      "geolocations.address",
      "geolocations.city",
      "geolocations.state",
      "geolocations.zipcode"
    )],
    by = c(
      "geolocations.address",
      "geolocations.city",
      "geolocations.state",
      "geolocations.zipcode"
    )
  )

# Writing it out to csv ---------------------------------------------------

write_csv(Sites, here("random_data/Sites.csv"))
