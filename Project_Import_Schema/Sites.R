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

phones <- read_xlsx(here("data_to_Clarity/RMisc2.xlsx"),
                    sheet = 18) %>%
  filter(ProjectTelPrimary == "Yes") %>%
  mutate(ProjectID = as.character(ProjectID)) %>%
  select(ProjectID, "phone" = ProjectTelNo)

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
  group_by(City, State, ZIP, Address1, Address2) %>%
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
  select(Address1, Address2, City, ProjectCounty, State, ZIP) %>%
  unique() %>%
  mutate(SiteID = rownames(.)) 

# Remove all sites already associated to an Agency ------------------------

# need a df with SP-ish address columns along with projects considered "Agencies"
# so we know what addresses need to be excluded
prep_agency_addresses <- agency_from_export %>%
  left_join(Addresses[c("ProjectID",
                        "Address1",
                        "Address2",
                        "City",
                        "ProjectCounty",
                        "State",
                        "ZIP")], by = c("id" = "ProjectID")) %>%
  rename("AgencyID" = "id")

# first removing all addresses that are already in the Agencies df, then joining
# that all the projects' addresses so we wind up with sites that are not 
# represented in the Agencies df but are attached to other projects
non_agency_sites <- all_unique_addresses %>%
  anti_join(
    prep_agency_addresses,
    by = c("Address1",
           "Address2",
           "City",
           "State",
           "ZIP")
  ) %>%
  left_join(
    Addresses %>% 
      select(
      "ProjectID",
      "Address1",
      "Address2",
      "City",
      "State",
      "ZIP"
    ),
    by = c("Address1", "Address2", "City", "State", "ZIP")
  )

# Matching ProjectIDs up with AgencyIDs -----------------------------------

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

sites <- non_agency_sites %>%
  left_join(projects_orgs, by = "ProjectID") %>%
  select(-ProjectID) %>% unique() %>%
  filter(!is.na(AgencyID))

shaping_sites <- sites %>%
  left_join(hud_geocodes, by = c("City" = "Name")) %>%
  left_join(hud_geocodes %>%
              filter(str_detect(Name, " County") == TRUE) %>%
              mutate(Name = str_remove(Name, " County")), 
            by = c("ProjectCounty" = "Name")) %>%
  left_join(phones, by = c("AgencyID" = "ProjectID")) %>%
  mutate(
    name = if_else(is.na(Address2), Address1,
                   paste(Address1, Address2)),
    geolocations.address = name,
    geolocations.city = City,
    geolocations.state = State,
    geolocations.zipcode = ZIP,
    ref_geography_type = "asked question in PS",
    location = "",
    geolocations.geocode = if_else(is.na(Geocode.x), Geocode.y, Geocode.x),    
    coc = if_else(ProjectCounty != "Mahoning", "OH-507", "OH-504"),
    status = 1,
    information_date = format.Date(today(), "%Y-%m-%d"),
    added_date = format.Date(today(), "%Y-%m-%d %T"),
    last_updated = format.Date(today(), "%Y-%m-%d %T"),
  ) %>%
  rename(
    "id" = SiteID,
    "refagency" = AgencyID) %>%
  select(-AgencyName, -Address1, -Address2, -City, -State, -ZIP, -ProjectCounty,
         -starts_with("Geocode")) %>%
  relocate(phone, .after = location)

# Writing it out to csv ---------------------------------------------------

write_csv(Sites, here("random_data/Sites.csv"))
