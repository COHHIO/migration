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

library(here)
library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(tidycensus)


# Getting Geocodes --------------------------------------------------------

census2010 <- load_variables(2010, "sf1", cache = TRUE)

ohio <- get_decennial(geography = "county", 
                      year = 2010, 
                      state = 39,
                      variables = c("P005001")) %>%
  mutate(County = str_remove(NAME, " County, Ohio")) %>%
  select("Geocode" = GEOID, County) %>%
  unique()

# Getting HMIS data -------------------------------------------------------

# from ART because ART has the DataCollectionStage already figured out whereas
# the severance file would need a lot of work to get that.

at_entry <- read_xlsx("random_data/Enrollment_Custom_ART.xlsx", sheet = 1) %>%
  mutate(InformationDate = as.Date(InformationDate, origin = "1899-12-30"))

at_exit <- read_xlsx("random_data/Enrollment_Custom_ART.xlsx", sheet = 2) %>%
  mutate(InformationDate = as.Date(InformationDate, origin = "1899-12-30"))

counties_all <- rbind(at_entry, at_exit) %>%
  rename("county_served" = CountyServed, "county_prior" = CountyPrior) %>%
  mutate(ExportID = as.numeric(format.Date(today(), "%Y%m%d")),
         EnrollmentCustomID = row_number()) %>%
  relocate(EnrollmentCustomID, .before = "PersonalID")
# there's no duplicate combinations of EnrollmentID & DataCollectionStage - GD

enrollment_custom <- counties_all %>%
  left_join(ohio, by = c("county_prior" = "County")) %>%
  mutate(c_county_prior = if_else(county_prior == "--Outside of Ohio--", "0", Geocode)) %>%
  left_join(ohio, by = c("county_served" = "County")) %>%
  mutate(c_county_served = if_else(county_served == "--Outside of Ohio--", "0", Geocode.y)) %>%
  select(
    EnrollmentCustomID,
    PersonalID,
    EnrollmentID,
    InformationDate,
    DataCollectionStage,
    ExportID,
    c_county_served,
    c_county_prior
  ) #%>%
  # left_join(sp_entry_exit %>% select(entry_exit_id, provider_id),
  #         by = c("EnrollmentID" = "entry_exit_id"))


# enrollments_not_matching <- enrollment_custom %>%
#   filter(!EnrollmentID %in% c(enrollments_we_gave_them)) %>%
#   left_join(all_projects, by = c("provider_id" = "Legacy_ProgramID")) %>%
#   count(provider_id, Legacy_ProgramName) %>%
#   left_join(in_provider_group,
#             by = c("provider_id" = "Legacy_ProgramID",
#                    "Legacy_ProgramName"))


# 
# clients_not_matching <- enrollment_custom %>%
#   filter(!PersonalID %in% c(clients_we_gave_them)) %>%
#   left_join(sp_client %>% select(client_id, active), by = c("PersonalID" = "client_id")) %>%
#   left_join(in_provider_group, by = c("provider_id" = "Legacy_ProgramID"))

# Writing it out to csv ---------------------------------------------------

write_csv(enrollment_custom, here("data_to_Clarity/Enrollment_Custom.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(
      InformationDate = format.Date(InformationDate, "%Y-%m-%d")
    )
  
  fwrite(x,
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Enrollment_Custom")
