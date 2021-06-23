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
library(here)
library(lubridate)
library(readxl)
library(writexl)

# From Clarity ------------------------------------------------------------

agencies_programs_participating <- 
  read_csv("data_from_Clarity/AgenciesProgramsHMISParticipating.csv") %>%
  select(-X1) %>%
  filter(!is.na(AgencyName))

participating_only <- agencies_programs_participating %>%
  filter(IsHMISParticipatingProject == "Yes")

write_csv(participating_only, "random_data/participating.csv")


# From ServicePoint -------------------------------------------------------

orgs_providers <- 
  read_xlsx("random_data/RMisc2.xlsx", sheet = 3) %>%
  select(ProjectID, ProjectName, OrganizationName, UsesSP) %>%
  filter(!is.na(OrganizationName) &
           OrganizationName != "Coalition on Homelessness and Housing in Ohio(1)") %>%
  mutate(
    whereisit = str_locate(OrganizationName, "[(]"),
    OrganizationName = substr(OrganizationName, 1, whereisit - 1),
    FullName = paste(OrganizationName, ProjectName, sep = " - ")
    ) %>%
  select(-whereisit)

# Compare -----------------------------------------------------------------

not_in_Clarity <- orgs_providers %>%
  select(FullName) %>%
  anti_join(agencies_programs_participating %>% select(FullName), 
            by = "FullName") %>%
  left_join(orgs_providers, by = "FullName")

different_in_SP <- agencies_programs_participating %>%
  select(FullName) %>%
  anti_join(orgs_providers %>% select(FullName), by = "FullName") %>%
  left_join(agencies_programs_participating, by = "FullName")

write_xlsx(list(not_in_Clarity = not_in_Clarity,
                different_in_SP = different_in_SP), path = "random_data/diffs.xlsx")


