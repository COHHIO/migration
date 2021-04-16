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

# To help associate Project-level data to AgencyIDs -----------------------

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

# Getting each User-Project connection ------------------------------------

COHHIO_admin_user_ids <- c(641, 835, 1041, 1239, 1563, 1624, 1628, 1868, 1698)

users_eda_groups <- read_xlsx("data_to_Clarity/RMisc2.xlsx", 
                              sheet = 15) %>%
  select(UserID, UserName, UserEmail, EDAGroupName)

eda_groups_providers <- read_xlsx("data_to_Clarity/RMisc2.xlsx",
                                  sheet = 16) %>%
  select(ProjectID, EDAGroup)

providers_users <- users_eda_groups %>%
  left_join(eda_groups_providers, by = c("EDAGroupName" = "EDAGroup")) %>%
  filter(!is.na(ProjectID) &
           !UserID %in% c(COHHIO_admin_user_ids)) %>%
  select(-EDAGroupName)

# Getting general shape of final file -------------------------------------
# id	name	members.first_name	members.last_name	email	ref_user_group	user_agencies.ref_agency 	ref_user_status	members.auto_suggest	ref_profile_screen_name	users.ref_auth_option	members.force_password_change	password	Confirm Password	members.is_warning	members.warning_days	members.last_policy_updated_date	Last Visited	user_agencies	Activation Code	Access Role (Additional Agency)	members.home_screen	user_groups.ref_license	"date"	added_date	last_updated

shaping_users <- providers_users %>%
  left_join(projects_orgs, by = "ProjectID") %>%
  filter(!is.na(AgencyID)) %>%
  separate(UserName, into = c("Last", "First"), sep = "[,]") %>%
  mutate(
    members.first_name = First,
    members.last_name = Last,
    email = UserEmail,
    ref_user_group = "",
    user_agencies.ref_agency = "",
    ref_user_status	 = "",
    members.auto_suggest = "",
    ref_profile_screen_name	 = "",
    users.ref_auth_option = "",
    members.force_password_change = "",
    password = "",
    members.is_warning = "",
    members.warning_days = "",
    members.last_policy_updated_date = "",
    user_agencies = "",
    members.home_screen = "",
    user_groups.ref_license = "",
    added_date = "",
    last_updated = ""
  )


