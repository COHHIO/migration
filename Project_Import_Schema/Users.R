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
           !UserID %in% c(COHHIO_admin_user_ids, 2169)) %>%
  select(-EDAGroupName)

providers_users %>% 
  filter(str_detect(UserName, ",", negate = TRUE)) %>% 
  select(UserName) %>% 
  unique()

# Getting general shape of final file -------------------------------------

shaping_users <- providers_users %>%
  left_join(projects_orgs, by = "ProjectID") %>%
  filter(!is.na(AgencyID) & AgencyID != 1695) %>%
  mutate(AgencyID = as.numeric(AgencyID)) %>%
  select(-ProjectID) %>% unique() %>%
  group_by(UserID, UserName, UserEmail) %>%
  arrange(AgencyID) %>%
  summarise(NoAgencies = n(),
            SingleAgencyID = min(AgencyID),
            SecondaryAgencyIDs = if(NoAgencies > 1){
              toString(AgencyID[-1])
            } else{
              ""
            }) %>%
  ungroup() %>%
  separate(UserName, into = c("Last", "First"), sep = "[,]") %>%
  mutate(
    First = str_squish(First),
    Last = str_squish(Last),
    Last = case_when(
      First == "Lindsey" & Last == "Smith" ~ "Smith2",
      First == "Chris" & Last == "Myers" ~ "Myers2",
      First == "Melissa" & Last == "Wright" ~ "Wright2",
      TRUE ~ Last
    ),
    name = tolower(paste0(str_sub(First, start = 1L, end = 2L), Last)),
    members.first_name = First,
    members.last_name = str_remove(Last, "2"),
    email = UserEmail,
    ref_user_group = "Agency Staff",  
    user_agencies.ref_agency = SingleAgencyID,
    ref_user_status = 1, 
    members.auto_suggest = 1, 
    ref_profile_screen_name	 = "Agency Default", 
    users.ref_auth_option = 0, # 2FA
    members.force_password_change = 1,
    password = "changethislater", 
    members.is_warning = 1, # assessment due warning
    members.warning_days = "30 days", 
    members.last_policy_updated_date = "", # leave blank
    user_agencies = SecondaryAgencyIDs, # no data type or format specified
    members.home_screen = 1,
    user_groups.ref_license = 1,
    date = format.Date(today(), "%Y-%m-%d"), 
    added_date = format.Date(today(), "%Y-%m-%d %T"), 
    last_updated = format.Date(today(), "%Y-%m-%d %T") 
  ) %>%
  select(name:last_updated)

# Data Quality Check ------------------------------------------------------

if_else(shaping_users %>% mutate(username_len = nchar(name)) %>%
  select(username_len) %>%
  slice_min(username_len, n = 1, with_ties = FALSE) %>%
  pull() >= 4, "all good", "there's a username that's less than 4 characters")

duplicates <- shaping_users %>%
  select(email, name) %>%
  unique() %>%
  group_by(name) %>% 
  summarise(total = n()) %>%
  filter(total > 1)

duplicate_usernames <- duplicates %>%
  nrow()

if_else(duplicate_usernames == 0, "all good", 
        paste("there are", duplicate_usernames, "duplicate usernames"))

special_characters <- shaping_users %>%
  filter(str_detect(name, "-") |
           str_detect(name, "'") |
           str_detect(name, ",")) %>% 
  select(name) %>% unique() 

if_else(special_characters %>% nrow() == 0, "all good",
        paste("there are", 
              special_characters %>% nrow(),
              "usernames with special characters in them"))

