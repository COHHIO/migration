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
library(janitor)

# From Clarity ------------------------------------------------------------

clarity_users <- 
  read_csv("data_from_Clarity/clarity_users.csv") %>%
  clean_names() %>%
  filter(!is.na(staff_primary_access_role))


# From ServicePoint -------------------------------------------------------

sp_users <- 
  read_csv("random_data/servicepoint_users.csv") %>%
  clean_names()

# Compare -----------------------------------------------------------------

users_created_after_file <- sp_users %>%
  filter(ymd_hms(users_audit_date_added) >= ymd("20210507"))

users_should_be_in_clarity <- sp_users %>%
  filter(
    users_status == "Yes" &
      users_role != "SUPPORT" &
      !users_username %in% c("estevens", "rmatthews", "auser", "cp5user",
                             "artsupport")
  )

users_not_in_Clarity <- users_should_be_in_clarity %>%
  select("username" = users_username) %>%
  anti_join(clarity_users %>% select("username" = staff_username), 
            by = "username") %>%
  left_join(users_should_be_in_clarity, by = c("username" = "users_username"))

users_not_in_ServicePoint <- clarity_users %>%
  anti_join(sp_users, by = c("staff_username" = "users_username")) 


