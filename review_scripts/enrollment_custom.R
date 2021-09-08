#COHHIO_HMIS
#Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published
#by the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(lubridate)
library(tidyverse)
library(here)
library(readxl)

getting_counties <- read_xlsx(here("data/Enrollment_Custom.xlsx"), sheet = 1) %>%
  select(-DataCollectionStage) %>%
  mutate(InformationDate = as.Date(InformationDate, origin = "1899-12-30")) %>%
  filter(!PersonalID %in% c(5, 4216))

getting_names <- read_csv(here("final_sp_data/Client.csv")) %>%
  select(PersonalID, FirstName, LastName, DOB)

getting_ees <- read_csv(here("final_sp_data/Enrollment.csv")) %>%
  select(PersonalID, EnrollmentID, EntryDate) %>%
  left_join(read_csv(here("final_sp_data/Exit.csv")) %>%
              select(EnrollmentID, ExitDate), by = "EnrollmentID") %>%
  left_join(getting_names, by = "PersonalID")

sp_counties <- getting_counties %>%
  left_join(getting_ees, by = c("PersonalID", "EnrollmentID")) %>%
  filter(!is.na(EntryDate)) %>%
  select(-PersonalID, -EnrollmentID)

bf_counties <- read_csv(here("data/Clarity_Enrollment_Custom.csv"), 
                        col_types = cols()) %>%
  filter(!is.na(EntryDate) &
           (!is.na(CountyPrior) | !is.na(CountyServed)))

bf_rows <- nrow(bf_counties)
sp_rows <- nrow(sp_counties)

not_on_bf <- sp_rows - bf_rows

join_by_name_dob_ees <- bf_counties %>%
  anti_join(sp_counties, by = c("FirstName", "LastName", "EntryDate",
                                "ExitDate", "DOB"))



