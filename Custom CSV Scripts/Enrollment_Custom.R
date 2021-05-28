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
library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)

from_ART1 <- read_xlsx("random_data/Enrollment_Custom_ART.xlsx", sheet = 1) %>%
  mutate(InformationDate = as.Date(InformationDate, origin = "1899-12-30"))

from_ART2 <- read_xlsx("random_data/Enrollment_Custom_ART.xlsx", sheet = 2) %>%
  mutate(InformationDate = as.Date(InformationDate, origin = "1899-12-30"))

from_ART <- rbind(from_ART1, from_ART2) %>%
  rename("county_served" = CountyServed, "county_prior" = CountyPrior) %>%
  mutate(ExportID = as.numeric(format.Date(today(), "%Y%m%d")),
         EnrollmentCustomID = row_number()) %>%
  relocate(EnrollmentCustomID, .before = "PersonalID")

# there's no duplicate combinations of EnrollmentID & DataCollectionStage - GD

# Writing it out to csv ---------------------------------------------------

write_csv(from_ART, here("data_to_Clarity/Enrollment_Custom.csv"))

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
