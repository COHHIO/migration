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
library(here)
library(readxl)
library(HMIS)
library(janitor)

# ServicePoint ------------------------------------------------------------

directory <- "data_to_Clarity"

if(ncol(read_csv(paste0(directory, "/Client.csv"))) == 36) {
  Client <-
    read_csv(paste0(directory, "/Client.csv"),
             col_types = "nccccncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216)) # our fake Client IDs are 5 and 4216
} else {
  Client <-
    read_csv(paste0(directory, "/Client.csv"),
             col_types = "ncncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216))
}

# Masking PII in the Client file (but not DOB) 

if(ncol(read_csv(paste0(directory, "/Client.csv"))) == 36)
{Client <- Client %>%
  mutate(
    # FirstName = case_when(
    #   NameDataQuality %in% c(8, 9) ~ "DKR",
    #   NameDataQuality == 2 ~ "Partial",
    #   NameDataQuality == 99 |
    #     is.na(NameDataQuality) |
    #     FirstName == "Anonymous" ~ "Missing",!(
    #       NameDataQuality %in% c(2, 8, 9, 99) |
    #         is.na(NameDataQuality) |
    #         FirstName == "Anonymous"
    #     ) ~ "ok"
    # ),
    # # LastName = NULL,
    # MiddleName = NULL,
    # NameSuffix = NULL,
    SSN = case_when(
      (is.na(SSN) & !SSNDataQuality %in% c(8, 9)) |
        is.na(SSNDataQuality) | SSNDataQuality == 99 ~ "Missing",
      SSNDataQuality %in% c(8, 9) ~ "DKR",
      (nchar(SSN) != 9 & SSNDataQuality != 2) |
        substr(SSN, 1, 3) %in% c("000", "666") |
        substr(SSN, 1, 1) == 9 |
        substr(SSN, 4, 5) == "00" |
        substr(SSN, 6, 9) == "0000" |
        SSNDataQuality == 2 |
        SSN %in% c(
          111111111,
          222222222,
          333333333,
          444444444,
          555555555,
          777777777,
          888888888,
          123456789
        ) ~ "Invalid",
      SSNDataQuality == 2 & nchar(SSN) != 9 ~ "Incomplete"
    )
  )

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))}

# this overwrites the raw Client.csv file on your computer with the final Client
# object as a security measure.

if(ncol(Client) == 33)
{write_csv(Client, paste0(directory, "/Client.csv"), append = FALSE)}

# CurrentLivingSituation <- 
#   read_csv(paste0(directory, "/CurrentLivingSituation.csv"),
#             col_types = "nnnTncnnnnncTTcTc") DON'T NEED YET

Disabilities <-
  read_csv(paste0(directory, "/Disabilities.csv"),
           col_types = "cnnDnnnnnnnnnnTTnTn")

EmploymentEducation <-
  read_csv(paste0(directory, "/EmploymentEducation.csv"),
           col_types = "cnnDnnnnnnTTnTn")

Exit <-
  read_csv(paste0(directory, "/Exit.csv"),
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")

Project <- 
  read_csv(paste0(directory, "/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

EnrollmentCoC <- 
  read_csv(paste0(directory, "/EnrollmentCoC.csv"), 
           col_types = "cncnnDcnTTnTn")

Enrollment <-
  read_csv(paste0(directory, "/Enrollment.csv"),
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

Funder <- 
  read_csv(paste0(directory, "/Funder.csv"),
           col_types = "nnnccDDTTcTn")

HealthAndDV <-
  read_csv(paste0(directory, "/HealthAndDV.csv"),
           col_types = "cnnDnnnnnnnDnTTnTn")

IncomeBenefits <- 
  read_csv(paste0(directory, "/IncomeBenefits.csv"),
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnTn")

Inventory <-
  read_csv(paste0(directory, "/Inventory.csv"),
           col_types = "nncnnnnnnnnnnnnDDTTcTn")

Organization <- 
  read_csv(paste0(directory, "/Organization.csv"),
           col_types = "ncncTTnTn")

ProjectCoC <- 
  read_csv(paste0(directory, "/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTn")


# Clarity -----------------------------------------------------------------

clarity_enrollment_count <- 
  read_csv("data_from_Clarity/enrollment_count_by_project.csv") %>%
  select("ProjectName" = 1, "clarity_count" = 2) %>%
  mutate(clarity_count = as.integer(clarity_count))

# Compare -----------------------------------------------------------------

sp_enrollment_count <- Enrollment %>%
  left_join(Project, by = "ProjectID") %>%
  count(ProjectName) 

diff_enrollment_count <- clarity_enrollment_count %>%
  full_join(sp_enrollment_count, by = "ProjectName") %>%
  filter(clarity_count != n)


# why are the numbers different in example --------------------------------

clarity_example <- 
  read_csv(
    "data_from_Clarity/Clark - IHN of Clark County - Norms Place - ES clients.csv"
    ) %>%
  select("FirstName" = 2, "LastName" = 3, "EntryDate" = 4) 

sp_example <- Enrollment %>%
  filter(ProjectID == 288) %>%
  select(PersonalID, EntryDate) %>%
  left_join(Client %>% 
              select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  select(-PersonalID)

diff_example <- sp_example %>% 
  anti_join(clarity_example, by = c("FirstName", "LastName", "EntryDate"))

duplications_sp <- sp_example %>%
  get_dupes() %>% unique()

duplications_clarity <- clarity_example %>%
  get_dupes() %>% unique()

