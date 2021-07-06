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

sp_directory <- "data_to_Clarity/initial_csv"

if(ncol(read_csv(paste0(sp_directory, "/Client.csv"))) == 36) {
  SP_Client <-
    read_csv(paste0(sp_directory, "/Client.csv"),
             col_types = "nccccncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216)) # our fake Client IDs are 5 and 4216
} else {
  SP_Client <-
    read_csv(paste0(sp_directory, "/Client.csv"),
             col_types = "ncncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216))
}

# Masking PII in the Client file (but not DOB) 

if(ncol(read_csv(paste0(sp_directory, "/Client.csv"))) == 36)
{SP_Client <- SP_Client %>%
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

SP_Client <- SP_Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))}

# this overwrites the raw Client.csv file on your computer with the final Client
# object as a security measure.

if(ncol(SP_Client) == 33)
{write_csv(SP_Client, paste0(sp_directory, "/Client.csv"), append = FALSE)}

# CurrentLivingSituation <- 
#   read_csv(paste0(sp_directory, "/CurrentLivingSituation.csv"),
#             col_types = "nnnTncnnnnncTTcTc") DON'T NEED YET

SP_Disabilities <-
  read_csv(paste0(sp_directory, "/Disabilities.csv"),
           col_types = "cnnDnnnnnnnnnnTTnTn")

SP_EmploymentEducation <-
  read_csv(paste0(sp_directory, "/EmploymentEducation.csv"),
           col_types = "cnnDnnnnnnTTnTn")

SP_Exit <-
  read_csv(paste0(sp_directory, "/Exit.csv"),
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")

SP_Export <- read_csv(paste0(sp_directory, "/Export.csv"),
                      col_types = c("iicccccccTDDcciii"))

SP_Project <- 
  read_csv(paste0(sp_directory, "/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

SP_EnrollmentCoC <- 
  read_csv(paste0(sp_directory, "/EnrollmentCoC.csv"), 
           col_types = "cncnnDcnTTnTn")

SP_Enrollment <-
  read_csv(paste0(sp_directory, "/Enrollment.csv"),
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

SP_Funder <- 
  read_csv(paste0(sp_directory, "/Funder.csv"),
           col_types = "nnnccDDTTcTn")

SP_HealthAndDV <-
  read_csv(paste0(sp_directory, "/HealthAndDV.csv"),
           col_types = "cnnDnnnnnnnDnTTnTn")

SP_IncomeBenefits <- 
  read_csv(paste0(sp_directory, "/IncomeBenefits.csv"),
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnTn")

SP_Inventory <-
  read_csv(paste0(sp_directory, "/Inventory.csv"),
           col_types = "nncnnnnnnnnnnnnDDTTcTn")

SP_Organization <- 
  read_csv(paste0(sp_directory, "/Organization.csv"),
           col_types = "ncncTTnTn")

SP_ProjectCoC <- 
  read_csv(paste0(sp_directory, "/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTn")


# Clarity -----------------------------------------------------------------

clarity_enrollment_count <- 
  read_csv("data_from_Clarity/enrollment_count_by_project.csv") %>%
  select("ProjectName" = 1, "clarity_count" = 2) %>%
  mutate(clarity_count = as.integer(clarity_count))

clarity_directory <- "data_from_Clarity"

if(ncol(read_csv(paste0(clarity_directory, "/Client.csv"))) == 36) {
  Clarity_Client <-
    read_csv(paste0(clarity_directory, "/Client.csv"),
             col_types = "nccccncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216)) # our fake Client IDs are 5 and 4216
} else {
  Clarity_Client <-
    read_csv(paste0(clarity_directory, "/Client.csv"),
             col_types = "ncncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216))
}

# Masking PII in the Client file (but not DOB) 

if(ncol(read_csv(paste0(clarity_directory, "/Client.csv"))) == 36)
{Clarity_Client <- Clarity_Client %>%
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

Clarity_Client <- Clarity_Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))}

# this overwrites the raw Client.csv file on your computer with the final Client
# object as a security measure.

if(ncol(Clarity_Client) == 33)
{write_csv(Clarity_Client, paste0(clarity_directory, "/Client.csv"), append = FALSE)}

# CurrentLivingSituation <- 
#   read_csv(paste0(clarity_directory, "/CurrentLivingSituation.csv"),
#             col_types = "nnnTncnnnnncTTcTc") DON'T NEED YET

Clarity_Disabilities <-
  read_csv(paste0(clarity_directory, "/Disabilities.csv"),
           col_types = "cnnDnnnnnnnnnnTTnTn")

Clarity_EmploymentEducation <-
  read_csv(paste0(clarity_directory, "/EmploymentEducation.csv"),
           col_types = "cnnDnnnnnnTTnTn")

Clarity_Exit <-
  read_csv(paste0(clarity_directory, "/Exit.csv"),
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")

Clarity_Export <- read_csv(paste0(clarity_directory, "/Export.csv"),
                           col_types = c("iicccccccTDDcciii"))

Clarity_Project <- 
  read_csv(paste0(clarity_directory, "/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

Clarity_EnrollmentCoC <- 
  read_csv(paste0(clarity_directory, "/EnrollmentCoC.csv"), 
           col_types = "cncnnDcnTTnTn")

Clarity_Enrollment <-
  read_csv(paste0(clarity_directory, "/Enrollment.csv"),
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

Clarity_Funder <- 
  read_csv(paste0(clarity_directory, "/Funder.csv"),
           col_types = "nnnccDDTTcTn")

Clarity_HealthAndDV <-
  read_csv(paste0(clarity_directory, "/HealthAndDV.csv"),
           col_types = "cnnDnnnnnnnDnTTnTn")

Clarity_IncomeBenefits <- 
  read_csv(paste0(clarity_directory, "/IncomeBenefits.csv"),
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnTn")

Clarity_Inventory <-
  read_csv(paste0(clarity_directory, "/Inventory.csv"),
           col_types = "nncnnnnnnnnnnnnDDTTcTn")

Clarity_Organization <- 
  read_csv(paste0(clarity_directory, "/Organization.csv"),
           col_types = "ncncTTnTn")

Clarity_ProjectCoC <- 
  read_csv(paste0(clarity_directory, "/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTn")

# Compare -----------------------------------------------------------------

sp_enrollment_count <- SP_Enrollment %>%
  left_join(SP_Project, by = "ProjectID") %>%
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

sp_example <- SP_Enrollment %>%
  filter(ProjectID == 288) %>%
  select(PersonalID, EntryDate) %>%
  left_join(SP_Client %>% 
              select(PersonalID, FirstName, LastName), 
            by = "PersonalID") %>%
  select(-PersonalID)

diff_example <- sp_example %>% 
  anti_join(clarity_example, by = c("FirstName", "LastName", "EntryDate"))

duplications_sp <- sp_example %>%
  get_dupes() %>% unique()

duplications_clarity <- clarity_example %>%
  get_dupes() %>% unique()


# Export Comparison -------------------------------------------------------

SP_Export$ExportStartDate == Clarity_Export$ExportStartDate
SP_Export$ExportEndDate == Clarity_Export$ExportEndDate

# all clients in the SP export that are NOT in the Clarity export
missing_clients <- anti_join(SP_Client, Clarity_Client, 
                             by = c("FirstName",
                                    "LastName",
                                    "SSN",
                                    "DOB"))

# Clarity can only take one CoC at a time so let's pretend that's the only problem
bos_missing_clients <- missing_clients %>%
  left_join(SP_EnrollmentCoC %>%
              select(PersonalID, CoCCode), by = "PersonalID") %>%
  filter(CoCCode == "OH-507")


