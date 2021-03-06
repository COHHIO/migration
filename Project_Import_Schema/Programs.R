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
library(data.table)

source(here("Project_Import_Schema/Agencies.R"))
source(here("Project_Import_Schema/Funding_Sources.R"))

agencies <- read_csv("frozen/Agencies.csv") %>% pull(id)

stray_projects <- c(
  77, 85,95, 113, 154, 171, 254, 313, 339, 381, 382, 416, 530, 533, 570, 634, 
  760, 766, 767, 876, 884, 889, 915, 916, 927, 928, 929, 930, 1123,1230, 1231,
  1242, 1265, 1322, 1326, 1395, 1561, 1641, 1645, 1646, 1649,1650, 1657, 1698, 
  1699, 1716, 1743, 1831, 1838, 1910, 2090, 2100, 2317, 2319, 2322,2332, 2333,
  2335, 2336, 2339, 2352, 2358, 2372, 2430, 2435, 2454, 2456, 2458, 2460,2462,
  2465,2466, 2468, 2470, 2473,2475, 2476
)

mismatched_addresses <- c(
  50, 68, 77, 85, 95, 97, 113, 154, 171, 254, 313, 331, 339, 349, 381, 382, 415,
  416, 426, 453, 459, 530, 533, 544, 570, 584, 634, 713, 724, 746, 747, 749, 760,  
  766, 767, 854, 876, 884, 889, 915, 916, 919, 920, 923, 924, 927, 928, 929, 930,
  962, 966, 971, 986, 987, 1018, 1019, 1024, 1097, 1123, 1230, 1231, 1242, 1265, 
  1322, 1326, 1336, 1337, 1364, 1365, 1395, 1397, 1418, 1421, 1561, 1566, 1576, 
  1581, 1601, 1628, 1641, 1645, 1646, 1649, 1650, 1657, 1698, 1699, 1716, 1743, 
  1831, 1838, 1847, 1848, 1882, 1883, 1910, 1922, 1949, 1974, 1984, 1986, 1997, 
  2002, 2021, 2090, 2100, 2149, 2161, 2189, 2317, 2319, 2322, 2332, 2333, 2335, 
  2336, 2339, 2352, 2358, 2372, 2423, 2424, 2430, 2435, 2454, 2456, 2458, 2460, 
  2462, 2465, 2466, 2468, 2470, 2473, 2475, 2476)

funding_sources <- read_csv("frozen/FundingSources.csv") %>% pull(id)

funder_buckets <- Funder %>%
  mutate(
    bucket = case_when(
      ProjectID %in% c(550, 780, 2369) ~ "GPD/HCHV",
      ProjectID == 2126 ~ "YHDP",
      ProjectID %in% c(2342, 2446) ~ "RHY",
      Funder %in% c(1:7, 44, 49) ~ "CoC",
      Funder %in% c(8:11, 47) |
        Funder == 46 & OtherFunder == "ODSA" ~ "ESG",
      Funder %in% c(22:26) ~ "RHY",
      Funder == 21 ~ "PATH",
      Funder %in% c(13:19, 48) ~ "HOPWA",
      Funder == 20 ~ "VASH",
      Funder == 43 ~ "YHDP",
      Funder == 46 &
        OtherFunder %in% c("ODH", "Ohio Department of Health- Youth Initiative") ~
        "ODH",
      Funder == 33 ~ "SSVF",
      Funder %in% c(37:42, 45, 27) ~ "GPD/HCHV",
      TRUE ~ "Other"
    ),
    bucket = if_else(bucket %in% c("Other", "CoC", "ESG"), "CoC/ESG", bucket)
  ) %>%
  select(ProjectID, bucket) %>% unique()

project_descriptions <- read_csv("data_to_Clarity/project_descriptions.csv")

funder_columns <- Funder %>%
  left_join(hud_specs, by = c("Funder" = "ReferenceNo")) %>%
  left_join(projects_orgs, by = "ProjectID") %>%
  mutate(
    ref_agency = AgencyID,
    funding_source = Funder,
    funding_source_non_federal = case_when(
      OtherFunder == "Bezos Day One" ~ 1,
      OtherFunder == "CDBG" ~ 2,
      OtherFunder == "church" ~ 3,
      OtherFunder %in% c("Community", "community") ~ 4,
      OtherFunder == "ODH" ~ 5,
      OtherFunder == "ODSA" ~ 6,
      OtherFunder == "OHFA" ~ 7,
      OtherFunder == "Ohio Department of Health- Youth Initiative" ~ 8,
      OtherFunder == "OSDA" ~ 9,
      OtherFunder == "Pandemic Emergency Fund" ~ 10,
      OtherFunder == "TANF" ~ 11,
      OtherFunder %in% c("Unknown", "unknown") ~ 12,
      OtherFunder == "CSBG" ~ 13,
      OtherFunder == "Local" ~ 14,
      OtherFunder == "OHFA ERA" ~ 15,
      OtherFunder == "OVW Transitional Housing" ~ 16,
      OtherFunder == "United Way" ~ 17
    ), # comes from https://docs.google.com/spreadsheets/d/1NZLRcv4m57HBKnME-Bcqy__zKD2q-WIPnEqYYesh88U/edit?usp=sharing
    amount = 0) %>%
  select(ProjectID, funding_source, funding_source_non_federal, amount) %>%
  group_by(ProjectID) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = n,
              values_from = c(funding_source, funding_source_non_federal, amount))

project_geocodes <- ProjectCoC %>%
  select(ProjectID, Geocode) %>%
  unique()

BitfocusPrograms <- Project %>%
  left_join(project_geocodes, by = "ProjectID") %>%
  left_join(funder_buckets, by = "ProjectID") %>%
  left_join(Addresses %>% select(ProjectID, 
                                 Address1, 
                                 Address2,
                                 OrganizationName), 
            by = "ProjectID") %>%
  filter(OrganizationID %in% c(agencies)) %>%
  left_join(funder_columns, by = "ProjectID") %>%
  left_join(project_descriptions, by = "ProjectID") %>%
  left_join(provider_extras %>%
              select(ProjectID, "longProjectName" = ProjectName,
                     "longProjectAKA" = ProjectAKA), by = "ProjectID") %>%
  filter(!is.na(longProjectName)) %>%
  mutate(
    id = ProjectID,
    ref_agency = OrganizationID,
    name = longProjectName,
    alias = longProjectAKA,
    description	= Description,
    ref_template = case_when(
      ProjectType == 14 ~ 27, # CE
      bucket == "SSVF" ~ 21,
      bucket == "YHDP" ~ 24,
      bucket == "PATH" ~ 18,
      bucket == "VASH" ~ 22,
      bucket == "ODH" ~ 26,
      bucket == "GPD/HCHV" ~ 23,
      bucket == "RHY" ~ 19,
      bucket == "HOPWA" ~ 20,
      bucket == "CoC/ESG" & ProjectType %in% c(12:13) ~ 25,
      bucket == "CoC/ESG" & !ProjectType %in% c(12:13) ~ 15,
      TRUE ~ 0
    ),
    availability_start = OperatingStartDate,
    availability_end = OperatingEndDate,
    status = 1,
    cross_agency = 0, # will be rarely used, so if we need it, set up manually
    funding_source_1 = if_else(is.na(funding_source_1), 34, funding_source_1),
    ref_category = ProjectType,
    aff_res_proj = if_else(ProjectType != 6, 0, ResidentialAffiliation),
    aff_res_proj_ids	= case_when(id == 1765 ~ "548, 774",
                                 id == 2176 ~ "1693",
                                 TRUE ~ ""), # hard coded since we have so few
    program_applicability	= ProjectType,
    continuum_project	= ContinuumProject,
    geolocations.address = case_when(
      is.na(Address2) & !is.na(Address1) ~ Address1,
      is.na(Address1) &
        !is.na(Address2) ~ Address2,!is.na(Address1) &
        !is.na(Address2) ~ paste(Address1, Address2),
      is.na(Address1) &
        is.na(Address2) ~ paste("Confidential -",
                                OrganizationName)
    ),
    programs.ref_target_b	= if_else(is.na(TargetPopulation),
                                    4, TargetPopulation),
    tracking_method	= TrackingMethod,
    ref_housing_type = if_else(is.na(HousingType), 1, HousingType),
    geocode = Geocode,
    hmis_participating_project = HMISParticipatingProject,
    public_listing = 2, # 2 = Public -> any agency can refer to this project
    allow_goals	= 0, # based on GB's answer, seems like this should be off
    allow_autoservice_placement	= 0, # default
    eligibility_enabled	= 1, # from C009
    allow_history_link = 0, # not needed bc there can't be stray srvcs anyway
    enable_assessments = 1, # based on GB's answer, seems like this should be off
    enable_notes = 1, # from C009
    enable_files = 1, # from C009
    enable_charts	= 1, # from C009
    enable_autoexit	= 0, # suggested that this is off until after migration
    autoexit_duration	= "", # maybe this should be NULL?
    enable_cascade = 1, # from C009 ("Cascade Enrollment data")
    cascade_threshold	= case_when(
      ProjectType %in% c(3, 9) ~ 365, # PSH
      ProjectType == 2 ~ 120, # TH
      TRUE ~ 30
    ), 
    enable_assessment_cascade	= 1, # from C009
    assessment_cascade_threshold = case_when(
      ProjectType %in% c(3, 9) ~ 365, # PSH
      ProjectType == 2 ~ 120,
      TRUE ~ 30
    ), # choices: 1,2,3,4,5,6,7,14,21,30,60,90,120,180,365
    close_services = 1, # from C009
    enrollment_age_warning = 1, # from C009
    enrollment_age_warning_threshold = 17, # recommended (by BF) value
    all_client_forms_enabled = 0, # from C009
    added_date = today(),
    last_updated = today()
  ) %>%
  relocate(funding_source_1:amount_3, .after = cross_agency) %>%
  select(id:last_updated)

mismatched_addresses <- BitfocusPrograms %>% 
  filter(id %in% c(mismatched_addresses))

# Writing it out to csv ---------------------------------------------------

write_csv(BitfocusPrograms, here("data_to_Clarity/Programs.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(
      added_date = format.Date(added_date, "%Y-%m-%d %T"),
      last_updated = format.Date(last_updated, "%Y-%m-%d %T")
    )
  
  fwrite(x,
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Programs")
