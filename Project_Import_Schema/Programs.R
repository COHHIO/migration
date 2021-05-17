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

project_geocodes <- ProjectCoC %>%
  select(ProjectID, Geocode) %>%
  unique()

BitfocusPrograms <- Project %>%
  left_join(project_geocodes, by = "ProjectID") %>%
  mutate(
    id = ProjectID,
    ref_agency = OrganizationID,
    name = ProjectName,
    alias = ProjectCommonName,
    description	= "",
    ref_template = "", # will need discussion, decisions, setup
    availability_start = OperatingStartDate,
    availability_end = OperatingEndDate,
    status = 1,
    cross_agency = 0, # will be rarely used, so if we need it, set up manually
    ref_funding_source = "", 
    funding_source.funding_source_non_federal	= "", # ??? ^^
    funding_source.amount	= 0, # since we don't collect this in SP, setting to 0
    ref_category = ProjectType,
    aff_res_proj = if_else(ProjectType != 6, 0, ResidentialAffiliation),
    aff_res_proj_ids	= case_when(
      id == 1765 ~ "548, 774",
      id == 2176 ~ "1693",
      TRUE ~ ""
    ), 
    program_applicability	= ProjectType, 
    continuum_project	= ContinuumProject,
    geolocations.address = case_when(
      is.na(Address2) & !is.na(Address1) ~ Address1,
      is.na(Address1) &
        !is.na(Address2) ~ Address2,
      !is.na(Address1) &
        !is.na(Address2) ~ paste(Address1, Address2),
      is.na(Address1) &
        is.na(Address2) ~ paste("Confidential -",
                                OrganizationName)
    ),
    programs.ref_target_b	= TargetPopulation,
    tracking_method	= TrackingMethod,
    ref_housing_type = HousingType,
    geocode = Geocode,
    hmis_participating_project = HMISParticipatingProject,
    public_listing = 2, # 2 = Public -> any agency can refer to this project
    allow_goals	= 1,
    allow_autoservice_placement	= 0, # default
    eligibility_enabled	= 0, # default	
    allow_history_link	= 0, # default
    enable_assessments	= 0, # default
    enable_notes	= 0, # default
    prenable_files	= 0, # default
    enable_charts	= 0, # default
    enable_autoexit	= 0, # default
    autoexit_duration	= 0, # maybe this should be NULL?
    enable_cascade	= 0, # default
    cascade_threshold	= 0,
    enable_assessment_cascade	= 0, # default
    assessment_cascade_threshold = 0,
    close_services = 1, # default
    enrollment_age_warning = 1,
    enrollment_age_warning_threshold = 17,
    all_client_forms_enabled = 1,
    added_date = today(),	
    last_updated = today()
  ) %>%
  select(id:last_updated)

# Writing it out to csv ---------------------------------------------------

write_csv(BitfocusPrograms, here("data_to_Clarity/Programs.csv"))

fix_date_times <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols())  %>%
    mutate(added_date = format.Date(added_date, "%Y-%m-%d %T"),
           last_updated = format.Date(last_updated, "%Y-%m-%d %T"))
  
  fwrite(x, 
         here(paste0("data_to_Clarity/", file, ".csv")),
         logical01 = TRUE)
}

fix_date_times("Programs")

