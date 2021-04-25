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
    description	= ProjectCommonName,
    ref_template = "", # ???
    availability_starrt	= OperatingStartDate,
    availability_end	= OperatingEndDate,
    status = 1,
    cross_agency = "", # ???
    ref_funding_source = "", # what is the data type here? does this change the granularity of the dataset?
    funding_source.funding_source_non_federal	= "", # ??? ^^
    funding_source.amount	= "", # ??? ^^
    ref_category = ProjectType,
    aff_res_proj = ResidentialAffiliation,
    aff_res_proj_ids	= "", # ???	does this also change granularity of the dataset
    program_applicability	= "", # ???
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
    programs.ref_target_b	= TargetPopulation,
    tracking_method	= TrackingMethod,
    ref_housing_type = HousingType,
    geocode = Geocode,
    hmis_participating_project = HMISParticipatingProject,
    public_listing = "", # ???
    allow_goals	= "",
    allow_autoservice_placement	= "",	
    eligibility_enabled	= "",	
    allow_history_link	= "",
    enable_assessments	= "",
    enable_notes	= "",
    prenable_files	= "",
    enable_charts	= "",
    enable_autoexit	= "",
    autoexit_duration	= "",	
    enable_cascade	= "",
    cascade_threshold	= "",
    enable_assessment_cascade	= "",
    assessment_cascade_threshold	= "",
    close_services	= "",
    enrollment_age_warning	= "",
    enrollment_age_warning_threshold	= "",
    all_client_forms_enabled	= "",
    added_date	= "",	
    last_updated	= ""
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

