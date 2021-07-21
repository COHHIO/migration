
# mapping document scratchings

# Third tab ---------------------------------------------------------------

original <- read_csv(here("random_data/mapping_migration_site.csv"))

new <- read_csv(here("random_data/mapping_live_site.csv"))

getting_old_data_attached <- new %>%
  left_join(
    original,
    by = c(
      "ProjectID",
      "ProjectName",
      "RecordType",
      "TypeProvided",
      "SubTypeProvided",
      "ReferralOutcome"
    )
  )

minus <- getting_old_data_attached %>%
  filter(is.na(ProgramID))

write_csv(minus, here("random_data/service_mapping_not_done_previously.csv"))


# First tab ---------------------------------------------------------------

from_clarity <- 
  read_csv(here("random_data/sp_to_clarity_project_mapping_final.csv")) %>%
  select("Legacy_OrganizationName" = 1, 
         "Legacy_OrganizationID" = 2, 
         "Legacy_ProgramName" = 3,
         "Legacy_ProgramID" = 4,
         "Clarity_AgencyID" = 5,
         "Clarity_AgencyName" = 6,
         "Clarity_ProgramID" = 7,
         "Clarity_ProgramName" = 8)

testing <- from_clarity %>%
  left_join(id_cross, by = c("Legacy_OrganizationID", 
                             "Legacy_ProgramID", 
                             "Clarity_AgencyID",
                             "Clarity_ProgramID")) %>%
  filter(Legacy_ProgramName.x != Legacy_ProgramName.y) %>%
  select(Legacy_ProgramName.x, Legacy_ProgramName.y)



