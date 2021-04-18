

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(gt)


## Migration to BitFocus

art_data <-
  read_xlsx(here("random_data/OrganizationsBitFocus.xlsx"),
            sheet = 4) %>%
  filter(!is.na(ProjectID) & ProjectID != 1) %>% # dropping deleted providers and Provider ID 1
  mutate(
    MinEntry = as.Date(MinEntry, origin = "1899-12-30"),
    MaxExit = as.Date(MaxExit, origin = "1899-12-30"),
    OpenEEs = replace_na(OpenEEs, 0),
    operating = if_else(operating == "Yes", 1, 0),
    participating = if_else(participating == "Yes", 1, 0),
    ce_or_vash = if_else(
      str_detect(Project, "VASH") | (
        ptc == "Coordinated Entry (HUD)" &
          !ProjectID %in% c(213, 1989, 2025, 2041, 2044) # DV APs
      ),
      1,
      0
    ),
    
    obsolete_ptc = if_else(
      ptc %in% c(
        "Family Homelessness Prevention Pilot (FHPP)",
        "RETIRED (HUD)",
        "Homelessness Prevention",
        "Housing Stability Program (HSP)",
        "Direct Housing"
      ),
      1,
      0
    )
  )

### Organizations

org_level <- art_data %>%
  mutate(MinEntry = if_else(is.na(MinEntry), ymd("20040101"), MinEntry),
         MaxExit = if_else(is.na(MaxExit), ymd("20040101"), MaxExit),
         ce_or_vash = if_else(is.na(ce_or_vash), 0, ce_or_vash)) %>%
  group_by(Org) %>%
  summarise(
    minEntry = min(ymd(MinEntry)),
    maxExit = max(ymd(MaxExit)),
    openEEs = sum(OpenEEs),
    maxOperating = max(operating),
    maxParticipating = max(participating),
    CEorVASH = max(ce_or_vash)
  ) %>%
  ungroup()

### Not Migrating

not_migrating <- org_level %>%
  mutate(nodata = str_detect(minEntry, "/")) %>%
  filter((ymd(maxExit) <= ymd("20140501") | is.na(nodata)) &
           openEEs == 0 &
           maxParticipating == 0 &
           maxOperating == 0 &
           CEorVASH == 0) %>%
  unique() 

### Migrating Inactive
possibly_migrating <- org_level %>%
  anti_join(not_migrating, by = "Org")

migrating_inactive <- possibly_migrating %>%
  mutate(nodata = str_detect(minEntry, "/")) %>%
  filter(maxParticipating == 0 &
           CEorVASH == 0 &
           openEEs == 0)

### Migrating Active

migrating_active <- possibly_migrating %>%
  anti_join(migrating_inactive, by = "Org")

### Migrating

migrating <- rbind(migrating_active, migrating_inactive %>% select(-nodata))

### Maybe Add to Migrating Inactive

level2_providers <- art_data %>%
  filter(Org == "Coalition on Homelessness and Housing in Ohio(1)" &
           ptc == "Administrating Agency" &
           str_starts(Project, "zz", negate = TRUE)) %>%
  select("Org" = Project)

referral_orgs <-
  anti_join(level2_providers,
            art_data,
            by = "Org") %>%
  arrange(Org)







