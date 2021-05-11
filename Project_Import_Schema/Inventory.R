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

library(janitor)
library(here)
library(data.table)

source(here("Project_Import_Schema/Programs.R"))

Inventory <-
  read_csv("data_to_Clarity/Inventory.csv",
           col_types = "nncnnnnnnnnnnnnDDTTcTn")

ProgramInventory <- Inventory %>%
  left_join(project_geocodes, by = "ProjectID") %>%
  mutate(
    id = InventoryID,	
    ref_program = ProjectID,	
    program_inventory.information_date = DateUpdated,
    program_inventory.start_date = InventoryStartDate,	
    program_inventory.end_date = InventoryEndDate,	
    program_inventory.coc = CoCCode,	
    program_inventory.ref_household_type = HouseholdType,	
    program_inventory.ref_bed_type = ESBedType,	
    program_inventory.availability = Availability,	
    program_inventory.is_bed_youth_veteran = if_else(
      YouthVetBedInventory > 0 & !is.na(YouthVetBedInventory), 1, 0),	
    program_inventory.bed_youth_veteran = replace_na(YouthVetBedInventory, 0),	
    program_inventory.is_bed_other_veteran = if_else(
      VetBedInventory > 0 & !is.na(VetBedInventory), 1, 0),	
    program_inventory.bed_other_veteran = replace_na(VetBedInventory, 0),	
    program_inventory.is_bed_other_youth = if_else(
      YouthBedInventory & !is.na(YouthBedInventory), 1, 0),	
    program_inventory.bed_other_youth = replace_na(YouthBedInventory, 0),	
    program_inventory.is_bed_non_dedicated = if_else(
      OtherBedInventory > 0 & !is.na(OtherBedInventory), 1, 0),
    program_inventory.bed_non_dedicated = replace_na(OtherBedInventory, 0),
    program_inventory.is_bed_ch_veteran = if_else(
      CHVetBedInventory > 0 & !is.na(CHVetBedInventory), 1, 0),
    program_inventory.bed_ch_veteran = replace_na(CHVetBedInventory, 0),
    program_inventory.is_bed_ch_youth = if_else(
      CHYouthBedInventory > 0 & !is.na(CHYouthBedInventory), 1, 0),
    program_inventory.bed_ch_youth = replace_na(CHYouthBedInventory, 0),	
    program_inventory.is_bed_ch_other = if_else(
      CHBedInventory > 0 & !is.na(CHBedInventory), 1, 0),	
    program_inventory.bed_ch_other = replace_na(CHBedInventory, 0),	
    program_inventory.bed_inventory = BedInventory,
    everything_adds_up = program_inventory.bed_ch_other +
      program_inventory.bed_ch_veteran +
      program_inventory.bed_ch_youth +
      program_inventory.bed_other_veteran +
      program_inventory.bed_other_youth +
      program_inventory.bed_youth_veteran +
      program_inventory.bed_non_dedicated ==
      program_inventory.bed_inventory,
    program_inventory.unit_inventory = UnitInventory,	
    program_inventory.bed_non_dedicated = if_else(
      everything_adds_up == TRUE,
      replace_na(OtherBedInventory, 0),
      BedInventory),
    added_date = DateCreated,	
    last_updated = DateUpdated
  ) %>%
  select(id:last_updated, -everything_adds_up) %>%
  relocate(program_inventory.bed_non_dedicated, 
           .after = program_inventory.is_bed_non_dedicated)


# DQ Checks ---------------------------------------------------------------

everything_adds_up <- ProgramInventory %>%
  filter(program_inventory.bed_ch_other +
           program_inventory.bed_ch_veteran +
           program_inventory.bed_ch_youth +
           program_inventory.bed_other_veteran +
           program_inventory.bed_other_youth +
           program_inventory.bed_youth_veteran +
           program_inventory.bed_non_dedicated !=
           program_inventory.bed_inventory)

write_csv(everything_adds_up, "random_data/not_adding_up.csv")

# Writing it out to csv ---------------------------------------------------

write_csv(BitfocusPrograms, here("data_to_Clarity/ProgramInventory.csv"))

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

fix_date_times("ProgramInventory")

