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

april <- read_csv(here("data_to_Clarity/initial_csv/Inventory.csv")) %>%
  filter(ProjectID %in% c(final_project_ids))

august <- read_csv(here("data_to_Clarity/final_csv/Inventory.csv"))

# In April, I didn't have the ProviderGroup tightened down like I did by the time
# August came around. So filtering out inventory records not attached to projects
# that landed in the final export.

final_project_ids <- read_csv(here("data_to_Clarity/final_csv/Project.csv")) %>%
  pull(ProjectID) %>% unique()


# need to find all missing records, but the InventoryIDs don't match from one
# export to the other so I'm going to create an ID column on each df

august_adj <- august %>%
  unite(NewID,
        c(
          ProjectID,
          HouseholdType,
          InventoryStartDate,
          InventoryEndDate,
          UnitInventory,
          BedInventory
        ),
        remove = FALSE)

april_adj <- april %>%
  unite(NewID,
        c(
          ProjectID,
          HouseholdType,
          InventoryStartDate,
          InventoryEndDate,
          UnitInventory,
          BedInventory
        ),
        remove = FALSE)

not_in_april <- setdiff(august_adj$NewID, april_adj$NewID)

not_imported <- august_adj %>%
  filter(NewID %in% c(not_in_april))

# friendlier

project_names <- read_csv(here("data_to_Clarity/final_csv/Project.csv")) %>%
  select(ProjectID, ProjectName)

data_entry_friendly_missing <- not_imported %>%
  left_join(project_names, by = "ProjectID") %>%
  select(ProjectName, CoCCode:InventoryEndDate)

write_csv(data_entry_friendly_missing, "random_data/MissingInventory.csv")  

# need to find records that were different between april and august

different <- setdiff(april_adj$NewID, august_adj$NewID)

data_entry_friendly_different <- april_adj %>%
  filter(NewID %in% c(different)) %>%
  left_join(project_names, by = "ProjectID") %>%
  select(ProjectName, CoCCode:InventoryEndDate)

write_csv(data_entry_friendly_different, "random_data/DifferentInventory.csv")  

