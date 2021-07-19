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
library(janitor)

# Organizations -----------------------------------------------------------

organization_sp <- read_csv("data_to_Clarity/Organization.csv") %>%
  select(OrganizationName, VictimServicesProvider, DateCreated, DateUpdated)

organization_bf <- read_csv("data_from_Clarity/Organization.csv") %>%
  select(OrganizationName, VictimServicesProvider, DateCreated, DateUpdated)

differences <- base::setdiff(organization_sp, organization_bf)

# Projects ----------------------------------------------------------------

project_sp <- read_csv("data_to_Clarity/Project.csv") %>%
  select(-ProjectID, -OrganizationID, -UserID, -ExportID, -starts_with("Date"))
project_bf <- read_csv("data_from_Clarity/Project.csv") %>%
  select(-ProjectID, -OrganizationID, -UserID, -ExportID, -starts_with("Date"))

differences <- base::setdiff(project_sp, project_bf)

projects_not_in_clarity <- base::setdiff(
  project_sp$ProjectName %>% unique(), project_bf$ProjectName %>% unique()
)

projects_not_in_sp <- base::setdiff(
  project_bf$ProjectName %>% unique(), project_sp$ProjectName %>% unique()
)


# Inventory ---------------------------------------------------------------

inventory_sp <- read_csv("data_to_Clarity/Inventory.csv") 
inventory_bf <- read_csv("data_from_Clarity/Inventory.csv") 

# clarity hud csv export has 0 records in Inventory.csv


# Funding Source ----------------------------------------------------------

funding_sp <- read_csv("data_to_Clarity/Funder.csv") 
funding_bf <- read_csv("data_from_Clarity/Funder.csv")

# clarity hud csv export has 0 records in Funder.csv








