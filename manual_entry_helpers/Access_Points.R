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

source(here("reading_severance.R"))

Access_Points <- sp_provider %>% 
  filter(program_type_code == "Coordinated Entry (HUD)") %>%
  pull(provider_id)

AP_General <- sp_provider_service_geography_served_by_county %>%
  filter(provider_id %in% c(Access_Points)) %>%
  group_by(provider_id) %>%
  summarise(counties = toString(county)) %>%
  ungroup() %>%
  rename("general_service_area" = counties)

AP_Veteran <- sp_provider_service_code %>% 
  filter(provider_id %in% c(Access_Points) & 
           service_code == "FT-1000.9000") %>% 
  select(provider_id, "veteran_service_area" = service_area) 

AP_Youth <- sp_provider_service_code %>% 
  filter(provider_id %in% c(Access_Points) & 
           service_code == "RP-1400.8000-750") %>% 
  select(provider_id, "youth_service_area" = service_area)

AP_DV <- sp_provider_service_code %>% 
  filter(provider_id %in% c(Access_Points) & 
           service_code == "BH-1800.1500-100") %>% 
  select(provider_id, "dv_service_area" = service_area)

hours_website <- sp_provider %>%
  filter(provider_id %in% c(Access_Points)) %>%
  select(provider_id, hours, website_address)

access_point_manual <- AP_General %>%
  full_join(AP_Veteran, by = "provider_id") %>%
  full_join(AP_Youth, by = "provider_id") %>%
  full_join(AP_DV, by = "provider_id") %>%
  full_join(hours_website, by = "provider_id")

write_csv(access_point_manual, "random_data/ap_manual_data_entry.csv")








