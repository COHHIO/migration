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

source(here("Project_Import_Schema/Agencies.R"))

BitfocusServices <- Services %>%
  mutate(
    id = "",
    ref_agency = "",
    ref_program	= "",
    name = "",
    ref_category = "",
    ref_site = "",
    ref_site_type = "",
    status = "",
    ref_program_usage = "",
    added_date = "",
    service_items.name = "",
    servcie_items.si_cat1 = "",
    service_items.si_cat_2 = "",
    service_items.ref_delivery_type = "",
    service_items.is_service = "",
    service_items.attendance_history_assist = "",
    service_items.default_intake_model = "",
    service_items.roi_option = "",
    service_items.allow_groups = "",
    service_items.start_availability = "",
    service_items.end_availability = "",
    service_items.geocode = "",
    enabled_on_customer_portal = "",
    service_items.geolocation_is_enabled = "",
    service_items_housing.information_date = "",
    service_items_housing.ref_service_type = "",
    service_items_housing.ref_household_type = "",
    service_items_housing.inventory_type = "",
    service_items_housing.youth_inventory_type = "",
    service_items_housing.ref_bed_type = "",
    service_items_housing.availability = "",
    service_items_housing.beds_wo_children = "",
    service_items_housing.beds_with_children = "",
    service_items_housing.units_with_children = "",
    service_items_housing.reservation = "",
    service_items_housing.reservation_type = "",
    service_items_housing.reservation_term = "",
    service_items_housing.program_warning = "",
    service_items_housing.allow_referred_reservations = "",
    service_items.ref_funding = "",
    service_items.default_amount = "",
    service_items.expense_type = "",
    service_items.adjustable = "",
    service_items.charge_attendance = "",
    service_items.enable_time_tracking = "",
    service_items.default_tracking_hour = "",
    service_items.default_tracking_minute = "",
    service_items.time_tracking_type = "",
    service_items.time_tracking_adjustable = "",
    added_date = "",
    last_updated = ""

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

