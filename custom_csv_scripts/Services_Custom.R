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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the=
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.



# from ServicePoint:
source("reading_severance.R")

# from Clarity live (may need updating): "Project Services and Service Items"
  cl_api <- clarity.looker::clarity_api$new("../Rm_data/inst/auth/Looker.ini")
  
    clarity_service_items <- readr::read_csv("data_from_Clarity/Project Services and Service Items 2021-10-11T1016.csv")
  clarity_service_items <- cl_api$api$runLook(65622, resultFormat = "csv", queryParams = list(limit = -1, apply_vis = T))
  
  clarity_service_items <- clarity_service_items |>
    dplyr::mutate(
      ServiceItemName = dplyr::case_when(
        ServiceItemName %in% c("Case management", "Case management services") ~
          "Case Management",
        ServiceItemName == "Moving costs" ~ "Moving Cost Assistance",
        ServiceItemName == "Utility payments" ~ "Utility Payments",
        ServiceItemName %in% c("Security deposit", "Security deposits") ~
          "Security Deposit",
        ServiceItemName %in% c("Utility deposit", "Utility deposits") ~
          "Utility Deposit",
        TRUE ~ ServiceItemName
      )
    ) |>
    dplyr::filter(!is.na(Clarity_ProgramID) & !is.na(ServiceItemID))
  
  all_service_items <- clarity_service_items |>
    dplyr::select(ServiceItemName) |>
    unique()
  
  nrow(all_service_items) == 130 # YOU WANT TRUE! If FALSE, check service_translator
  
  # connector
  service_translator <- dplyr::tibble(
    sp_code = c(
      "PH-1000",
      "BH-3800.7000",
      "BH-3800.7250",
      "BV-8900.9300",
      "BV-8900.9150",
      "BH-1800.8500-300",
      "BH-3800.5150"
    ),
    sp_desc = c(
      "Case/Care Management",
      "Rent Payment Assistance",
      "Rental Deposit Assistance",
      "Utility Service Payment Assistance",
      "Utility Deposit Assistance",
      "Homeless Motel Vouchers",
      "Moving Expense Assistance"
    ),
    clarity_desc = c(
      "Case Management",
      "Rental Assistance",
      "Security Deposit",
      "Utility Payments",
      "Utility Deposit",
      "Motel and Hotel Vouchers",
      "Moving Cost Assistance"
    )
  )
  
  # from ServicePoint:
  
  cohort_services <- sp_need_service |>
    dplyr::left_join(
      sp_provider |> dplyr::select(provider_id, program_type_code),
      by = c("provide_provider_id" = "provider_id")
    ) |>
    dplyr::filter(active == TRUE &
                    provide_start_date > lubridate::ymd("20140601") &
                    client_id %in% c(client_cohort) &
                    is.na(hopwa_service_type) &
                    is.na(path_service_type) &
                    is.na(rhy_service_type) &
                    is.na(ssvf_service_type) &
                    is.na(ssvf_fin_assist_type) &
                    ((
                      program_type_code == "Homelessness Prevention (HUD)" &
                        code %in% c(
                          "PH-1000",
                          "BH-3800.7000",
                          "BH-3800.7250",
                          "BV-8900.9150",
                          "BV-8900.9300"
                        )
                    ) |
                      (
                        program_type_code == "PH - Rapid Re-Housing (HUD)" &
                          code %in% c(
                            "PH-1000",
                            "BH-3800.7000",
                            "BH-3800.7250",
                            "BV-8900.9150",
                            "BV-8900.9300",
                            "BH-3800.5150",
                            "BH-1800.8500-300"
                          )
                      ) |
                      (
                        program_type_code == "Other (HUD)" &
                          code %in% c(
                            "BH-3800.7000",
                            "BH-3800.7250",
                            "BV-8900.9150",
                            "BV-8900.9300"
                          )
                      ) |
                      (
                        program_type_code == "Emergency Shelter (HUD)" &
                          code == "BH-1800.8500-300"
                      )
                    ))
  
  # mostly ServicePoint + connector data:
  
  prep <- cohort_services |>
    dplyr::select(
      "ServicesID" = need_service_id,
      "PersonalID" = client_id,
      "Legacy_ProgramID" = provide_provider_id,
      # "GroupID" = group_id,
      # "NeedServiceGroupID" = need_service_group_id,
      # "HouseholdID" = household_id,
      "DateProvided" = provide_start_date,
      "DateCreated" = date_added,
      "DateUpdated" = date_updated,
      "UserID" = user_creating_id,
      "sp_code" = code
    ) |>
    dplyr::left_join(clarity_projects_orgs,
                     by = "Legacy_ProgramID") |>
    dplyr::relocate(Clarity_AgencyID, .after = PersonalID) |>
    # start checking that Services are connected to an Enrollment
    dplyr::left_join(sp_entry_exit |>
                       dplyr::filter(active == TRUE) |>
                       dplyr::select(client_id, entry_exit_id, entry_date, exit_date, provider_id),
                     by = c("PersonalID" = "client_id")) |>
    dplyr::mutate(
      exit_adjust = dplyr::if_else(is.na(exit_date), lubridate::now(), exit_date),
      enrollment_interval = lubridate::interval(entry_date, exit_adjust)
    ) |>
    dplyr::filter(lubridate::`%within%`(DateProvided, enrollment_interval) &
                    provider_id == Legacy_ProgramID) |>
    dplyr::rename("EnrollmentID" = entry_exit_id) |>
    dplyr::relocate(EnrollmentID, .after = Clarity_AgencyID) |>
    dplyr::select(ServicesID:Clarity_ProgramName)
  
  service_items <- prep |>
    dplyr::left_join(service_translator, by = "sp_code") |>
    dplyr::left_join(
      clarity_service_items,
      by = c("clarity_desc" = "ServiceItemName",
             "Clarity_ProgramName",
             "Clarity_ProgramID")
    ) |>
    unique()
  
  projects_not_done <- service_items |> 
    dplyr::filter(is.na(ServiceItemID)) |> 
    dplyr::count(Legacy_ProgramName, sp_desc, clarity_desc) |>
    dplyr::mutate(ok = dplyr::case_when(
      stringr::str_detect(Legacy_ProgramName, "SSVF") |
        stringr::str_detect(Legacy_ProgramName, "YHDP") |
        stringr::str_detect(Legacy_ProgramName, "RHY") |
        stringr::str_detect(Legacy_ProgramName, "ODH") ~ "ok", 
      TRUE ~ "not ok"))
  
  services_not_connecting <- service_items |> 
    dplyr::filter(is.na(ServiceItemID) &
                    stringr::str_detect(Legacy_ProgramName, "SSVF", negate = TRUE) &
                    stringr::str_detect(Legacy_ProgramName, "YHDP", negate = TRUE) &
                    stringr::str_detect(Legacy_ProgramName, "RHY", negate = TRUE) &
                    stringr::str_detect(Legacy_ProgramName, "ODH", negate = TRUE))
  
  cat(services_not_connecting |> nrow() == 0) # you want true
  
  services_not_coming <- service_items |>
    dplyr::filter(is.na(ServiceItemID) &
                    (
                      stringr::str_detect(Legacy_ProgramName, "SSVF") |
                        stringr::str_detect(Legacy_ProgramName, "YHDP") |
                        stringr::str_detect(Legacy_ProgramName, "RHY") |
                        stringr::str_detect(Legacy_ProgramName, "ODH")
                    ))
  
  writexl::write_xlsx(services_not_coming,
                      "random_data/IntegratedServicesServices.xlsx")
  
  # because these were supposed to have been entered as VA/HHS Services ^^
  
  # dropping all the services that did not connect to any ServiceItemID
  
  prep_2 <- service_items |>
    dplyr::filter(!is.na(ServiceItemID)) 
  
  # getting funding sources ready
  # TODO There are 44 of these
  # unique(sp_need_service_group_fund$provider_service_funding_source_id) |> sort()
  # and 44 of these
  hud_funding_sources <- readr::read_csv("random_data/HUDSpecs.csv") |>
  dplyr::filter(DataElement == "FundingSource") |>
    dplyr::mutate(ReferenceNo = as.numeric(ReferenceNo))
  
  # Should they be joined on this instead?
  
  # TODO
  #  
  clarity_funding_sources <- funding_sources <- readxl::read_xlsx("data_from_Clarity/cohhio_connection_model agencies 2021-09-27T1409.xlsx")
  # setdiff(program_funds_clarity$ClarityFundingSourceID,
  # clarity_funding_sources$`Agency Funding Sources ID`)
  # ind <- setdiff(clarity_funding_sources$`Agency Funding Sources ID`,
  #         program_funds_clarity$ClarityFundingSourceID)
  # 
  # clarity_funding_sources$`Agency Funding Sources Fund Name`[ind] |> unique() 
  # 
  # # no longer matching fundingsources
  funds_deleted <- clarity_funding_sources[1:97,]$`Agency Funding Sources ID`
  
  fund_amounts <- sp_need_service_group_fund |>
    dplyr::filter(active == TRUE &
                    date_added >= lubridate::ymd("20140601") &
                    source != "Diversion" &
                    last_action %in% c("Modified", "Submitted")) |>
    dplyr::select(need_service_group_id, cost, source) |>
    dplyr::left_join(sp_need_service |>
                       dplyr::select(need_service_group_id,
                                     need_service_id), by = "need_service_group_id") |>
    dplyr::select("ServicesID" = need_service_id, source, "FAAmount" = cost)
  
  # has duplicates, maybe needs another ID column or a date column?
  changes_to_sources <- readr::read_csv("random_data/funding_source_exceptions.csv")
  
   # program_funds_clarity <- readr::read_csv("data_from_Clarity/Program Funding Sources 2021-10-13T2011.csv") |>
   #   dplyr::filter(Active == "Yes")
  
  program_funds_clarity <- cl_api$api$runLook(65790, resultFormat =  "csv", queryParams = list(limit = -1, apply_vis = T))  |>
    dplyr::filter(Active == "Yes")
  
  sp_service_fund_types <- c(fund_amounts$source, changes_to_sources$NewSource) |> 
    unique() |> 
    sort()
  clarity_funds <-
     program_funds_clarity  |> 
    dplyr::mutate(
      FundingOtherID = dplyr::case_when(FundingSourceID == 46 ~ FundingOtherID),
      FundingOther = dplyr::case_when(FundingSourceID == 46 ~ FundingOther)
    )
 
  # TODO Re-write with easier to read line-by-line correspondence between sp_service_fund_types
  fund_translator <- dplyr::tibble(
    SPServiceFundingSource = c(sp_service_fund_types),
    funding_source = c(
      # TODO 46 isn't a mapped value in the hUD funding sources nor the other_funding_source_crosswalk
      46, 46, 46, 46, 3, 46, 46, 46, 47, 9, 46, 46, 46, 15, 16, 17, 46, 2, 2, 46, 
      46, 46, 46, 46, 46, 46, 2, 46, 46, 2, 10, 46, 46, 46, 33, 43, 46
    ),
    funding_source_other = c(
      14, 14, 14, 2, NA, 18, 13, 1, NA, NA, 0, 0, 6, 0, 0, 0, 6, NA, NA, 14, 5, 6, 7,
      7, 15, 10, NA, 19, 14, NA, NA, 6, 11, 14, NA, NA, 14
    )
  ) |>
    dplyr::left_join(hud_funding_sources, by = c("funding_source" = "ReferenceNo")) |>
    dplyr::left_join(other_funding_source_crosswalk, 
                     by = c("funding_source_other" = "ReferenceNo")) |>
    dplyr::left_join(clarity_funds |> dplyr::select(Clarity_ProgramID, FundingSourceID, FundingOtherID), by = c(funding_source = "FundingSourceID", funding_source_other = "FundingOtherID")) |> 
    dplyr::select(-DataElement, -DataElementID) |> 
    dplyr::distinct()
  
  
  
  prep_3 <- prep_2 |>
    dplyr::left_join(fund_amounts, by = "ServicesID") |>
    dplyr::left_join(changes_to_sources, by = c(
      "Clarity_ProgramID", "Clarity_ProgramName", "source"
    )) |>
    dplyr::mutate(source = dplyr::if_else(is.na(NewSource), source, NewSource))
    
  
  prep_4 <- prep_3 |>
    dplyr::select(
      # Remove all columns not included in prep_4
      - dplyr::starts_with("Legacy"),
      - Clarity_AgencyName,
      - Clarity_ProgramName,
      - sp_desc,
      - clarity_desc,
      - NewSource
    ) |> 
      dplyr::left_join(fund_translator, by = c("source" = "SPServiceFundingSource", "Clarity_ProgramID")) |> 
    dplyr::left_join(
      dplyr::distinct(clarity_funds, FundingSourceID, FundingOtherID, Clarity_ProgramID, .keep_all  = T) |>
        dplyr::select(-Clarity_AgencyName, - Clarity_AgencyID, -Clarity_ProgramName, - FundName, - FundingAmount, - Active),
      by = c(
        "Clarity_ProgramID",
        "funding_source" = "FundingSourceID",
        "funding_source_other" = "FundingOtherID"
      )
    )
  
  Custom_Services <- prep_4 |>
    dplyr::mutate(ServicesID = dplyr::row_number()) |>
    dplyr::select(
      ServicesID,
      PersonalID,
      "AgencyID" = Clarity_AgencyID,
      EnrollmentID,
      ServiceItemID,
      DateProvided,
      "FundingSourceID" = ClarityFundingSourceID,
      FundingSource,
      FundingOther,
      FAAmount,
      DateCreated,
      DateUpdated,
      UserID
    ) |> 
    dplyr::mutate(
      DateProvided = format.Date(DateProvided, "%Y-%m-%d"),
      DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
      DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T")
    )
  
  
  nrow(Custom_Services)
  
  
  # checking for services missing legit funding sources  
  
  missings <- prep_3 |>
    dplyr::filter(!is.na(source) & is.na(ClarityFundingSourceID)) |>
    dplyr::count(Clarity_ProgramID, Clarity_ProgramName, source, Description, OtherFundingSource)
  
  writexl::write_xlsx(missings, "random_data/funding_source_setup.xlsx")
  
  # Writing it out to csv ---------------------------------------------------
  
  readr::write_csv(Custom_Services, "data_to_Clarity/Services_Custom.csv")
  
  fix_date_times <- function(file) {
    cat(file, sep = "\n")
    x <- readr::read_csv(paste0("data_to_Clarity/", file, ".csv"),
                         col_types = readr::cols())  |>
      dplyr::mutate(
        DateProvided = format.Date(DateProvided, "%Y-%m-%d"),
        DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
        DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T")
      )
    
    data.table::fwrite(x,
                       paste0("data_to_Clarity/", file, ".csv"),
                       logical01 = TRUE)
  }
  
  fix_date_times("Services_Custom")
