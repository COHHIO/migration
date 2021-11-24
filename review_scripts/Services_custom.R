Services_custom <- list(sp = readr::read_csv("~/R/Contributor_Repos/COHHIO/migration/data_to_Clarity/Services_Custom.csv"),
                        cl = cl_api$api$runLook(71173, resultFormat = "csv", queryParams = list(apply_vis = TRUE, limit = -1))) 
Services_custom <- Services_custom |> 
  purrr::map(~dplyr::mutate(.x, dplyr::across(dplyr::ends_with("ID"), .f = as.character))) |> 
  purrr::map(~dplyr::filter(.x, !PersonalID %in% as.character(c(15081L,
                                    15129L,
                                    30103L,
                                    154249L,
                                    154250L)))
)

.suffi <- c("_sp", "_cl")
cl_nms <- paste0("EnrollmentID", .suffi)
enrollment_map <- readr::read_csv("../migration/data_from_Clarity/Cohhio_Mig_Connection_Model_Imports_2021-11-02T1559.csv", col_types = c("cc")) |> rlang::set_names(cl_nms)

client_map <- readxl::read_xlsx("../migration/data_from_Clarity/cohhio_mig_connection_model imports 2021-11-02T1625.xlsx", col_types = c(rep("text",2))) |> rlang::set_names(paste0("PersonalID", rev(.suffi)))

Services <- readr::read_csv("~/R/Contributor_Repos/COHHIO/migration/data_to_Clarity/final_csv/Services.csv")


Services_summary <- purrr::imap(Services_custom, ~{
  by_col <- glue::glue("EnrollmentID_{.y}")
  dplyr::left_join(.x, enrollment_map, by = do.call(c, rlang::list2(
    "EnrollmentID" = by_col
  ))) |>
    dplyr::rename(!!by_col := "EnrollmentID") |> 
    dplyr::group_by(!!!purrr::map(cl_nms, rlang::sym)) |>
    dplyr::summarise(N = dplyr::n(),
                     Amt = sum(FAAmount, na.rm = TRUE))
})


cl_sp_summary <- rlang::exec(dplyr::left_join, !!!unname(Services_summary), by = cl_nms, suffix = .suffi)



extra_cl <- dplyr::filter(Services_summary$sp, is.na(EnrollmentID_cl)) |> 
  dplyr::left_join(Services_custom$sp |> dplyr::distinct(EnrollmentID, PersonalID), by = do.call(c, rlang::list2(!!cl_nms[1] := "EnrollmentID"))) |> 
  dplyr::left_join(client_map, by = c("PersonalID" = paste0("PersonalID", .suffi[1]))) |> 
  dplyr::rename(!!paste0("PersonalID", .suffi[1]) := "PersonalID")



not_diff <- cl_sp_summary |> 
  dplyr::filter(!is.na(EnrollmentID_sp)) |> 
  dplyr::group_by(!!rlang::sym(cl_nms[1])) |> 
  dplyr::summarise(N_sp = sum(N_sp),
                   N_cl = sum(N_cl),
                   Amt_sp = sum(Amt_sp),
                   Amt_cl = sum(Amt_cl)
                   ) |> 
  dplyr::left_join(dplyr::distinct(cl_sp_summary, !!!purrr::map(cl_nms, rlang::sym)))

Services_hud <- Services |> 
  dplyr::mutate(EnrollmentID = as.character(EnrollmentID)) |> 
  dplyr::group_by(EnrollmentID) |> 
  dplyr::summarise(N_hud = dplyr::n(),
                   Amt_hud = sum(FAAmount, na.rm = TRUE))

hud_diff <- dplyr::left_join(not_diff, 
                 Services_hud, by = c(EnrollmentID_sp = "EnrollmentID")) |> 
  tidyr::replace_na(list(N_hud = 0, Amt_hud = 0))

not_diff <- dplyr::mutate(hud_diff,
                          N_cl = N_cl - N_hud,
                          Amt_cl = Amt_cl - Amt_hud,
                          N_hud = NULL,
                          Amt_hud = NULL) 

actual_diff <- not_diff[!round(not_diff$Amt_cl) == round(not_diff$Amt_sp),] |> 
dplyr::left_join(dplyr::distinct(Services_custom$cl, PersonalID, EnrollmentID), by = do.call(c, rlang::list2(!!cl_nms[2] := "EnrollmentID"))) |> 
  dplyr::rename(!!paste0("PersonalID", .suffi[2]) := "PersonalID") |> 
  dplyr::filter(!is.na(EnrollmentID_sp))
actual_diff <- actual_diff |>
  dplyr::mutate(Amt_diff = abs(Amt_cl - Amt_sp)) |> 
  dplyr::filter(Amt_diff > mean(Amt_diff, na.rm = TRUE))

d2c <- "../migration/data_to_Clarity"
readr::write_csv(actual_diff, file = file.path(d2c, "Services_Custom_diffs.csv"))

sp_diff_o <- Services_custom$sp |>
  dplyr::filter(EnrollmentID %in% actual_diff$EnrollmentID_sp) |> 
  dplyr::left_join(dplyr::distinct(actual_diff, !!!purrr::map(cl_nms, rlang::sym)), by = do.call(c, rlang::list2("EnrollmentID" = cl_nms[1])))


sp_diff_o |> 
  dplyr::filter(EnrollmentID_cl == "189322") |> 
  readr::write_csv(file.path(d2c, "SC_diff_example.csv"))

readr::write_csv(sp_diff_o, file = file.path(d2c, "Services_Custom_diffs.csv"))

readr::write_csv(extra_cl, file = "../migration/data_to_Clarity/Services_Custom_missing_clients.csv")
