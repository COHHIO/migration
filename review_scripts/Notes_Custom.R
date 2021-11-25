devtools::load_all("../Rm_data")
Notes_custom <- list(sp = readr::read_csv("~/R/Contributor_Repos/COHHIO/migration/data_to_Clarity/Notes_Custom.csv"),
                        cl_client = cl_api$api$runLook(71807, resultFormat = "csv", queryParams = list(apply_vis = TRUE, limit = -1)),
                        cl_services = cl_api$api$runLook(71805, resultFormat = "csv", queryParams = list(apply_vis = TRUE, limit = -1))) 

Notes_custom$cl <- dplyr::full_join(Notes_custom$cl_client, Notes_custom$cl_services)
Notes_custom$cl_client <- NULL
Notes_custom$cl_services <- NULL
Notes_custom <- Notes_custom |> 
  purrr::map( ~ {
    dplyr::ungroup(.x) |>
      dplyr::mutate(dplyr::across(dplyr::ends_with("ID"), ~as.character(.x)))
  })


.suffi <- c("_sp", "_cl")
cl_nms <- paste0("EnrollmentID", .suffi)
enrollment_map <- readr::read_csv("../migration/data_from_Clarity/Cohhio_Mig_Connection_Model_Imports_2021-11-02T1559.csv", col_types = c("cc")) |> rlang::set_names(cl_nms)

client_map <- readxl::read_xlsx("../migration/data_from_Clarity/cohhio_mig_connection_model imports 2021-11-02T1625.xlsx", col_types = c(rep("text",2))) |> rlang::set_names(paste0("PersonalID", rev(.suffi)))

Notes_custom <- purrr::imap(Notes_custom, ~{
  dplyr::left_join(.x, client_map, by = c(PersonalID = paste0("PersonalID_", .y)))
})
future::plan(future::cluster(workers = 4))
all_notes <- furrr::future_map(rlang::set_names(unique(Notes_custom$sp$PersonalID)), ~{
  sp_id <- .x
  cl_id <- unique(Notes_custom$sp[Notes_custom$sp$PersonalID == .x,][["PersonalID_cl"]])
  all_notes <- purrr::imap(Notes_custom, ~ {
    f_exp <- rlang::expr(PersonalID %in% !!rlang::sym(paste0(.y,"_id")))
    dplyr::filter(.x, !!f_exp) |>
      dplyr::select(dplyr::contains("Note")) |> 
      dplyr::select( - dplyr::ends_with("ID")) |>
      unlist() |>
      na.omit() |> 
      tm::removeWords(tm::stopwords()) |> 
      stringr::str_remove_all(paste0("[^[",paste0(letters, collapse = ""),toupper(paste0(letters, collapse = "")),"\\s]+]"))
  })
  out <- NULL
  if (!identical(length(all_notes$sp), length(all_notes$cl)))
    out <- list(sp_cl = setdiff(all_notes$sp, all_notes$cl),
    cl_sp = setdiff(all_notes$cl, all_notes$sp))
  out
}) |> purrr::compact()
future::plan(future::sequential())
which_missing <- purrr::map_lgl(all_notes, ~UU::is_legit(.x) && length(.x$sp_cl) > 0)
sp_join <- Notes_custom$sp |> 
  dplyr::filter(PersonalID %in% names(all_notes[which_missing])) |> 
  dplyr::select(PersonalID, Note, Date) |> 
  dplyr::mutate(Note = tm::removeWords(Note, tm::stopwords()) |> 
                  stringr::str_remove_all(paste0("[^[",paste0(letters, collapse = ""),toupper(paste0(letters, collapse = "")),"\\s]+]")))

missing_tbl <-
  tibble::tibble(
    PersonalID_sp = names(all_notes[which_missing]),
    Missing_sp_notes = purrr::map(all_notes[which_missing], "sp_cl"),
    Missing_cl_notes = purrr::map(all_notes[which_missing], "cl_sp")
  ) |>
  dplyr::left_join(client_map) |> 
  tidyr::unnest(cols = "Missing_sp_notes") |> 
  dplyr::rowwise() |> 
  dplyr::mutate(Is_missing = !substr(Missing_sp_notes, 0, 30) %in% substr(Missing_cl_notes, 0, 30)) |> 
  dplyr::filter(Is_missing) |> 
  dplyr::left_join(sp_join, by = c(PersonalID_sp = "PersonalID", Missing_sp_notes = "Note"))

missing_cl <- missing_tbl |>
  dplyr::mutate(Missing_sp = substr(stringr::str_remove_all(Missing_sp_notes, "[[:alnum:]]+(?<=com)"), 0, 30)) |> 
  dplyr::mutate(Is_missing = !UU::is_legit(agrep(Missing_sp, substr(unlist(Missing_cl_notes), 0, 30)))) |> 
  dplyr::filter(Is_missing)




