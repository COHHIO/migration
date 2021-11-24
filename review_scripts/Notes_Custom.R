devtools::load_all("../Rm_data")
Notes_custom <- list(sp = readr::read_csv("~/R/Contributor_Repos/COHHIO/migration/data_to_Clarity/Notes_Custom.csv"),
                        cl_client = cl_api$api$runLook(71807, resultFormat = "csv", queryParams = list(apply_vis = TRUE, limit = -1)),
                        cl_services = cl_api$api$runLook(71805, resultFormat = "csv", queryParams = list(apply_vis = TRUE, limit = -1))) 

Notes_custom$cl <- dplyr::full_join(Notes_custom$cl_client, Notes_custom$cl_services)
Notes_custom$cl_client <- NULL
Notes_custom$cl_services <- NULL
Notes_custom <- Notes_custom |> 
  purrr::map(~dplyr::group_by(.x, PersonalID))

purrr::map_lgl(unique(Notes_custom$sp$PersonalID), ~{
  pid <- .x
  all_notes <- purrr::map(Notes_custom, ~ {
    dplyr::filter(.x, PersonalID %in% pid) |>
      dplyr::select(dplyr::contains("Note")) |>
      unlist()
  })
  all(all_notes$cl %in% all_notes$sp)
})


                    