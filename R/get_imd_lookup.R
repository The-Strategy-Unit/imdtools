#' Import ranks and deciles for all English LSOAs for the 2025 IMD
#'
#' @returns A tibble with columns for LSOA codes and names, LAD codes and names,
#'  and IMD ranks and deciles
#' @export
get_imd_lookup <- function() {
  fold <- "691dece32c6b98ecdbc500d5"
  file <- "File_1_IoD2025_Index_of_Multiple_Deprivation.xlsx"

  file.path(base_gov_url(), fold, file) |>
    openxlsx2::read_xlsx(sheet = "IMD25") |>
    tibble::as_tibble() |>
    dplyr::rename_with(\(x) {
      c(
        "lsoa21cd",
        "lsoa21nm",
        "lad24cd",
        "lad24nm",
        "imd_rank",
        "imd_decile"
      )
    }) |>
    dplyr::mutate(dplyr::across(tidyselect::starts_with("imd"), as.integer)) |>
    dplyr::mutate(dplyr::across("imd_decile", as.factor))
}

base_gov_url <- \() "https://assets.publishing.service.gov.uk/media"
