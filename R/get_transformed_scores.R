#' Download the transformed domain scores for each LSOA
#'
#' @returns A tibble with columns for LSOA codes and names, LAD codes and names,
#'  and the transformed scores for each of the seven domains
#' @export
get_transformed_scores <- function() {
  fold <- "691ded670dcbf6343e9a2a6c"
  file <- "File_9_IoD2025_Transformed_Domain_Scores.xlsx"

  file.path(base_gov_url(), fold, file) |>
    openxlsx2::read_xlsx(sheet = "IoD25 Transformed Domain Scores") |>
    tibble::as_tibble() |>
    dplyr::rename_with(\(x) {
      c(
        "lsoa21cd",
        "lsoa21nm",
        "lad24cd",
        "lad24nm",
        "income_transformed_score",
        "employment_transformed_score",
        "education_transformed_score",
        "health_transformed_score",
        "crime_transformed_score",
        "barriers_transformed_score",
        "environment_transformed_score"
      )
    })
}

#' Download the overall IMD score for each LSOA
#'
#' @returns A tibble with columns for LSOA codes and names, LAD codes and names,
#'  and the overall score for each LSOA
#' @export
get_overall_scores <- function() {
  fold <- "691ded34513046b952c500bd"
  file <- "File_5_IoD2025_Scores_for_the_Indices_of_Deprivation.xlsx"

  file.path(base_gov_url(), fold, file) |>
    openxlsx2::read_xlsx(sheet = "IoD2025 Scores") |>
    tibble::as_tibble() |>
    dplyr::select(seq(5)) |>
    dplyr::rename_with(\(x) {
      c(
        "lsoa21cd",
        "lsoa21nm",
        "lad24cd",
        "lad24nm",
        "imd_score"
      )
    })
}
