#' Download the transformed domain scores for each LSOA
#'
#' @returns A tibble with columns for LSOA codes and names, LAD codes and names,
#'  and the transformed scores for each of the seven domains
#' @export
get_transformed_scores <- function() {
  base <- "https://assets.publishing.service.gov.uk/media"
  fold <- "691ded670dcbf6343e9a2a6c"
  file <- "File_9_IoD2025_Transformed_Domain_Scores.xlsx"

  file.path(base, fold, file) |>
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
        "living_environment_transformed_score"
      )
    })
}
