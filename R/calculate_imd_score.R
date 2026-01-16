#' Calculate IMD score from domain scores
#'
#' @param scores_tbl A table containing transformed domain scores, as produced
#'  by [get_transformed_scores]
#' @param domains A Boolean vector, best produced by [choose_domains]
#' @importFrom rlang .data
#' @export
calculate_imd_score <- function(scores_tbl, domains = choose_domains()) {
  stopifnot(ncol(scores_tbl) == 11)
  init_cols <- c("lsoa21cd", "lsoa21nm", "lad24cd", "lad24nm")
  stopifnot(identical(colnames(scores_tbl)[seq(4)], init_cols))
  stopifnot(all(grepl("_score$", colnames(scores_tbl)[seq(5, 11)])))
  stopifnot(all(rlang::is_bool(domains)))
  stopifnot(length(domains) == 7)
  cols <- which(domains) + 4 # col numbers for domains after initial 4 columns
  wts <- imd_weights()[which(domains)]
  scores_tbl <- dplyr::select(scores_tbl, c(seq(4), tidyselect::all_of(cols)))
  apply_weight <- function(tbl, col, wt) {
    dplyr::mutate(tbl, dplyr::across(col, \(x) x * wt))
  }
  cols |>
    purrr::reduce2(wts, apply_weight, .init = scores_tbl) |>
    dplyr::mutate(
      imd_score = sum(dplyr::c_across(tidyselect::ends_with("score"))),
      .by = "lsoa21cd"
    ) |>
    dplyr::arrange(dplyr::desc(.data[["imd_score"]])) |>
    dplyr::mutate(
      imd_rank = dplyr::row_number(),
      imd_decile = dplyr::ntile(n = 10),
      dplyr::across("imd_decile", as.factor)
    )
}


#' Helper function to select domains to include in calculation of IMD overall
#'
#' All domains are included by default
#' @param include_income Boolean, default `TRUE`. Include the Income domain?
#' @param include_employment Boolean, default `TRUE`. Include the Employment
#'  domain?
#' @param include_education Boolean, default `TRUE`. Include the Education
#'  domain?
#' @param include_health Boolean, default `TRUE`. Include the Health domain?
#' @param include_crime Boolean, default `TRUE`. Include the Crime domain?
#' @param include_barriers Boolean, default `TRUE`. Include the Barriers domain?
#' @param include_living_environment Boolean, default `TRUE`. Include the
#'  Living Environment domain?
#' @returns A Boolean vector
#' @export
choose_domains <- function(
  include_income = TRUE,
  include_employment = TRUE,
  include_education = TRUE,
  include_health = TRUE,
  include_crime = TRUE,
  include_barriers = TRUE,
  include_living_environment = TRUE
) {
  purrr::list_c(rlang::fn_fmls())
}

#' The per-domain weightings for the 2025 IMD
#' @keywords internal
#' @returns A numeric vector
imd_weights <- \() c(0.225, 0.225, 0.135, 0.135, rep(0.28 / 3, 3))
