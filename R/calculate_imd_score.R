#' Calculate IMD score from domain scores
#'
#' You may exclude 1 or more domains by switching the relevant element of the
#'  output of [choose_domains] to `FALSE`. You may additionally supply your own
#'  vector of weights. See [imd_weights] or the IMD documentation for the
#'  default weight values.
#' Setting a domain weight to zero is equivalent to excluding the domain, and
#'  vice versa.
#'
#' @param scores_tbl A table containing transformed domain scores, as produced
#'  by [get_transformed_scores]
#' @param domains A logical vector, best produced by [choose_domains]
#' @param weights A numeric vector, as produced by [imd_weights] by default
#' @importFrom rlang .data
#' @export
calculate_imd_score <- function(
  scores_tbl,
  domains = choose_domains(),
  weights = imd_weights()
) {
  stopifnot(ncol(scores_tbl) == 11)
  init_cols <- c("lsoa21cd", "lsoa21nm", "lad24cd", "lad24nm")
  stopifnot(identical(colnames(scores_tbl)[seq(4)], init_cols))
  stopifnot(all(grepl("_score$", colnames(scores_tbl)[seq(5, 11)])))
  stopifnot(rlang::is_logical(domains))
  stopifnot(length(domains) == 7)
  stopifnot(length(weights) == 7)

  cols <- unname(which(domains)) + 4 # column numbers for domain scores
  weights <- weights[which(domains)]

  scores_tbl <- dplyr::select(scores_tbl, c(seq(4), tidyselect::all_of(cols)))
  apply_weight <- function(tbl, col, wt) {
    dplyr::mutate(tbl, dplyr::across(tidyselect::all_of(col), \(x) x * wt))
  }

  cols |>
    purrr::reduce2(weights, apply_weight, .init = scores_tbl) |>
    dplyr::mutate(
      imd_score = sum(dplyr::c_across(tidyselect::ends_with("score"))),
      .by = "lsoa21cd"
    ) |>
    dplyr::arrange(dplyr::desc(.data[["imd_score"]])) |>
    dplyr::mutate(
      imd_rank = dplyr::row_number(),
      imd_decile = as.factor(dplyr::ntile(n = 10))
    )
}


#' Helper function to select domains to include in calculation of IMD overall
#'
#' All domains are included by default
#' @param include_income Boolean, default `TRUE`. Include the Income Deprivation
#'  domain?
#' @param include_employment Boolean, default `TRUE`. Include the Employment
#'  Deprivation domain?
#' @param include_education Boolean, default `TRUE`. Include the Education,
#'  Skills and Training domain?
#' @param include_health Boolean, default `TRUE`. Include the Health
#'  Deprivation and Disability domain?
#' @param include_crime Boolean, default `TRUE`. Include the Crime domain?
#' @param include_barriers Boolean, default `TRUE`. Include the Barriers to
#'  Housing and Services domain?
#' @param include_environment Boolean, default `TRUE`. Include the
#'  Living Environment Deprivation domain?
#' @returns A logical vector
#' @export
choose_domains <- function(
  include_income = TRUE,
  include_employment = TRUE,
  include_education = TRUE,
  include_health = TRUE,
  include_crime = TRUE,
  include_barriers = TRUE,
  include_environment = TRUE
) {
  unlist(rlang::fn_fmls())
}

#' The per-domain weightings for the 2025 IMD
#'
#' In order: income, employment, education, health deprivation and disability,
#'  crime, barriers to housing and services, living environment deprivation
#' @returns A numeric vector of length 7
#' @export
imd_weights <- \() c(rep(0.225, 2), rep(0.135, 2), rep(0.093, 3))
