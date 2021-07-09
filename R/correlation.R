#' Compute Correlation Index
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with Correlation index. Leave missing to return a vector.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept exposure
#' @examples
#' data('de_county')
#' ds_correlation(de_county, c(pop_white, starts_with('pop_')))
#' ds_correlation(de_county, starts_with('pop_'), 'correlation')
ds_correlation <- function(.data, .cols, .name) {
  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  sub <- .data %>%
    drop_sf() %>%
    dplyr::select(!!.cols)

  if (ncol(sub) <= 1) {
    stop('.cols refers to a single column')
  }

  sub <- sub %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.total = sum(dplyr::c_across()),
                  .x = dplyr::first(dplyr::cur_data())) %>%
    dplyr::ungroup()

  .X <- sum(sub$.x)
  .T <- sum(sub$.total)
  .P <- sum(dplyr::first(sub)) / .T

  out <- sub %>%
    dplyr::mutate(!!.name := (sum((.data$.x/.X)*(.data$.x/.data$.total)) - .P)/(1 - .P)) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>%
      dplyr::mutate(!!.name := out) %>%
      relocate_sf()
  } else {
    out
  }
}
