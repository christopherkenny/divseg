#' Compute Diversity
#'
#' This is equivalent to perplexity.
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with.
#' @param .name name for column with diversity. Leave missing to return a vector.
#' @param q exponent parameter. Default 0. Can not be 1.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept div
#' @examples
#' data('de_county')
#' ds_diversity(de_county, starts_with('pop_'))
#' ds_diversity(de_county, starts_with('pop_'), 'diversity')
ds_diversity <- function(.data, .cols, .name, q = 1) {
  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  out <- .data %>%
    drop_sf() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.total = sum(dplyr::c_across(!!.cols))) %>%
    dplyr::mutate(!!.name :=  sum((dplyr::select(dplyr::pick(everything()), !!.cols)
                                                      / .data$.total)^q)^(1 / (1 - q)) ) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>%
      dplyr::mutate(!!.name := out) %>%
      relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_diversity
#' @param ... arguments to forward to ds_diversity from diversity
#' @export
diversity <- function(..., .data = dplyr::pick(everything())) {
  ds_diversity(.data = .data, ...)
}
