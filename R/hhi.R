#' Compute Herfindahl-Hirshman Index
#'
#' This is equivalent to the Simpson Index.
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with.
#' @param .name name for column with HHI. Leave missing to return a vector.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept div
#' @examples
#' data("de_county")
#' ds_hhi(de_county, starts_with('pop_'))
#' ds_hhi(de_county, starts_with('pop_'), 'blau')
ds_hhi <- function(.data, .cols, .name){
  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_dissim'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  out <- .data %>% drop_sf() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.total = sum(dplyr::c_across(!!.cols))) %>%
    dplyr::mutate(!!.name := sum((dplyr::select(dplyr::cur_data(), !!.cols)/.data$.total)^2)) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_hhi
#' @param ... arguments to forward to ds_hhi from hhi
#' @export
hhi <- function(..., .data = dplyr::cur_data_all()) {
  ds_hhi(.data = .data, ...)
}
