#' Compute Simpson Index
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with.
#' @param .name name for column with Simpson Index Leave missing to return a vector.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept div
#' @examples
#' data("de_county")
#' ds_inv_simpson(de_county, starts_with('pop_'))
#' ds_inv_simpson(de_county, starts_with('pop_'), 'blau')
ds_inv_simpson <- function(.data, .cols, .name){
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
    dplyr::mutate(!!.name := 1/sum((dplyr::select(dplyr::cur_data(), !!.cols)/.data$.total)^2)) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_inv_simpson
#' @param ... arguments to forward to ds_inv_simpson from inv_simpson
#' @export
inv_simpson <- function(..., .data = dplyr::cur_data_all()) {
  ds_inv_simpson(.data = .data, ...)
}
