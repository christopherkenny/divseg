#' Compute Reyni Entropy
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with.
#' @param .name name for column with Reyni entropy. Leave missing to return a vector.
#' @param q exponent parameter. Default 0. Can not be 1.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept div
#' @examples
#' data('de_county')
#' ds_reyni(de_county, starts_with('pop_'))
#' ds_reyni(de_county, starts_with('pop_'), 'reyni')
ds_reyni <- function(.data, .cols, .name, q = 0) {
  if (q == 1) {
    stop('`q` must not be 1.')
  }
  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  out <- .data |>
    drop_sf() |>
    dplyr::rowwise() |>
    dplyr::mutate(.total = sum(dplyr::c_across(!!.cols))) |>
    dplyr::mutate(!!.name := (1 / (1 - q)) * log(sum((dplyr::select(dplyr::across(everything()), !!.cols)
    / .data$.total)^q))) |>
    dplyr::pull(!!.name)

  if (ret_t) {
    .data |>
      dplyr::mutate(!!.name := out) |>
      relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_reyni
#' @param ... arguments to forward to ds_reyni from reyni
#' @export
reyni <- function(..., .data = dplyr::across(everything())) {
  ds_reyni(.data = .data, ...)
}
