#' Compute Distance Decay Isolation
#'
#' @param .data [tibble][tibble::tibble-package] with sf geometry
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with distance decay isolation. Leave missing to return a vector.
#' @param .comp Default is FALSE. FALSE returns the sum, TRUE returns the components.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept clustering
#' @examples
#' data('de_county')
#' ds_dd_isolation(de_county, c(pop_black, starts_with('pop_')))
#' ds_dd_isolation(de_county, c(pop_black, starts_with('pop_')), 'dd_isolation')
ds_dd_isolation <- function(.data, .cols, .name, .comp = FALSE) {
  if (!inherits(.data, 'sf')) {
    stop('`ds_dd_isolation` requires `.data` to inherit sf for calculating areas.')
  }

  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  .data$.a <- calc_area(.data)

  sub <- .data |>
    drop_sf() |>
    dplyr::select(!!.cols)

  if (ncol(sub) <= 1) {
    stop('`.cols` refers to a single column')
  }

  sub <- sub |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .total = sum(dplyr::c_across(everything())),
      .x = pick_n(1),
      .y = .data$.total - .data$.x
    ) |>
    dplyr::ungroup()

  .X <- sum(sub$.x)
  .Y <- sum(sub$.y)

  .k <- calc_k(.data, sub[['.total']])


  .sum_kxt <- sum((.k * sub[['.x']]) / sub[['.total']], na.rm = TRUE)

  out <- sub |>
    rowwise_if(.comp) |>
    dplyr::mutate(!!.name := sum((.data$.x / .X) * .sum_kxt)) |>
    dplyr::pull(!!.name)

  if (ret_t) {
    .data |>
      dplyr::mutate(!!.name := out) |>
      relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_dd_isolation
#' @param ... arguments to forward to ds_dd_isolation from dd_isolation
#' @export
dd_isolation <- function(..., .data = dplyr::across(everything())) {
  ds_dd_isolation(.data = .data, ...)
}
