#' Compute Gini Index
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with gini index. Leave missing to return a vector.
#' @param .comp Default is FALSE. FALSE returns the sum, TRUE returns the components.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept evenness
#' @examples
#' data('de_county')
#' ds_gini(de_county, c(pop_white, starts_with('pop_')))
#' ds_gini(de_county, starts_with('pop_'), 'gini')
ds_gini <- function(.data, .cols, .name, .comp = FALSE) {
  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  sub <- .data |>
    drop_sf() |>
    dplyr::select(!!.cols)

  if (ncol(sub) <= 1) {
    stop('.cols refers to a single column')
  }

  sub <- sub |>
    dplyr::rowwise() |>
    dplyr::mutate(.total = sum(dplyr::c_across(everything()))) |>
    dplyr::ungroup()

  .T <- sum(sub$.total)
  .P <- sum(sub[[1]]) / .T

  pmat <- matrix(rep(sub[[1]], nrow(sub)), ncol = nrow(sub))
  pmat <- abs(pmat - t(pmat))
  tmat <- crossprod(t(sub$.total) / .T)

  out <- sub |>
    dplyr::mutate(.tp = rowSums(tmat * pmat)) |>
    rowwise_if(.comp) |>
    dplyr::mutate(!!.name := 0.5 * sum(.data$.tp) / (.T^2 * .P * (1 - .P))) |>
    dplyr::pull(!!.name)

  if (ret_t) {
    .data |>
      dplyr::mutate(!!.name := out) |>
      relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_gini
#' @param ... arguments to forward to ds_gini from gini
#' @export
gini <- function(..., .data = dplyr::across(everything())) {
  ds_gini(.data = .data, ...)
}
