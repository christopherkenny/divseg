#' Compute Atkinson b Index
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with Atkinson b index. Leave missing to return a vector.
#' @param b Default 0.5. Exponent parameter b, where 0 <= b <= 1.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept evenness
#' @examples
#' data('de_county')
#' ds_atkinson(de_county, c(pop_white, starts_with('pop_')))
#' ds_atkinson(de_county, starts_with('pop_'), 'atkinson')
ds_atkinson <- function(.data, .cols, .name, b = 0.5) {
  if (min(b) < 0 || max(b) > 1) {
    stop('`b` must be 0 <= b <= 1.')
  }
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
    dplyr::mutate(.total = sum(dplyr::c_across())) %>%
    dplyr::ungroup()

  .T <- sum(sub$.total)
  .P <- sum(dplyr::first(sub)) / .T

  out <- sub %>%
    dplyr::mutate(.p = dplyr::first(dplyr::cur_data()) / .data$.total) %>%
    dplyr::mutate(!!.name := 1 - (.P / (1 - .P)) *
                    abs((1 / (.P * .T)) *
                          sum((1 - .data$.p)^(1 - b) * .data$.p^b * .data$.total)
                        )^(1 / (1 - b))) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>%
      dplyr::mutate(!!.name := out) %>%
      relocate_sf()
  } else {
    out
  }
}
