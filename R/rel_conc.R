#' Compute Relative Concentration
#'
#' @param .data [tibble][tibble::tibble-package] with sf geometry
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with relative concentration. Leave missing to return a vector.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept concentration
#' @examples
#' data('de_county')
#' ds_rel_conc(de_county, c(pop_black, starts_with('pop_')))
#' ds_rel_conc(de_county, c(pop_black, starts_with('pop_')), 'rel_conc')
ds_rel_conc <- function(.data, .cols, .name) {
  if (!inherits(.data, 'sf')) {
    stop('`ds_rel_conc` requires `.data` to inherit sf for calculating areas.')
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
    stop('`.cols` refers to a single column')
  }

  sub <- sub %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .total = sum(dplyr::c_across()),
      .x = dplyr::first(dplyr::cur_data()),
      .y = .data$.total - .data$.x
    ) %>%
    dplyr::ungroup()

  sub$.a <- calc_area(.data)

  .X <- sum(sub$.x)
  .A <- sum(sub$.a)
  .Y <- sum(sub$.y)
  .T <- sum(sub$.total)

  if (.X / .T > 0.5) {
    warning('First column indicated in `.cols` is the majority group. Values may be unstable.')
  }

  .n1_sum <- calc_n1_sum(sub, .X)
  .n2_sum <- calc_n2_sum(sub, .X)

  out <- sub %>%
    dplyr::mutate(!!.name := (sum(.data$.x * .data$.a / .X) / sum(.data$.y * .data$.a / .Y) - 1)
                  / ((.n1_sum / .n2_sum) - 1)) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>%
      dplyr::mutate(!!.name := out) %>%
      relocate_sf()
  } else {
    out
  }
}
