#' Compute Relative Clustering
#'
#' @param .data [tibble][tibble::tibble-package] with sf geometry
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with relative clustering. Leave missing to return a vector.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept clustering
#' @examples
#' data("de_county")
#' ds_rel_clust(de_county, c(pop_black, starts_with('pop_')))
#' ds_rel_clust(de_county, c(pop_black, starts_with('pop_')), 'rel_clust')
ds_rel_clust <- function(.data, .cols, .name){

  if (!inherits(.data, 'sf')) {
    stop('`ds_rel_clust` requires `.data` to inherit sf for calculating areas.')
  }

  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  .data$.a <- calc_area(.data)

  sub <- .data %>%
    drop_sf() %>%
    dplyr::select(!!.cols)

  if (ncol(sub) <= 1) {
    stop('`.cols` refers to a single column')
  }

  sub <- sub %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.total = sum(dplyr::c_across()),
                  .x = pick_n(1),
                  .y = .data$.total - .data$.x) %>%
    dplyr::ungroup()

  .X <- sum(sub$.x)
  .Y <- sum(sub$.y)

  .pxx <- calc_pgg(.data, sub %>% dplyr::pull(.data$.x))
  .pyy <- calc_pgg(.data, sub %>% dplyr::pull(.data$.y))

  out <- sub %>%
    dplyr::mutate(!!.name := (.pxx/.pyy) - 1) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_rel_clust
#' @param ... arguments to forward to ds_rel_clust from rel_clust
#' @export
rel_clust <- function(..., .data = dplyr::cur_data_all()) {
  ds_rel_clust(.data = .data, ...)
}
