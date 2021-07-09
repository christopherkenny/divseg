#' Compute Absolute Clustering
#'
#' @param .data [tibble][tibble::tibble-package] with sf geometry
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with absolute clustering. Leave missing to return a vector.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept clustering
#' @examples
#' data("de_county")
#' ds_abs_clust(de_county, c(pop_white, starts_with('pop_')))
#' ds_abs_clust(de_county, c(pop_white, starts_with('pop_')), 'abs_clust')
ds_abs_clust <- function(.data, .cols, .name){

  if (!inherits(.data, 'sf')) {
    stop('`ds_abs_clust` requires `.data` to inherit sf for calculating areas.')
  }

  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  .data$.a <- calc_area(.data)
  .c <- calc_c(.data)
  .C <- sum(.c)

  sub <- .data %>%
    drop_sf() %>%
    dplyr::select(!!.cols)

  if (ncol(sub) <= 1) {
    stop('`.cols` refers to a single column')
  }

  sub <- sub %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.total = sum(dplyr::c_across()),
                  .x = dplyr::first(dplyr::cur_data())) %>%
    dplyr::ungroup()

  .X <- sum(sub$.x)
  .N <- nrow(sub)
  .N2 <- .N^2

  .xmat <- matrix(data = rep(sub %>% dplyr::pull(.data$.x), .N),
                  nrow = .N, ncol = .N, byrow = TRUE)
  sub$.xvec <- rowSums(.c * .xmat)
  .tmat <- matrix(data = rep(sub %>% dplyr::pull(.data$.total), .N),
                  nrow = .N, ncol = .N, byrow = TRUE)
  sub$.tvec <- rowSums(.c * .tmat)


  out <- sub %>%
    dplyr::mutate(!!.name := (sum((.data$.x/.X * .data$.xvec) ) - (.X/.N2*.C))/
                    (sum((.data$.x/.X ) * .data$.tvec) - (.X/.N2*.C)) ) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_abs_clust
#' @param ... arguments to forward to ds_abs_clust from abs_clust
#' @export
abs_clust <- function(..., .data = dplyr::cur_data_all()) {
  ds_abs_clust(.data = .data, ...)
}
