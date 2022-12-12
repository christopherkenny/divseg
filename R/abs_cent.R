#' Compute Absolute Centralization
#'
#' @param .data [tibble][tibble::tibble-package] with sf geometry
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with absolute centralization. Leave missing to return a vector.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept centralization
#' @examples
#' data("de_county")
#' ds_abs_cent(de_county, c(pop_white, starts_with('pop_')))
#' ds_abs_cent(de_county, c(pop_white, starts_with('pop_')), 'abs_cent')
ds_abs_cent <- function(.data, .cols, .name){

  if (!inherits(.data, 'sf')) {
    stop('`ds_abs_cent` requires `.data` to inherit sf for calculating areas.')
  }

  .cols <- rlang::enquo(.cols)

  if (missing(.name)) {
    .name <- 'v_index'
    ret_t <- FALSE
  } else {
    ret_t <- TRUE
  }

  .a <- calc_area(.data)
  .A <- sum(.a)
  .data$.dist <- calc_dist_centroid(.data)
  .data <- .data %>% dplyr::arrange(.data$.dist)


  sub <- .data %>%
    drop_sf() %>%
    dplyr::select(!!.cols)

  if (ncol(sub) <= 1) {
    stop('`.cols` refers to a single column')
  }

  sub <- sub %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.x = pick_n(1)) %>%
    dplyr::ungroup()

  .X <- sum(sub[['.x']])

  sub <- sub %>%
    dplyr::mutate(.Xi = cumsum(.data$.x)/.X,
                  .Ai = cumsum(.a)/.A,
                  .Xi_min1 = dplyr::lag(.data$.Xi, default = 0),
                  .Ai_min1 = dplyr::lag(.data$.Ai, default = 0))

  out <- sub %>%
    dplyr::mutate(!!.name := sum(.data$.Xi_min1*.data$.Ai) -
                    sum(.data$.Xi*.data$.Ai_min1)) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_abs_cent
#' @param ... arguments to forward to ds_abs_cent from abs_cent
#' @export
abs_cent <- function(..., .data = dplyr::cur_data_all()) {
  ds_abs_cent(.data = .data, ...)
}
