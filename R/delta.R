#' Compute Delta Index
#'
#' @param .data [tibble][tibble::tibble-package] with sf geometry
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with delta index. Leave missing to return a vector.
#' @param .comp Default is FALSE. FALSE returns the sum, TRUE returns the components.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept seg
#' @examples
#' data("de_county")
#' ds_delta(de_county, c(pop_white, starts_with('pop_')))
#' ds_delta(de_county, starts_with('pop_'), 'delta')
ds_delta <- function(.data, .cols, .name, .comp = FALSE){

  if (!inherits(.data, 'sf')) {
    stop('`ds_delta` requires `.data` to inherit sf for calculating areas.')
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
    dplyr::mutate(.x = dplyr::first(dplyr::cur_data())) %>%
    dplyr::ungroup()

  sub$.a <- as.numeric(sf::st_area(.data))

  .X <- sum(sub$.x)
  .A <- sum(sub$.a)

  out <- sub %>%
    if_rowwise(.comp) %>%
    dplyr::mutate(!!.name := 0.5 * sum(abs(.x/.X - .a/.A)) ) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}
