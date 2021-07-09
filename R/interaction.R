#' Compute Interaction Index
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with Interaction index. Leave missing to return a vector.
#' @param .comp Default is FALSE. FALSE returns the sum, TRUE returns the components.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept exposure
#' @examples
#' data('de_county')
#' ds_interaction(de_county, c(pop_white, starts_with('pop_')))
#' ds_interaction(de_county, starts_with('pop_'), 'interaction')
ds_interaction <- function(.data, .cols, .name, .comp = FALSE) {
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
    dplyr::mutate(.total = sum(dplyr::c_across()),
                  .x = dplyr::first(dplyr::cur_data()),
                  .y = .data$.total - .data$.x) %>%
    dplyr::ungroup()

  .X <- sum(sub$.x)

  out <- sub %>%
    rowwise_if(.comp) %>%
    dplyr::mutate(!!.name := sum((.data$.x/.X)*(.data$.y/.data$.total))) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}
