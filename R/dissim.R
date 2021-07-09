#' Compute Dissimilarity Index
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with dissimilarity index. Leave missing to return a vector.
#' @param .comp Default is FALSE. FALSE returns the sum, TRUE returns the components.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept evenness
#' @examples
#' data("de_county")
#' ds_dissim(de_county, c(pop_white, starts_with('pop_')))
#' ds_dissim(de_county, c(pop_white, starts_with('pop_')), .comp = TRUE)
#' ds_dissim(de_county, starts_with('pop_'), 'dissim')
ds_dissim <- function(.data, .cols, .name, .comp = FALSE){
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
  .P <- sum(dplyr::first(sub))/.T

  out <- sub %>%
    rowwise_if(.comp) %>%
    dplyr::mutate(!!.name := 0.5 * sum(.data$.total * abs(dplyr::first(dplyr::cur_data())/.data$.total - .P))
                  /(.T * .P * (1 - .P)) ) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_dissim
#' @param ... arguments to forward to ds_dissim from dissim
#' @export
dissim <- function(..., .data = dplyr::cur_data_all()) {
  ds_dissim(.data = .data, ...)
}
