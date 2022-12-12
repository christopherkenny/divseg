#' Compute Entropy Index
#'
#' @param .data [tibble][tibble::tibble-package]
#' @param .cols [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
#' Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
#' first column as first group and sum of other columns as second.
#' @param .name name for column with entropy index. Leave missing to return a vector.
#' @param .comp Default is FALSE. FALSE returns the sum, TRUE returns the components.
#'
#' @return a [tibble][tibble::tibble-package] or numeric vector if .name missing
#' @export
#'
#' @md
#' @concept evenness
#' @examples
#' data("de_county")
#' ds_entropy(de_county, c(pop_white, starts_with('pop_')))
#' ds_entropy(de_county, starts_with('pop_'), 'entropy')
ds_entropy <- function(.data, .cols, .name, .comp = FALSE){
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
    dplyr::mutate(.total = sum(dplyr::c_across(everything()))) %>%
    dplyr::ungroup()

  .T <- sum(sub$.total)
  .P <- sum(dplyr::first(sub))/.T
  .E <- .P * plog (1/.P) + (1 - .P) * plog (1/(1 - .P))

  out <- sub %>%
    dplyr::mutate(.p = pick_n(1)/.data$.total,
                  .e =  .data$.p * plog (1/.data$.p) + (1 - .data$.p) * plog (1/(1 - .data$.p))) %>%
    rowwise_if(.comp) %>%
    dplyr::mutate(!!.name := sum(.data$.total * (.E * .data$.e))/(.E * .T) ) %>%
    dplyr::pull(!!.name)

  if (ret_t) {
    .data %>% dplyr::mutate(!!.name := out) %>% relocate_sf()
  } else {
    out
  }
}

#' @rdname ds_entropy
#' @param ... arguments to forward to ds_entropy from entropy
#' @export
entropy <- function(..., .data = dplyr::pick(everything())) {
  ds_entropy(.data = .data, ...)
}
