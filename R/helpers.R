#' Drop sf conditionally
#' @keywords internal
drop_sf <- function(.data) {
  if (inherits(.data, 'sf')) {
    sf::st_drop_geometry(.data)
  } else {
    .data
  }
}

#' Moves sf column to last
#' @keywords internal
relocate_sf <- function(.data) {
  if (inherits(.data, 'sf')) {
    .data %>% dplyr::relocate(any_of(attr(.data, 'sf_column')), .after = last_col())
  } else {
    .data
  }
}

#' Pseudo Log
#' log(0) = 0
#' @keywords internal
plog <- function(x){
  dplyr::if_else(x == 0, 0, log(x))
}

#' Conditional rowwise()
#' @keywords internal
if_rowwise <- function(.data, cond) {
  if (cond) {
    .data %>% dplyr::rowwise()
  } else {
    .data
  }
}

#' Calculate conc sum from n1 to n
#' \deqn{\sum_{i=1}^{n1}\frac{t_i*a_i}{T_1}}
#' @keywords internal
calc_n1_sum <- function(.data, .X) {
  .data <- .data %>%
    dplyr::mutate(.n1_rank = dplyr::row_number(.data$.a)) %>%
    dplyr::arrange(.data$.n1_rank) %>%
    dplyr::mutate(.n1_cs = cumsum(.data$.total),
                  .cut = .data$.n1_cs - .X) %>%
    dplyr::filter(.data$.n1_rank <= min(.$.n1_rank[.$.cut > 0])) %>%
    dplyr::mutate(.ta = .data$.total * .data$.a)

  (.data %>% dplyr::pull(.data$.ta) %>% sum())/(.data %>% dplyr::pull(.data$.total) %>% sum())
}

#' Calculate conc sum from n2 to n
#' \deqn{\sum_{i=n2}^{n}\frac{t_i*a_i}{T_2}}
#' @keywords internal
calc_n2_sum <- function(.data, .X) {
  .data <- .data %>%
    dplyr::mutate(.n2_rank = dplyr::row_number(dplyr::desc(.data$.a))) %>%
    dplyr::arrange(.data$.n2_rank) %>%
    dplyr::mutate(.n2_cs = cumsum(.data$.total),
                  .cut = .data$.n2_cs - .X) %>%
    dplyr::filter(.data$.n2_rank <= min(.$.n2_rank[.$.cut > 0])) %>%
    dplyr::mutate(.ta = .data$.total * .data$.a)

  (.data %>% dplyr::pull(.data$.ta) %>% sum())/(.data %>% dplyr::pull(.data$.total) %>% sum())
}
