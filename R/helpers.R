#' Drop sf conditionally
#' @keywords internal
#' @noRd
drop_sf <- function(.data) {
  if (inherits(.data, 'sf')) {
    sf::st_drop_geometry(.data)
  } else {
    .data
  }
}

#' Moves sf column to last
#' @keywords internal
#' @noRd
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
#' @noRd
plog <- function(x){
  dplyr::if_else(x == 0, 0, log(x))
}

#' Conditional rowwise()
#' @keywords internal
#' @noRd
rowwise_if <- function(.data, cond) {
  if (cond) {
    .data %>% dplyr::rowwise()
  } else {
    .data
  }
}

#' Calculate Area
#' Ensure it's converted to km^2 then dropped
#' @keywords internal
#' @noRd
calc_area <- function(.data) {
  units::drop_units(units::set_units(sf::st_area(.data), km^2))
}


#' Calculate conc sum from n1 to n
#' \deqn{\sum_{i=1}^{n1}\frac{t_i*a_i}{T_1}}
#' @keywords internal
#' @noRd
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
#' @noRd
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

#' Calculate d matrix
#' @keywords internal
#' @noRd
calc_d <- function(.data){
  suppressWarnings(
    dmat <- sf::st_distance(sf::st_centroid(.data))
  )
  dmat <- units::drop_units(units::set_units(dmat, km))
  diag(dmat) <- sqrt(.data$.a)^0.5

  dmat
}

#' Calculate c matrix
#' @keywords internal
#' @noRd
calc_c <- function(.data){
  exp(-1 * calc_d(.data))
}

#' Calculate Pgg matrix
#' @keywords internal
#' @noRd
calc_pgg <- function(.data, .g){
  .c <- calc_c(.data)
  .N <- length(.g)
  .gmat <- matrix(data = rep(.g, .N), nrow = .N, ncol = .N, byrow = TRUE)

  sum(.gmat * .c)/sum(.g)^2
}

#' Calculate k matrix
#' @keywords internal
#' @noRd
calc_k <- function(.data, .total){
  .d <- calc_d(.data)
  .N <- nrow(.data)
  .tmat <- matrix(data = rep(.total, .N), nrow = .N, ncol = .N, byrow = TRUE)

  rowSums(.tmat*(-1 * .d))/sum(.tmat*(-1 * .d))
}


#' Weighted Centroid
#' @keywords internal
#' @noRd
calc_weighted_centroid<- function(.data, .wt) {
  suppressWarnings(
    .coords <- sf::st_coordinates(sf::st_centroid(.data))
  )

  sf::st_sfc(sf::st_point(c(stats::weighted.mean(x = .coords[, 1], w = .wt),
                            stats::weighted.mean(x = .coords[, 2], w = .wt))),
             crs = sf::st_crs(.data))
}

#' Distance to Point
#' @keywords internal
#' @noRd
calc_dist_centroid<- function(.data) {
  .pt <- calc_weighted_centroid(.data)

  units::drop_units(units::set_units(sf::st_distance(.pt, .data), km ))
}
