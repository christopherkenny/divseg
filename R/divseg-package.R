## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

utils::globalVariables(c('.', 'km'))


pick_n <- function(n) {
  stopifnot(rlang::is_integerish(n, n = 1))
  tibble::deframe(dplyr::across(all_of(n)))
}
