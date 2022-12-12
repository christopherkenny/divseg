`%||%` <- function(a, b) if (is.null(a)) b else a

template_mutate <-
  "#' @rdname ``fn_name``
#' @param ... arguments to forward to ``fn_name`` from ``fun``
#' @export
  ``fun`` <- function(..., .data = dplyr::cur_data_all()){
  ``fn_name``(.data = .data, ...)
  }"


use_mutate <- function(name = NULL, open = rlang::is_interactive()) {
  usethis::proj_path()
  name <- name %||% usethis:::get_active_r_file(path ='R')
  path <- fs::path('R', name)
  name <- usethis:::slug(name, "R")
  lines <- readr::read_lines(path)

  single <- sum(stringr::str_detect(lines, '<- function'))
  if (single > 2) {
    usethis::ui_stop('`{name}` has more than two function definitions.')
  } else if (single < 1) {
    usethis::ui_stop('`{name}` does not have function definitions.')
  }

  def_line <- lines[stringr::str_detect(lines, '<- function')]

  fn_name <- stringr::str_split(def_line, '<-')[[1]][1] %>% stringr::str_trim()
  prefix <- paste0(stringr::str_split(def_line, '_')[[1]][1], '_')
  fun <- stringr::str_sub(fn_name, start = stringr::str_length(prefix) + 1)

  if (single == 2) {
    if (!stringr::str_detect(def_line[2], paste0(fun, ' <- function'))){
      usethis::ui_stop('Two function definitions in `{name}` and second is not made by prior call to `use_mutate`.')
    }
  }

  new_lines <- stringr::str_replace_all(template_mutate, pattern = '``fun``', replacement = fun)
  new_lines <- stringr::str_replace_all(new_lines, pattern = '``fn_name``', replacement = fn_name)

  if (rlang::is_installed('styler')){
    new_lines <- styler::style_text(new_lines)
  }

  if (single == 2){
    lines[(length(lines) - length(new_lines) + 1):length(lines)] <- new_lines
    readr::write_lines(lines, path)
  } else {
    readr::write_lines(c(lines, '', new_lines), path)
  }

  invisible(new_lines)
}

pick_n <- function(n) {
  stopifnot(rlang::is_integerish(n, n = 1))
  tibble::deframe(dplyr::pick(all_of(n)))
}
