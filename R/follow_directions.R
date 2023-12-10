#' follow_directions
#'
#' @param .data A vector of `RLUD` directions. See `expand_directions` to
#'   generate from a data.frame of direction and magnitude.
#' @param row (Default: `0`) The starting row value
#' @param col (Default: `0`) The starting column value
#' @param preserve_data A boolean to indicate if data besides `row` and `col`
#'   should be returned.
#'
#' @return A data.frame of coordinate pairs with `row` and `col` values.
#' @export
follow_directions <- function(.data, row = 0, col = 0, preserve_data = FALSE) {
  dt <- .data
  re_pete <- if (rlang::is_atomic(dt)) length(dt) else nrow(dt)

  df <-
    re_pete |>
    seq_len() |>
    purrr::map(\(i) {
      dir <- if (rlang::is_atomic(dt)) dt[i] else dt[i,"dir"]

      if (dir == "R") { coords <- list("row" = row, "col" = col + 1) }
      else if (dir == "L") { coords <- list("row" = row, "col" = col - 1) }
      else if (dir == "U") { coords <- list("row" = row - 1, "col" = col) }
      else if (dir == "D") { coords <- list("row" = row + 1, "col" = col) }

      data.frame("row" = coords$row, "col" = coords$col)
    }) |>
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(c('row', 'col')), cumsum))

  if (preserve_data) cbind(dt, df) else df
}
