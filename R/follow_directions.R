#' follow_directions
#'
#' @param .data A vector of `RLUD` directions. See `expand_directions` to
#'   generate from a data.frame of direction and magnitude.
#' @param row The starting row value
#' @param col The starting column value
#' @param preserve_data A boolean to indicate if data besides `row` and `col`
#'   should be returned.
#'
#' @return A data.frame of coordinate pairs with `row` and `col` values.
#' @export
follow_directions <- function(.data, row, col, preserve_data = FALSE) {
  dt <- .data
  re_pete <- ifelse(is.atomic(dt), length(dt), nrow(dt))

  df <-
    lapply(1:re_pete, \(i) {
      dir <- ifelse(rlang::is_atomic(dt), dt[i], dt[i,"dir"])

      if (dir == "R") { coords <- list("row" = row, "col" = col + 1) }
      else if (dir == "L") { coords <- list("row" = row, "col" = col - 1) }
      else if (dir == "U") { coords <- list("row" = row - 1, "col" = col) }
      else if (dir == "D") { coords <- list("row" = row + 1, "col" = col) }

      row <<- coords$row
      col <<- coords$col

      return(list("row" = row, "col" = col))
    }) |>
      unlist(recursive = TRUE) |>
      matrix(ncol = 2, byrow = TRUE) |>
      `colnames<-`(c("row", "col")) |>
      data.frame()

  if (preserve_data) cbind(dt, df) else df
}
