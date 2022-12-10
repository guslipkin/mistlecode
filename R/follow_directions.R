#' follow_directions
#'
#' @param .data A vector of `RLUD` directions. See `expand_directions` to
#'   generate from a data.frame of direction and magnitude.
#' @param row The starting row value
#' @param col The starting column value
#'
#' @return A data.frame of coordinate pairs with `row` and `col` values.
#' @export
follow_directions <- function(.data, row, col) {
  dt <- .data
  lapply(1:length(dt), \(i) {
    dir <- dt[i]

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
}
