#' Get coordinates adjacent to a point
#'
#' @details Does NOT include the target point. If a wall is clipped, the
#'   coordinates will not include the clipped wall.
#'
#' @param yy The target y coordinate
#' @param xx The target x coordinate
#' @param nrow The maximum row value. Not needed if `data` is provided.
#' @param ncol The maximum column value. Not needed if `data` is provided.
#' @param data A matrix used to determine `nrow` and `ncol` if provided.
#' @param yCount The number of rows to include on either side of the target
#' @param xCount The number of columns to include on either side of the target
#'
#' @return A matrix of row and col coordinates
#' @export
get_adjacent_coords <- function(yy, xx, nrow = NULL, ncol = NULL, data = NULL,
                                yCount = 1, xCount = 1) {
  if (!is.null(data)) {
    nrow <- nrow(data)
    ncol <- ncol(data)
  }

  l <- .get_adjacent(yy, xx, yCount, xCount)
  y <- l$row
  x <- l$col

  expand.grid("row" = y, "col" = x) |>
    dplyr::filter(!(col == xx & row == yy)) |>
    as.matrix()
}

#' Get values adjacent to the target
#'
#' @details Does include the target point. If a wall is clipped, the dimensions
#'   will not include the clipped wall.
#'
#' @param yy The target y coordinate
#' @param xx The target x coordinate
#' @param data A matrix
#' @param yCount The number of rows to include on either side of the target
#' @param xCount The number of columns to include on either side of the target
#'
#' @return A matrix of the dimensions specified
#' @export
get_adjacent_values <- function(yy, xx, data, yCount = 1, xCount = 1) {
  nrow <- nrow(data)
  ncol <- ncol(data)

  l <- .get_adjacent(yy, xx, yCount, xCount)
  y <- l$row
  x <- l$col

  data[min(y):max(y), min(x):max(x)]
}

#' @keywords internal
.get_adjacent <- function(yy, xx, yCount, xCount) {
  y <- yy; y <- (y - yCount):(y + yCount)
  x <- xx; x <- (x - xCount):(x + xCount)

  y <- y[sapply(y, \(j) dplyr::between(j, 1, nrow))]
  x <- x[sapply(x, \(j) dplyr::between(j, 1, ncol))]

  return(list("row" = y, "col" = x))
}
