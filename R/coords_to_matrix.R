#' Convert a set of coords to a matrix
#'
#' @param data A vector of data
#' @param row A vector of row indices
#' @param col A vector of column indices
#' @param maxRow The row count for the resulting matrix
#' @param maxCol The column count for the resulting matrix
#'
#' @return A matrix of dimensions row,col with data
#' @export
coords_to_matrix <- function(data, row, col, maxRow = NULL, maxCol = NULL) {
  maxRow <- if(is.null(maxRow)) max(row) else max(maxRow)
  maxCol <- if(is.null(maxCol)) max(col) else max(maxCol)

  mat <- matrix(NA, maxRow, maxCol)

  mapply(\(row, col, data) {
    mat[row,col] <<- data
  }, row, col, data)

  return(mat)
}
