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
  if(is.null(maxRow)) { maxRow <- max(row) } else { maxRow <- max(maxRow) }
  if(is.null(maxCol)) { maxCol <- max(col) } else { maxCol <- max(maxCol) }

  mat <- matrix(NA, maxRow, maxCol)

  mapply(\(row, col, data) {
    mat[row,col] <<- data
  }, row, col, data)

  return(mat)
}
