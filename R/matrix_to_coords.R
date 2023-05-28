#' Convert a matrix to a matrix of coordinates and values
#'
#' @param mat A matrix to be converted to coordinate pair and data columns
#'
#' @return A data.frame with row, col, and data columns
#' @export
matrix_to_coords <- function(mat) {
  row <- nrow(mat)
  col <- ncol(mat)
  data.frame(
    "data" = as.vector(mat),
    expand.grid("row" = 1:row, "col" = 1:col)
  )
}
