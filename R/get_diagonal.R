#' Get any matrix diagonal
#'
#' @rdname get_diagonal
#'
#' @param m A matrix
#' @param x A row or column index corresponding to the chosen `dim`
#' @param dim `"row"` or `"col"` to indicate where `x` should be applied
#' @param dir Whether or not the diagonal goes `"up"` or `"down"`
#'
#' @returns A vector of the diagonal
#' @export
get_diagonal <- function(m, x, dim = c("row", "col"), dir = c("up", "down")) {
  dim <- rlang::arg_match(dim)
  dir <- rlang::arg_match(dir)
  if (dir == "up") m <- m[(nrow(m)):1,]
  if (dim == "row") {
    m <- m[x:(nrow(m)), 1:(ncol(m) + 1 - x)]
  } else {
    m <- m[1:(nrow(m) + 1 - x), x:(ncol(m))]
  }
  if (length(m) == 1) m else diag(m)
}
