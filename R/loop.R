#' Offset a Vector in a Looping Fashion
#'
#' @rdname loop
#'
#' @param x A vector or a matrix
#' @param n The number of places to offset
#' @param margin "row" or "col" of a matrix
#' @param index The row or column number to change
#' @param index1 Whether or not the index starts at 0 (`FALSE`) or 1 (`TRUE`)
#'
#' @return `x`, but with the appropriate shifting
#' @export
loop <- function(x, n, margin = c("row", "col"), index, index1 = FALSE) {
  UseMethod("loop")
}

loop.default <- function(x, n) {
  n <- n %% length(x)
  if (n == 0) x else c(utils::tail(x, n), utils::head(x, -n))
}

loop.matrix <- function(x, n, margin, index, index1) {
  if (index1) { index <- index + 1 }
  if (margin == "row") {
    x[index,] <- loop.default(x[index,], n)
  } else if (margin == "col") {
    x[,index] <- loop.default(x[,index], n)
  }
  return(x)
}
