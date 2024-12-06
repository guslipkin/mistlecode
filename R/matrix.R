#' @title Matrix intersections
#' @param x A matrix
#' @param y A matrix
#' @param false The value to use when `fn` evaluates to `FALSE`
#' @param fn A function to compare the matrices
#' @name matrix
#' @examples
#' x <- matrix(sample(c('', '@'), 10, replace = TRUE), 2) |> print()
#' y <- matrix(sample(c('', '@'), 10, replace = TRUE), 2) |> print()
#'
#' matrix_intersect(x, y)
#' matrix_join(x, y)
#' matrix_anti_join(x, y)
NULL

#' @rdname matrix
#' @export
matrix_intersect <- function(x, y, false = '', fn = \(x, y) x == y) {
  stopifnot(dim(x) == dim(y))
  x[!fn(x, y) |> replace_na()] <- false
  return(x)
}

#' @rdname matrix
#' @export
matrix_anti_join <- function(x, y, false = '', fn = \(x, y) x == y) {
  stopifnot(dim(x) == dim(y))
  x[fn(x, y) |> replace_na()] <- false
  return(x)
}

#' @rdname matrix
#' @export
matrix_join <- function(x, y, false = '', fn = \(z) z != false) {
  stopifnot(dim(x) == dim(y))
  copy <- matrix(false, nrow(x), ncol(x))
  fn_x <- fn(x); fn_y <- fn(y);
  copy[fn_x] <- x[fn_x]
  copy[fn_y] <- y[fn_y]
  return(copy)
}


#' Replace NA
#'
#' @param x A vector or matrix
#'
#' @return `x` but `NA` values have been replaced with `FALSE`
#' @keywords internal
.replace_na <- function(x) {
  x[is.na(x)] <- FALSE
  return(x)
}

#' Cast a matrix to the desired type
#' @rdname cast_matrix
#' @param x A matrix
#' @param fn A bare function such as [as.character] or [as.integer]
#'
#' @return A matrix of the desired type
#' @export
cast_matrix <- function(x, fn) {
  x |>
    fn() |>
    `dim<-`(dim(x))
}
