#' Get the index of an object using modulo
#'
#' @param x A vector or list
#' @param n A numeric vector of indices
#'
#' @return A numeric index
#' @export
index <- function(x, n) {
  stopifnot(length(x) > 0)
  n <- n %% length(x)
  n[n == 0] <- length(x)
  return(n)
}
