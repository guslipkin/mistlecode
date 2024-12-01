#' Count the number of times `x` appears in `y`
#'
#' @param x A vector
#' @param y A vector of the same class/type as `x`
#'
#' @return A named vector where the names are the value of `x` that was found in
#'   `y` and the values are the number of times `x` was found in `y`
#' @export
#'
#' @examples
#' count_in(
#'   x = c(1, 2, 3, 3, 3, 4, 4),
#'   y = c(2, 2, 3, 3, 4)
#' )
count_in <- function(x, y) {
  stopifnot(all(c(is.vector(x), is.vector(y))))

  tab <- table(y)[as.character(x)]
  tab[!is.na(tab)]
}
