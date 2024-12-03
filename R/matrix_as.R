#' Convert a matrix to the given type
#' @name matrix_as
#' @param x A matrix
#'
#' @return A matrix in the desired type
NULL

#' @rdname matrix_as
#' @export
as.integer.matrix <- function(x) {
  x |>
    as.integer() |>
    `dim<-`(dim(x))
}

#' @rdname matrix_as
#' @export
as.character.matrix <- function(x) {
  x |>
    as.character() |>
    `dim<-`(dim(x))
}

#' @rdname matrix_as
#' @export
as.numeric.matrix <- function(x) {
  x |>
    as.numeric() |>
    `dim<-`(dim(x))
}

#' @rdname matrix_as
#' @export
as.logical.matrix <- function(x) {
  x |>
    as.logical() |>
    `dim<-`(dim(x))
}
