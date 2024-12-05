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

#' Get a list of matrix diagonals
#'
#' @param m A square matrix
#' @param corner A coner where the diagonal should start from
#'
#' @return A list of vectors
#' @export
#' @details The returned vectors all start on the top or bottom of the original
#' matrix. If opposing corners (eg top left and bottom right) are chosen, then
#' the longest vector from the bottom corner will be dropped because it is a
#' duplicate of the longest from the top.
get_diagonal2 <- function(m, corner = c('tl', 'tr', 'bl', 'br')) {
  stopifnot(nrow(m) == ncol(m))
  corner <- rlang::arg_match(corner, c('tl', 'tr', 'bl', 'br'), multiple = TRUE)
  col_seq <-
    m |>
    ncol() |>
    seq_len()

  map_forward <- function(m, col_seq) {
    purrr::map(col_seq, \(x) {
      x <- m[x:(nrow(m)), 1:(ncol(m) + 1 - x)]
      if (length(x) == 1) x else diag(x)
    })
  }

  diags <-
    list(
      'tr' = \() { map_forward(m, col_seq) },
      'tl' = \() { m[(nrow(m)):1,] |> map_forward(col_seq) },
      'br' = \() { m[,(ncol(m)):1] |> map_forward(col_seq) },
      'bl' = \() { m[(nrow(m):1),(ncol(m):1)] |> map_forward(col_seq) }
    )[corner] |>
      purrr::map(\(x) rev(x())) |>
      (\(x) {
        if (length(corner) == 1) unlist(x, recursive = FALSE) else x
      })()

  if (all(c('tl', 'br') %in% names(diags))) diags$br[[length(diags$br)]] <- NULL
  if (all(c('tr', 'bl') %in% names(diags))) diags$bl[[length(diags$bl)]] <- NULL

  return(diags)
}
