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

get_diagonals_at <- function(m, y, x) {
  g <- expand.grid(
    'y' = seq_len(nrow(m)),
    'x' = seq_len(ncol(m))
  )
  m[!(round(sqrt(abs(y - g$y) ^ 2 + abs(x - g$x) ^ 2) %% sqrt(2), 6) %in% round(c(0, sqrt(2)), 6))] <- NA
  d <-
    m |>
    mistlecode::matrix_to_coords() |>
    dplyr::filter(!is.na(.data$data))
  dplyr::bind_rows(
    'ne' = dplyr::filter(d, row < y & col > x),
    'nw' = dplyr::filter(d, row < y & col < x),
    'se' = dplyr::filter(d, row > y & col > x),
    'sw' = dplyr::filter(d, row > y & col < x),
    .id = 'dir'
  ) |>
    dplyr::mutate(
      'dist' = sqrt(abs(row - y)^2 + abs(col - x)^2)
    ) |>
    dplyr::arrange(.data$dir, .data$dist)
}

# m <- matrix(1:100, 10, 10)
# y <- 7; x <- 10;
# d <-
#   get_diagonals_at(m, y, x) |>
#   mistlecode::matrix_to_coords() |>
#   dplyr::filter(!is.na(.data$data))
# dplyr::bind_rows(
#   'ne' = dplyr::filter(d, row < y & col > x),
#   'nw' = dplyr::filter(d, row < y & col < x),
#   'se' = dplyr::filter(d, row > y & col > x),
#   'sw' = dplyr::filter(d, row > y & col < x),
#   .id = 'dir'
# )
