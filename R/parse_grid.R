#' Parse a grid input
#'
#' @param vec A vector of lines of text created using readLines
#' @param width The width of each chunk. If not supplied, it is guessed (poorly...)
#'
#' @return A list of vectors representing the grid
#' @export
parse_grid <- function(vec, width = NULL) {
  if (is.null(width)) {
    width <-
      strsplit(vec, " ") |>
      lapply(length) |>
      unlist() |>
      min()
  }

  # these bits are from Emil Hvitfeldt's AoC 2022-05 solution
  loc <- seq(1, (max(nchar(vec)) + 1) / width) * width - sqrt(width)
  purrr::map(loc, ~ stringr::str_sub(vec, .x, .x)) |>
    purrr::map(setdiff, c("", " ")) |>
    purrr::map(rev)
}
