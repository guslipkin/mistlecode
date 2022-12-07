#' Parse a grid input
#'
#' @importFrom purrr map
#' @importFrom stringr str_sub
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
    width <- ceiling(max(nchar(vec)) / width)
  }

  # these bits are from Emil Hvitfeldt's AoC 2022-05 solution
  loc <- seq(1, (max(nchar(vec)) + 1) / width) * width - (width / 2)
  purrr::map(loc, ~ stringr::str_sub(vec, .x, .x)) |>
    purrr::map(setdiff, c("", " ")) |>
    purrr::map(rev)
}
