#' Read a list of vectors
#'
#' @param path A file path
#' @param split A character to split the rows by
#' @param cast A bare function to cast with
#'
#' @return A list of vectors of type `cast`
#' @export
read_list <- function(path, split = ' ', cast = identity) {
  path |>
    readLines() |>
    strsplit(split) |>
    purrr::map(cast)
}
