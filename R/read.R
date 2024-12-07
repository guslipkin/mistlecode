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

#' Read text that should be a matrix
#'
#' @param path A file path
#' @param split A character to split the rows by
#' @param cast A bare function to cast with
#'
#' @return A `matrix` of type `cast`
#' @export
read_matrix <- function(path, split = '', skip = 0, cast = identity) {
  path |>
    read.delim(header = FALSE, sep = '', skip = skip) |>
    dplyr::mutate('V1' = .data$V1 |> strsplit(split)) |>
    tidyr::unnest_wider('V1', names_sep = '_') |>
    as.matrix() |>
    unname() |>
    cast()
}

# read_split_input <- function(path, fn1, fn2) {
#   # skip <- which(readLines(path) == '')
#
#   # list(
#   #   'dt1' = fn1(path),
#   #   'dt2' = fn2(path)
#   # )
# }

