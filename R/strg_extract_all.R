#' Actually Extract All Patterns
#'
#' @param str A string or vector of strings
#' @param pattern A regex pattern. If you want to extract multiple terms, use
#'   the regex or operator. (For 'one' or 'two': `one|two`)
#'
#' @return A list of matches the same length as `str`. Each list item will
#'   contain all matches.
#' @export
#' @examples
#' c(
#'   'oneightwo',
#'   '1two3four'
#' ) |>
#'   strg_extract_all('one|two|three|four|five|six|seven|eight|nine|[1-9]')
strg_extract_all <- function(str, pattern) {
  pattern <-
    pattern |>
    stringi::stri_split_regex(pattern = '\\|', simplify = TRUE) |>
    as.vector()
  str |>
    purrr::map(\(x) {
      m <-
        x |>
        stringi::stri_locate_all_regex(pattern, omit_no_match = TRUE) |>
        purrr::reduce(Matrix::rbind2) |>
        data.table::data.table() |>
        dplyr::arrange(.data$start)
      stringi::stri_sub(x, from = m$start, to = m$end)
    })
}
