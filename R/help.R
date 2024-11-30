#' Get help for {mistlecode}
#'
#' @return NULL
#' @export
help <- function() {
  c(
    'Loaded packages include:',
    '{.pkg cheapr}:\t Greatest common divisor and least common multiple',
    '{.pkg cipheR}:\t Basic text ciphers',
    '{.pkg slider}:\t Sliding window functions',
    'MISC:
      {.pkg data.table},
      {.pkg dplyr},
      {.pkg purrr},
      {.pkg stringr},
      {.pkg tidyverse},
      {.pkg glue},
      {.pkg rlang},
      {.pkg R6}'
  ) |>
    cli::cli_inform()
}
