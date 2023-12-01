#' Make a File for a New Day
#'
#' @param year The year
#' @param day The day
#'
#' @return Nothing
#' @export
make_day <- function(year = lubridate::year(Sys.Date()), day = lubridate::mday(Sys.Date() + 1)) {
  day <- sprintf("%02d", day)
  folder_path <- glue::glue('{year}/{day}')
  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
  e <- rlang::current_env()
  # readLines('data/template.qmd') -> template
  mistlecode::template |>
    sapply(glue::glue, .open = '{{', .close = '}}', .envir = e) |>
    paste(collapse = '\n') |>
    writeLines(glue::glue('{folder_path}/{year}-{day}.qmd'))
}
