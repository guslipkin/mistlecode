#' expand_directions
#'
#' @importFrom dplyr slice select
#'
#' @param .data A data.frame or vector depending on `preserve_data`
#' @param preserve_data A boolean to indicate if a data.frame (TRUE) or vector
#'   (FALSE) should be returned.
#'
#' @return A vector of directions
#' @export
expand_directions <- function(.data, preserve_data = FALSE) {
  dt <- .data
  n <- colnames(dt)
  n <- c("dir", "mag", n[!(n %in% c("dir", "mag"))])
  dt <-
    dt |>
    dplyr::select(!!n) |>
    dplyr::slice(rep(1:n(), mag))
  if (preserve_data) {
    return(dt)
  } else {
    dt <- pull(dt, dir)
    return(dt)
  }
}
