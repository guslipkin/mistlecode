#' expand_directions
#'
#' @param dt A data.frame or vector depending on `preserve_data`
#' @param preserve_data A boolean to indicate if a data.frame (TRUE) or vector
#'   (FALSE) should be returned.
#'
#' @return A vector of directions
#' @export
expand_directions <- function(dt, preserve_data = FALSE) {
  n <- colnames(dt)
  n <- c("dir", "mag", n[!(n %in% c("dir", "mag"))])
  dt <-
    dt |>
    dplyr::select(!!.data$n) |>
    dplyr::slice(rep(1:n(), .data$mag))
  if (preserve_data) dt else dplyr::pull(dt, .data$dir)
}
