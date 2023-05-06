#' Perform a Function on Each Cell in a Matrix
#'
#' @description This performs the function without changing the matrix, then
#' applies all changes at once.
#'
#' @param m A matrix
#' @param .fn The function to apply
#' @param i The number of times to apply the function to the whole matrix
#' @param cores The amount of cores to use when applying the function to each
#'   cell
#'
#' @returns A matrix of the same dimensions as `m`
flash <- function(m, .fn, i, cores = future::availableCores()) {
  future::plan(future::multicore, workers = cores)
  progressr::with_progress({
    p <- progressr::progressor(steps = i)
    .do_flash(m, .fn, i, p)
  })
  future::plan(future::sequential)
}

#' @noRd
#'
#' @inheritParams flash
#' @param p A [progressr::progressor()] object
#'
#' @return `m` after the flashes have been performed
#' @keywords internal
.do_flash <- function(m, .fn, i, p) {
  while (i > 0) {
    p()
    m <-
      expand.grid(
        "y" = seq_len(nrow(m)),
        "x" = seq_len(ncol(m))
      ) |>
      furrr::future_pmap(\(y, x) {
        rlang::exec(quote(.fn), y, x, m)
      }) |>
      unlist() |>
      matrix(nrow = nrow(m), byrow = FALSE)
    i <- i - 1
  }
  return(m)
}
