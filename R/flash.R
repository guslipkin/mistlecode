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
#' @export
flash <- function(m, .fn, i = 1, cores = future::availableCores() - 1, ...) {
  session <- if (cores == 1) future::sequential else future::multisession
  future::plan(session, workers = cores)
  progressr::with_progress({
    p <- progressr::progressor(steps = ncol(m) * nrow(m) * i)
    while (i > 0) {
      m <-
        expand.grid(
          "y" = seq_len(nrow(m)),
          "x" = seq_len(ncol(m))
        ) |>
        furrr::future_pmap(\(y, x) {
          p()
          rlang::exec(quote(.fn), rlang::inject(list(y = y, x = x, m = m, ...)))
        }) |>
        unlist() |>
        matrix(nrow = nrow(m), byrow = FALSE)
      i <- i - 1
    }
  })
  future::plan(future::sequential)
  return(m)
}
