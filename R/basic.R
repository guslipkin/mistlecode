#' Create a BASIC Computer
#'
#' @rdname basic
#'
#' @param registers A list of registers
#' @param functions A list of functions. Registers can be referred to with
#'   `self[[register_name]]`. The index is iterated with `private$.inc(j)` where
#'   `j` is the number of places to "jump."
#'
#' @return A new R6 object
#' @export
create_basic <- function(registers, functions, increment = 1, regex = "[, \\+]+") {
  basic <-
    R6::R6Class(
      "basic",
      public = unlist(list(
        as.list(c(registers, functions)),
        "index" = 1,
        "try_numeric" = \(x) {
          xx <- suppressWarnings(as.numeric(x))
          if (is.na(xx)) x else xx
        },
        "val_or_index" = \(x) { if (is.numeric(x)) x else self[[x]] },
        "call" = \(f, x = NULL, y = NULL) {
          if (length(f) != 1 & is.null(x) & is.null(y)) {
            x <- f[2]; y <- f[3]; f <- f[1]; # don't put this first!!!
          }
          self[[f]](self$try_numeric(x), self$try_numeric(y))
        },
        "run" = \(x, target = NULL, until = length(x), pattern = regex) {
          if (!is.list(x)) { x <- stringr::str_split(x, pattern) }
          while(self$index <= until) {
            self$call(x[[self$index]])
          }
          if (is.null(target)) self else self[[target]]
        }
      )),
      private = list(
        .inc = \(i = increment) { self$index <- self$index + i; self; }
      )
    )
  return(basic$new())
}
