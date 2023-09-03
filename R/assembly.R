#' Create an AoC Assembly Computer
#'
#' @rdname assembly
#' @md
#'
#' @param registers A list of registers
#' @param functions A list of functions. Registers can be referred to with
#'   `self[[register_name]]`. They must have two arguments, `x` and `y`, even if
#'   only one is used. The index is iterated with `private$.inc(j)` where `j` is
#'   the number of places to "jump." Example: `\(x, y) private$.inc(self[[x]])`
#'   will increment `self$index` by `self[[x]]`.
#' @param increment (Default: `1`) The number of places to increment by default.
#' @param regex (Default: `"[, \\+]+"`) The pattern to use for
#'   [stringr::str_split]
#'
#' @details
#' `try_numeric(x)`
#' :  Takes input and tries to cast it as numeric. If that fails, it returns the
#'    original object.
#'
#' `val_or_index(x)`
#' :  If `x` is numeric, return `x`, otherwise return `self[[x]]`.
#'
#' `call(f, x, y)`
#' :  Calls one of `functions` using `x` and `y` as arguments.
#'    There are two ways the function can be used:
#'    * `f` is not length one and `x` and `y` are null, then `f[1](f[2], f[3])`
#'    * `f` is length one, and `x` and `y` are not null, then `f(x, y)`
#'
#' `run(x, target, until, pattern)`
#' :  Tries to run the instructions provided by `x`. If `x` is a character
#'    vector, it will try to split the instructions into a segments for
#'    `call(f, x, y)`.
#'    * `x`: A character vector of instructions as `c("f x y")` or a list of
#'      character vectors in the format `list(c("f", "x", "y"))`
#'    * `target`: (Default: `NULL`) The `register` to print as the final result.
#'      If `NULL`, `self` will be printed.
#'    * `pattern`: See the `regex` argument for more details.
#'
#' `.inc(increment)`
#' :  A private function used to increment `self$index` by `increment`.
#'
#' @returns A new `assembly` R6 object with `registers` and `functions` as the
#'   fields and methods, respectively. Along with the functions detailed in the
#'   details section.
#' @export
create_assembly <- function(registers, functions, increment = 1, regex = "[, \\+]+") {
  assembly <-
    R6::R6Class(
      "assembly",
      public = unlist(list(
        as.list(c(registers, functions)),
        "index" = 1,
        "try_numeric" = \(x) {
          tryCatch(as.numeric(x), warning = function(w) x)
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
  return(assembly$new())
}

#' Add a Processor Layer to an Assembly Instruction Set
#'
#' @rdname add_processor
#'
#' @param x A vector of instructions
#'
#' @return A list of vectors of length two. The first item is `"proc"` and the
#'   second is the original instruction in x.
#' @export
add_processor <- function(x) {
  lapply(x, \(x) c("proc", x))
}
