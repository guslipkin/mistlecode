#' Create an AoC Assembly Computer
#'
#' @rdname assembly2
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
#' `jump(x, y)`
#' :  A shortcut for a common jump scheme. Can be called with
#'    `"jump_fun" = \(x, y) { self$jump(x, y) }`.
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
#' @return A new `assembly` R6 object with `registers` and `functions` as the
#'   fields and methods, respectively. Along with the functions detailed in the
#'   details section.
#' @export
create_assembly2 <- function(registers, functions, increment = 1, regex = "[\\w\\d\\-]+") {
  assembly <-
    R6::R6Class(
      "assembly",
      public = unlist(list(
        as.list(c(registers, functions)),
        "index" = 1,
        "try_numeric" = \(x) {
          tryCatch(as.numeric(x), warning = function(w) x)
        },
        "val_or_index" = \(x) {
          x <- self$try_numeric(x)
          if (is.character(x) && !is.null(self[[x]])) self[[x]] else x
        },
        "call" = \(f, ...) {
          do.call(self[[f]], args = lapply(..., self$try_numeric))
          private$.inc()
        },
        "run" = \(x, pattern = regex, target = NULL, until = length(x)) {
          while(self$index <= until) self$clock(x, pattern)
          if (is.null(target)) self else self[[target]]
        },
        "clock" = \(x, pattern = regex) {
          x <- private$.match(x, pattern)
          self$call(x[1], x[-1])
        }
      )),
      private = list(
        ".inc" = \(i = increment) { self$index <- self$index + i; self; },
        ".match" = \(x, pattern = regex) {
          if (!is.list(x)) {
            x <-
              x[[self$index]] |>
              stringr::str_match_all(pattern) |>
              unlist()
          }
          x
        }
      )
    )
  return(assembly$new())
}

# registers <- list("a" = 1, "b" = 2)
# functions <- list(
#   "sum" = \(x, y) { self[[x]] <- sum(self[[x]], y)},
#   "prod" = \(x, ...) {
#     print(x)
#     self[[x]] <- prod(self[[x]], ...)
#   }
# )
# a <- create_assembly2(registers, get_premade(c("add", "prod" = "multiply")))
# a$run(c("add a 4", "prod b 4"))
