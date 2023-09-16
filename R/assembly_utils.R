#' Add a Processor Layer to an Assembly Instruction Set
#'
#' @rdname assembly_utils
#'
#' @param x A vector of instructions
#'
#' @return A list of vectors of length two. The first item is `"proc"` and the
#'   second is the original instruction in x.
#' @export
add_processor <- function(x) {
  lapply(x, \(x) c("proc", x))
}

#' Easily Get Premade Assembly Instructions
#'
#' @rdname assembly_utils
#'
#' @param x A vector of functions. The options are
#'    `c("set", "add", "subtract", "multiply", "divide", "modulo")`.
#' @param fun_names (Default: `NULL`). A vector the same length as `x` with
#'   names for the functions. If `NULL`, the default names are used.
#'
#' @return A list of functions.
#' @export
get_premade <- function(x = c("jump", "set", "add", "subtract", "multiply", "divide", "modulo"), fun_names = NULL) {
  stopifnot(length(x) == length(fun_names))
  x <- rlang::arg_match(x, multiple = TRUE)
  fun_list <- list(
    "jump" = \(x, y) {
      if (self$val_or_index(x) != 0) private$.inc(self$try_numeric(y) - 1)
    },
    "set" = \(x, y) { self[[x]] <- self$val_or_index(y) },
    "add" = \(x, y) { self[[x]] <- self[[x]] + y },
    "subtract" = \(x, y) { self[[x]] <- self[[x]] - y },
    "multiply" = \(x, y) { self[[x]] <- self[[x]] * self$val_or_index(y) },
    "divide" = \(x, y) { self[[x]] <- self[[x]] - self$val_or_index(y) },
    "modulo" = \(x, y) { self[[x]] <- self[[x]] %% self$val_or_index(y) }
  )[x]
  if (!is.null(fun_names)) names(fun_list) <- fun_names
  fun_list
}
