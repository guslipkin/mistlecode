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
#' @export
#'
#' @rdname assembly_utils
#'
#' @param x A vector of functions. The options are `c("set", "add", "subtract",
#'   "multiply", "divide", "modulo")`. A named vector can be supplied where the
#'   functions returned will match the values of the supplied vector with the
#'   names of the supplied vector.
#'
#' @return A list of functions.
#' @export
#'
#' @examples
#' \dontrun{
#' get_premade(c("add", "prod" = "multiply"))
#' }
get_premade <- function(x = c("jump", "set", "add", "subtract", "multiply", "divide", "modulo")) {
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
  fun_names <- names(x)
  blank_names <- which(fun_names == "")
  fun_names[blank_names] <- names(fun_list[blank_names])
  names(fun_list) <- fun_names
  if (length(fun_list) == 1) fun_list <- fun_list[[1]]
  return(fun_list)
}
