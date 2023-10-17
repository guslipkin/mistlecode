#' @importFrom pacman p_depends
.onAttach <- function(libname, pkgname) {
  d <- pacman::p_depends("mistlecode", local = TRUE)$Depends
  d <- d[!(d %in% c("pacman", "renv", "testthat"))]
  packageStartupMessage(
    "To install `mistlecode` yourself, run `devtools::install_github('guslipkin/mistlecode')`.\n\n",
    "Also loading: ", paste(d, collapse = " ")
  )
}
