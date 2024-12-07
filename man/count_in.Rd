% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_in.R
\name{count_in}
\alias{count_in}
\title{Count the number of times \code{x} appears in \code{y}}
\usage{
count_in(x, y)
}
\arguments{
\item{x}{A vector}

\item{y}{A vector of the same class/type as \code{x}}
}
\value{
A named vector where the names are the value of \code{x} that was found in
\code{y} and the values are the number of times \code{x} was found in \code{y}
}
\description{
Count the number of times \code{x} appears in \code{y}
}
\examples{
count_in(
  x = c(1, 2, 3, 3, 3, 4, 4),
  y = c(2, 2, 3, 3, 4)
)
}
