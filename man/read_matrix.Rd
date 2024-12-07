% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_matrix}
\alias{read_matrix}
\title{Read text that should be a matrix}
\usage{
read_matrix(path, split = "", skip = 0, cast = identity)
}
\arguments{
\item{path}{A file path}

\item{split}{A character to split the rows by}

\item{cast}{A bare function to cast with}
}
\value{
A \code{matrix} of type \code{cast}
}
\description{
Read text that should be a matrix
}
