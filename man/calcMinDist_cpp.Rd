% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{calcMinDist_cpp}
\alias{calcMinDist_cpp}
\title{Calculate minimum distance}
\usage{
calcMinDist_cpp(r, x)
}
\arguments{
\item{r}{A numeric matrix with each row being a pair of coordinates.}

\item{x}{A numeric matrix with each row being a pair of coordinates.}
}
\value{
A vector with as many elements as rows in r, corresponding to the minimum distance between the rows in r and all the rows in x.
}
\description{
This function calculates the minimum distance between a matrix
of locations r and another matrix of locations x
}
