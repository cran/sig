% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{print_engine}
\alias{print_engine}
\title{Workhorse of the print methods}
\usage{
print_engine(x, ...)
}
\arguments{
\item{x}{Object to print}

\item{...}{Passed to \code{toString}.}
}
\value{
The input is invisibly returned, but the function is mostly invoked for the side effect of printing the object.
}
\description{
Wraps toString methods with cat.
}
\note{
Not intended for general consumption.  This function is only 
exported because of package build requirements.
}
