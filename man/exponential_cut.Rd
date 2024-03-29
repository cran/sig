% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{exponential_cut}
\alias{exponential_cut}
\title{Cut with exponential breaks}
\usage{
exponential_cut(x)
}
\arguments{
\item{x}{A vector of positive integers.}
}
\value{
A factor.
}
\description{
Wrapper to \code{cut} for positive integers.
}
\note{
The breaks are 1, 2, 3 to 4, 5 to 8, etc. 
No input checking is done; use at your peril.
}
\examples{
\dontrun{
exponential_cut(c(1:10, 500))
}
}
\seealso{
\code{\link[base]{cut}}
}
