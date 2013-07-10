\name{is.sig}
\alias{is.sig}
\title{Is the input a sig?}
\usage{
  is.sig(x)
}
\arguments{
  \item{x}{Object to test.}
}
\value{
  \code{TRUE} if the object inherits from class ``sig'',
  and FALSE otherwise.
}
\description{
  Does the input inherit from ``sig''?
}
\examples{
stopifnot(
  is.sig(sig(with)),
  !is.sig(with)     #functions are not their signatures.
)
}

