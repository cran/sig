\name{is.siglist}
\alias{is.siglist}
\title{Is the input a siglist?}
\usage{
  is.siglist(x)
}
\arguments{
  \item{x}{Object to test.}
}
\value{
  \code{TRUE} if the object inherits from class ``siglist''
  and \code{is.sig} returns \code{TRUE} for each element of
  the input, and FALSE otherwise.
}
\description{
  Does the input inherit from ``siglist''?
}
\examples{
stopifnot(
  !is.siglist(sig(with))     #1 sig is not a siglist.
)
}

