\name{as.sig}
\alias{as.sig}
\alias{as.sig.default}
\alias{as.sig.list}
\alias{as.sig.sig}
\alias{as.sig.siglist}
\title{Coerce object to be a sig}
\usage{
  as.sig(x, ...)

  \method{as.sig}{default} (x, ...)

  \method{as.sig}{siglist} (x, ...)

  \method{as.sig}{list} (x, ...)

  \method{as.sig}{sig} (x, ...)
}
\arguments{
  \item{x}{Object to coerce.}

  \item{...}{Passed to other \code{as.sig} methods.}
}
\value{
  An object of class \code{sig}.
}
\description{
  Coerces an object to be a \code{sig}.
}
\examples{
as.sig(
  list(name = "fun", alist(x =,y = 1))
)
}
\seealso{
  \code{\link{as.siglist}}
}

