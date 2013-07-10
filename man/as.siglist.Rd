\name{as.siglist}
\alias{as.siglist}
\alias{as.siglist.list}
\alias{as.siglist.sig}
\alias{as.siglist.siglist}
\title{Coerce object to be a siglist}
\usage{
  as.siglist(x, ...)

  \method{as.siglist}{sig} (x, ...)

  \method{as.siglist}{list} (x, ...)

  \method{as.siglist}{siglist} (x, ...)
}
\arguments{
  \item{x}{Object to coerce.}

  \item{...}{Passed to other \code{as.siglist} methods.}
}
\value{
  An object of class \code{siglist}.
}
\description{
  Coerces an object to be a \code{siglist}.
}
\examples{
as.siglist(list(
  sig(mean),
  list(name = "fun", alist(x =,y = 1))
))
}
\seealso{
  \code{\link{as.sig}}
}

