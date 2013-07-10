\name{as.list.sig}
\alias{as.list.sig}
\alias{as.list.siglist}
\alias{as.list.sigreport}
\title{Convert to list}
\usage{
  \method{as.list}{sig} (x, ...)

  \method{as.list}{siglist} (x, ...)

  \method{as.list}{sigreport} (x, ...)
}
\arguments{
  \item{x}{\code{sig}, \code{siglist} or \code{sigreport}
  object.}

  \item{...}{Passed from other \code{as.list} methods.}
}
\value{
  A list.
}
\description{
  Strips class attributes to return a list.
}
\examples{
as.list(sig(read.csv))
head(as.list(list_sigs(pkg2env(stats))))
as.list(sig_report(baseenv()))
}

