\name{[}
\alias{[}
\alias{[.siglist}
\alias{[[.siglist}
\alias{[[<-.siglist}
\alias{[<-.siglist}
\alias{Extract.siglist}
\title{Indexing for siglists}
\usage{
  \method{[}{siglist} (x, i, ...)

  \method{[[}{siglist} (x, i, ...)

  \method{[}{siglist} (x, ...) <- value

  \method{[[}{siglist} (x, ...) <- value
}
\arguments{
  \item{x}{A \code{siglist} object.}

  \item{i}{An integer vector index.}

  \item{...}{Passed from other index methods.}

  \item{value}{A value to set the subset to.}
}
\value{
  A siglist.
}
\description{
  Get or set a subset of a siglist.
}
\examples{
methods_sigs <- list_sigs(pkg2env(methods))
methods_sigs[1:5]
methods_sigs[[1]]
}
\seealso{
  \code{\link[base]{Extract}}
}

