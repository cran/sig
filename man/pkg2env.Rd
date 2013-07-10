\name{pkg2env}
\alias{pkg2env}
\title{Get environment of a package.}
\usage{
  pkg2env(pkg)
}
\arguments{
  \item{pkg}{A package.}
}
\value{
  the environment corresponding to \code{pkg}.
}
\description{
  Utility function to get the environment of a package on
  the search path.
}
\examples{
pkg2env(graphics)
}
\seealso{
  \code{\link[base]{list2env}}
}

