\name{list_sigs}
\alias{list_sigs}
\alias{list_sigs.character}
\alias{list_sigs.default}
\alias{list_sigs.environment}
\title{List the signatures of all functions}
\usage{
  list_sigs(x, pattern = NULL, ...)

  \method{list_sigs}{default} (x, ...)

  \method{list_sigs}{environment} (x, pattern = NULL, ...)

  \method{list_sigs}{character} (x, ...)
}
\arguments{
  \item{x}{An environment or the the path to a file.}

  \item{pattern}{An optional regular expression. Only names
  matching pattern are returned.}

  \item{...}{Currently ignored}
}
\value{
  An object of class \code{siglist}, which is a list of
  \code{sig} obejcts.
}
\description{
  Lists the signatures of all functions in an environment
  or file.
}
\examples{
#From a package
list_sigs(pkg2env(graphics))
#Just functions beginning with 'a'.
list_sigs(pkg2env(graphics), pattern = "^a")
#From a file
list_sigs(system.file("extdata", "sample.R", package = "sig"))
}

