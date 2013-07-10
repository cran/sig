\name{toString.siglist}
\alias{print.siglist}
\alias{toString.siglist}
\title{Print a siglist object}
\usage{
  \method{toString}{siglist} (x,
    width = getOption("width"), ...)

  \method{print}{siglist} (x, width = getOption("width"),
    ...)
}
\arguments{
  \item{x}{An object of class \code{siglist}.}

  \item{width}{Width of string to display.}

  \item{...}{Passed to the equivalent \code{sig} method.}
}
\value{
  \code{toString} creates a string representation of a
  function signature. \code{print} is mostly invoked for
  the side effect of printing a function signature,
  invisibly returning its input.
}
\description{
  Prints a list of function signature objects.
}
\examples{
method_sigs <- list_sigs(pkg2env(methods))
print(method_sigs)
print(method_sigs, width = 40)
print(method_sigs, width = 40, exdent = 2)
toString(method_sigs)
}

