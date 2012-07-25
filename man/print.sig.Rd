\name{toString.sig}
\alias{print.sig}
\alias{toString.sig}
\title{Print a sig object}
\usage{
  \method{toString}{sig} (x, width = getOption("width"),
    exdent = nchar(x$name), ...)

  \method{print}{sig} (x, width = getOption("width"),
    exdent = nchar(x$name), ...)
}
\arguments{
  \item{x}{An object of class sig.}

  \item{width}{Width of string to display.}

  \item{exdent}{Non-negative integer specifying the
  indentation of subsequent lines in the string.}

  \item{...}{Passed to \code{toString}}
}
\value{
  \code{toString} creates a string representation of a
  function signature. \code{print} is mostly invoked for
  the side effect of printing a function
  signature,invisibly returning its input.
}
\description{
  Prints a function signature object.
}
\examples{
spd <- sig(print.default)
print(spd)
print(spd, width = 40)
print(spd, width = 40, exdent = 2)
}

