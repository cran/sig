\name{write_sigs}
\alias{write_sigs}
\alias{write_sigs.character}
\alias{write_sigs.default}
\alias{write_sigs.environment}
\alias{write_sigs.siglist}
\title{Write sigs to file}
\usage{
  write_sigs(x, file = stdout(), ...)

  \method{write_sigs}{default} (x, file = stdout(), ...)

  \method{write_sigs}{siglist} (x, file = stdout(), ...)

  \method{write_sigs}{environment} (x, file = stdout(),
    ...)

  \method{write_sigs}{character} (x, file = stdout(), ...)
}
\arguments{
  \item{x}{A list of function signatures.  See details.}

  \item{file}{A file path or connection to write the output
  to (stdout by default).}

  \item{...}{passed to \code{toString.siglist}.}
}
\value{
  A character vector of the lines that were written to file
  is invisibly returned.  Mostly invoked for the side
  effect of writing function signatures to a file.
}
\description{
  Writes a list of function signatures to a file.
}
\details{
  Where \code{x} is an object of class |code{siglist}, the
  function essentially calls
  \code{writeLines(tostring(x))}. If the input is a single
  function signature (of class \code{sig}), then it is
  coerced into a \code{siglist}.  If the input is an
  environment or path to a file, then \code{list_sigs} is
  called on the input before writing.
}
\examples{
#Step by step:
#First, list some sigs.
utils_sigs <- list_sigs(pkg2env(utils))
#Without a file argument, sigs are just printed to the console.
head(write_sigs(utils_sigs))
#Write to a file
tmpf <- tempfile("sig", fileext = ".R")
write_sigs(utils_sigs, tmpf)
\dontrun{
Open the file we've just written
shell(tmpf, wait = FALSE)
}
#Can also list and write in one line.
tmpf2 <- tempfile("sig", fileext = ".R")
write_sigs(pkg2env(grDevices), tmpf2)
#Single sigs are coerced to siglists
write_sigs(sig(stats::var))
}

