\name{write_sigs}
\alias{write_sigs}
\title{Write sigs to file}
\usage{
  write_sigs(envir, file, pattern = NULL, ...)
}
\arguments{
  \item{envir}{An environment containing functions.}

  \item{file}{A file to write the output to.}

  \item{pattern}{If not \code{NULL}, a regular expression
  to filter the function names by.}

  \item{...}{passed to \code{toString.sig}.}
}
\value{
  Nothing of interest.  Invoked for the side effect of
  writing function signatures to a file.
}
\description{
  Writes function signatures to a text file.
}
\examples{
#From a package
tmpf <- tempfile(LETTERS[1:3], fileext = ".R")
on.exit(unlink(tmpf))
write_sigs(pkg2env(graphics), tmpf[1])
\dontrun{
shell(tmpf[1], wait = FALSE)
}
#Just functions beginning with 'a'.
write_sigs(pkg2env(graphics), tmpf[2], pattern = "^a")
\dontrun{
shell(tmpf[2], wait = FALSE)
}
#From a file
e <- new.env()
sys.source(
  system.file("extdata", "sample.R", package = "sig"),
  envir = e
)
write_sigs(e, tmpf[3])
\dontrun{
shell(tmpf[3], wait = FALSE)
}
}

