\name{backquote}
\alias{backquote}
\title{Wrap in backquotes}
\usage{
  backquote(x)
}
\arguments{
  \item{x}{A character vector.}
}
\value{
  A character vector.
}
\description{
  Wraps strings in backquotes.
}
\note{
  Existing backquote characters are escaped with a
  backslash.
}
\examples{
\dontrun{
backquote(c("foo bar", "a`b`c"))
}
}
\seealso{
  \code{\link[base]{sQuote}}
}

