\name{fix_fn_names}
\alias{fix_fn_names}
\title{Fix names for sigs}
\usage{
  fix_fn_names(fn_name)
}
\arguments{
  \item{fn_name}{A character vector.}
}
\value{
  A character vector.
}
\description{
  Make anonymous functions and special functions safe.
}
\note{
  Strings beginning with ``function'' are given the value
  \code{"..anonymous.."}.

  Special function names are wrapped in backquotes.
}
\examples{
\dontrun{
fix_fn_names(c("\%foo\%", "?", "foo bar", "repeat", "function"))
}
}

