\name{sig}
\alias{sig}
\title{Generate a function signature object}
\usage{
  sig(fn, name_override)
}
\arguments{
  \item{fn}{A function.}

  \item{name_override}{Override the default function name.
  See examples.}
}
\value{
  A list, with the elements \itemize{ \item{name}{The name
  of the function.} \item{args}{The arguments of the
  function.} }
}
\description{
  Generates a signature object for a function.
}
\note{
  Anonymous functions are given the name "..anonymous..".
}
\examples{
sig(R.Version)               #no args
sig(scan)                    #lots of args
sig(function(x, y) {x + y})  #anonymous
fn_list <- list(
  mean = mean,
  var = var
)
lapply(fn_list, sig)         #names are a mess
mapply(                      #use mapply for lists
  sig,
  fn_list,
  names(fn_list),
  SIMPLIFY = FALSE
)
}

