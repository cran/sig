% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sig_report.R
\name{sig_report}
\alias{sig_report}
\alias{sig_report.default}
\alias{sig_report.environment}
\alias{sig_report.character}
\alias{print.sigreport}
\title{Summarise function complexity of a file or environment}
\usage{
sig_report(x, ...)

\method{sig_report}{default}(x, ...)

\method{sig_report}{environment}(
  x,
  too_many_args = 10,
  too_many_lines = 50,
  length_metric = c("deparse", "body"),
  ...
)

\method{sig_report}{character}(x, ...)

\method{print}{sigreport}(x, ...)
}
\arguments{
\item{x}{A path to an R file or an environment.}

\item{...}{Passed to \code{sig_report.environment}.}

\item{too_many_args}{Upper bound for a sensible number of args.}

\item{too_many_lines}{Upper bound for a sensible number of lines.}

\item{length_metric}{Either \code{"deparse"} or \code{"body"}.  See note.}
}
\value{
An object of class ``sigreport'' with the elements:
\itemize{
  \item{n_vars}{Number of variables.}
  \item{n_fns}{Number of functions.}                            
  \item{n_args}{Table of the number of args of each function.}
  \item{too_many_args}{Upper bound for a sensible number of args.}
  \item{fns_with_many_args}{Names of each function with more args 
    than \code{too_many_args}.}
  \item{n_lines}{Table of the number of lines of each function body.} 
  \item{too_many_lines}{Upper bound for a sensible number of lines.}
  \item{long_fns}{Names of each function with more lines than 
  \code{too_many_lines}.}
}
}
\description{
Summarise function complexity of a file or environment
}
\details{
\code{sig_report} summarises the number of input arguments and the 
number of lines of each function in an environment of file, and 
identifies problem files, in order to help you refactor your code.
If the input is a path to an R file, then that file is sourced into 
a new environment and and the report is generated from that.
The number of lines of code that a function takes up is subjective 
in R; this function gives you a choice of \code{length(deparse(fn))} or
\code{length(body(fn))}, depending upon the value of \code{length_metric}. 
The \code{body} metric tends to give smaller values than \code{deparse}, so 
you may want to reduce the \code{too_many_lines} argument.
}
\examples{
#Summarize function complexity in an environment
sig_report(pkg2env(stats))    
#Summarize function complexity in a file
\donttest{
# From a file
tmp <- tempfile(fileext = ".R")
dump("scan", tmp)
sig_report(tmp)
}   
# From an environment, adjusting the cutoff for reporting
sig_report(              
  baseenv(),  
  too_many_args  = 20,    
  too_many_lines = 100
)      
# Alternate length metric          
sig_report(baseenv(), length_metric = "body")
}
