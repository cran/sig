#' Write sigs to file
#' 
#' Writes function signatures to a text file.
#' 
#' @param envir An environment containing functions.
#' @param file A file to write the output to.
#' @param pattern If not \code{NULL}, a regular expression to filter the function names by.
#' @param ... passed to \code{toString.sig}.
#' @return Nothing of interest.  Invoked for the side effect of writing 
#' function signatures to a file.
#' @examples
#' #From a package
#' tmpf <- tempfile(LETTERS[1:3], fileext = ".R")
#' on.exit(unlink(tmpf))
#' write_sigs(pkg2env(graphics), tmpf[1])
#' \dontrun{
#' shell(tmpf[1], wait = FALSE)
#' }
#' #Just functions beginning with 'a'.
#' write_sigs(pkg2env(graphics), tmpf[2], pattern = "^a")
#' \dontrun{
#' shell(tmpf[2], wait = FALSE)
#' }
#' #From a file
#' e <- new.env()
#' sys.source(
#'   system.file("extdata", "sample.R", package = "sig"),
#'   envir = e
#' )
#' write_sigs(e, tmpf[3])
#' \dontrun{
#' shell(tmpf[3], wait = FALSE)
#' }              
#' @export
write_sigs <- function(envir, file, pattern = NULL, ...)
{
  fns <- Filter(is.function, as.list(envir))
  if(!is.null(pattern))
  {
    fns <- fns[grepl(pattern, names(fns))]
  }
  o <- order(names(fns))
  fns <- fns[o]
  sigs <- mapply(
    sig,
    fns,
    names(fns),
    SIMPLIFY = FALSE
  )  
  strings <- unlist(lapply(sigs, function(s) c(toString(s, ...), "")))
  writeLines(strings, file)
}

#' Get environment of a package.
#' 
#' Utility function to get the environment of a package on the search path.
#' 
#' @param pkg A package.
#' @return the environment corresponding to \code{pkg}.
#' @examples
#' pkg2env(graphics)
#' @export
pkg2env <- function(pkg) 
{
  pkg_name <- deparse(substitute(pkg))
  if(!pkg_name %in% .packages())
  {
    if(pkg_name %in% .packages(TRUE))
    {
      message("Loading package ", sQuote(pkg_name), ".")
      library(pkg_name, character.only = TRUE)
    } else
    {
      stop("The package ", sQuote(pkg_name), " is not available.")
    }
  }
  as.environment(paste0("package:", pkg_name))
}
