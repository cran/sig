#' Generate a function signature object
#' 
#' Generates a signature object for a function.
#' 
#' @param fn A function.
#' @param name_override Override the default function name.  See examples.
#' @return A list, with the elements
#' \itemize{
#'   \item{name}{The name of the function.}
#'   \item{args}{The arguments of the function.}
#' }
#' @note Anonymous functions are given the name "..anonymous..". 
#' @examples
#' sig(R.Version)               #no args
#' sig(scan)                    #lots of args
#' sig(function(x, y) {x + y})  #anonymous
#' fn_list <- list(
#'   mean = mean, 
#'   var = var
#' )
#' lapply(fn_list, sig)         #names are a mess
#' mapply(                      #use mapply for lists
#'   sig, 
#'   fn_list, 
#'   names(fn_list), 
#'   SIMPLIFY = FALSE
#' )            
#' @export
sig <- function(fn, name_override)
{
  if(!is.function(fn))
  {
    stop("sig requires a function input.")
  }
  if(!missing(name_override))
  {
    fn_name <- name_override[1]
  } else
  {
    fn_name <- deparse(substitute(fn))
    if(grepl("^function", fn_name[1]))
    {
      fn_name <- "..anonymous.."
    }
  }
  structure(
    list(
      name = fn_name,
      args = as.list(formals(fn))
    ),
    class = "sig"
  )
}

#' @rdname print.sig
#' @method toString sig
#' @export
toString.sig <- function(x, width = getOption("width"), exdent = nchar(x$name), ...)
{
  old_op <- options(useFancyQuotes = FALSE)
  on.exit(options(old_op))
  arguments_without_defaults <- names(x$args)
  arguments_with_defaults <- paste(
    names(x$args), 
    ifelse(
      vapply(x$args, is.character, logical(1)), 
      dQuote(x$args), 
      x$args
    ), 
    sep = " = "
  )  
  arguments <- paste(
    ifelse(
      vapply(x$args, is.name, logical(1)),
      arguments_without_defaults,
      arguments_with_defaults
    ),
    collapse = ", "
  )
  string <- paste0(x$name, " <- function(", arguments, ")")
  strwrap(string, width = width, exdent = exdent)
}

#' Print a sig object
#' 
#' Prints a function signature object.
#' 
#' @method print sig
#' @param x An object of class sig.
#' @param width Width of string to display.
#' @param exdent Non-negative integer specifying the indentation of subsequent lines
#' in the string.
#' @param ... Passed to \code{toString}
#' @return \code{toString} creates a string representation of a function signature. 
#' \code{print} is mostly invoked for the side effect of printing a function 
#' signature,invisibly returning its input.
#' @examples
#' spd <- sig(print.default)
#' print(spd)
#' print(spd, width = 40)
#' print(spd, width = 40, exdent = 2)
#' @export
print.sig <- function(x, width = getOption("width"), exdent = nchar(x$name), ...)
{
  cat(toString(x, ...), sep = "\n")
  invisible(x)
}
