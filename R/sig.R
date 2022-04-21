#' Generate a function signature object
#' 
#' Generates a signature object for a function.
#' 
#' @param fn A function.
#' @param name_override Override the default function name.  
#' See examples.
#' @param ... For possible additional future arguments, currently unused.
#' @return A list, with the elements
#' \itemize{
#'   \item{name}{The name of the function.}
#'   \item{args}{The arguments of the function.}
#' }
#' @note Anonymous functions are given the name "..anonymous..". 
#' 
#' Nonstandard names ("foo bar"), assignment fns ("foo<-"),
#' operators ("%foo%") and reserved names ("repeat") are wrapped
#' in backquotes.
#' @examples
#' sig(R.Version)               #no args
#' sig(scan)                    #lots of args
#' sig(function(x, y) {x + y})  #anonymous
#' sig(sum)                     #primitive
#' 
#' sig("sd")                   #string input
#' sig("function(x, y) {x + y}")
#' sig(~ prod)                 #formula input
#' sig(~ function(x, y) {x + y})
#' sig(quote(paste0))          #name input
#' sig(quote(function(x, y) {x + y}))
#' 
#' fn_list <- list(
#'   mean = mean, 
#'   var = var
#' )
#' lapply(fn_list, sig)         #names are a mess
#' Map(                         #use Map for lists
#'   sig, 
#'   fn_list, 
#'   names(fn_list)             #Map mangles names, so override
#' )            
#' @export
sig <- function(fn, name_override, ...) 
{
  UseMethod("sig")
}

#' @rdname sig  
#' @method sig default
#' @export
sig.default <- function(fn, ...)
{
  sig_impl(fn, ...)
}

#' @rdname sig  
#' @method sig character
#' @export
sig.character <- function(fn, ...)
{
  name_override <- fn
  fn <- try(get(fn, mode = "function"))
  if(inherits(fn, "try-error"))
  {
    # Anon fn in a string
    fn <- eval(parse(text = name_override))
    name_override <- "..anonymous"
  }
  
  sig_impl(fn, name_override)
}

#' @rdname sig  
#' @method sig call
#' @export
sig.call <- function(fn, ...)
{
  # Probably anon fn in formula or quote
  fn <- eval(fn)
  name_override <- "..anonymous.."
  sig(fn, name_override)
}

#' @rdname sig  
#' @method sig formula
#' @export
sig.formula <- function(fn, ...)
{
  fn <- as.list(fn)[[2]]
  sig(fn, ...)
}

#' @rdname sig  
#' @method sig name
#' @export
sig.name <- function(fn, ...)
{
  name_override <- deparse(fn)
  fn <- get(name_override, mode = "function")
  sig_impl(fn, name_override)
}

sig_impl <- function(fn, name_override)
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
    fn_name <- deparse(substitute(fn))[1]
    fn_name <- fix_fn_names(fn_name)
  }
  # formals returns NULL for primitive functions. Need to use formals(args(fn)).
  if(is.primitive(fn))
  {
    fn <- args(fn)
  }
  structure(
    list(
      name = fn_name,
      args = as.list(formals(fn))
    ),
    class = c("sig", "list")
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
#' @param x An object of class \code{sig}.
#' @param width Width of string to display.
#' @param exdent Non-negative integer specifying the indentation 
#' of subsequent lines in the string.
#' @param ... Passed to \code{toString}
#' @return \code{toString} creates a string representation of a
#' function signature. 
#' \code{print} is mostly invoked for the side effect of printing 
#' a function 
#' signature, invisibly returning its input.
#' @examples
#' print_default_sig <- sig(print.default)
#' print(print_default_sig)
#' print(print_default_sig, width = 40)
#' print(print_default_sig, width = 40, exdent = 2)
#' toString(print_default_sig)
#' @export
print.sig <- function(x, width = getOption("width"), exdent = nchar(x$name), ...)
{
  print_engine(x, width, exdent, ...)
}
