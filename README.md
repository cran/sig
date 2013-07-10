sig prints R functions signatures and writes them to file.  (In this case, 
signature means the name and arguments, with possible defaults, of that 
function.) 

To load the package, type

    library(sig)

If you want to know how to call a function, just pass a function to the `sig`
function.  For example,

    sig(mean)
    #mean <- function(x, ...)
    sig(mean.default)
    #mean.default <- function(x, trim = 0, na.rm = FALSE, ...)

If you pass an anonymous function, it is given the name `..anonymous..`.

    sig(function(x, y) {x + y})
    #..anonymous.. <- function(x, y)
    
You can override the name of the function by passing a second argument.  This is
useful when using sig with an `*apply` function.

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
    
---

"Black Box" is a useful game for testing how maintainable your code is. You
give your friend or colleague the signatures of your functions and have them 
guess what the function contents.  For example, if you show them

    mean <- function(x, ...)
    
then they might be able to guess that the function calculates the arithmetic
mean of a numeric input.

If you didn't know what it was, the signature for the `lm` function doesn't
make it as clear what the function does

    sig(lm)
    #lm <- function(formula, data, subset, weights, na.action, method =
    #  "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok =
    #  TRUE, contrasts = NULL, offset, ...)
    
Your friend might guess that since the function takes a `formula` and a `data`
arguement that it is some kind of model.  Some of the other arguments may be
guessable.  "Oh, `weights` must let you run a weighted model!"  Beyond that, 
the function's purpose is difficult to determine without consulting 
documentation.

In general, if you can guess what a function does, and what its body should 
contain, based only on the signature, then that function will be easy to use
and easy to maintain.  By contrast, an unclear function signature provides
a warning that it may be difficult to use or maintain.

To make Black Box easy to play, use the `write_sigs` function to write all the 
functions from a file or R package to a text file.

    #From an environment
    write_sigs(
      pkg2env(graphics), 
      "graphics pkg sigs.R"
    )
    
    #From a file
    write_sigs(
      "my R file.R",
      "my sigs.R
    )
