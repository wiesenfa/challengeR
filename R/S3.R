utils::globalVariables(c("."))
 
print.ranked.list <-function(x,...)  print(x$matlist, ...)
print.aggregated.list <-function(x,...)  print(x$matlist, ...)
print.aggregated <-function(x,...)  print(x$mat,...)
print.ranked <-function(x,...)  print(x$mat[order(x$mat$rank),],...)



# density <- function(x,...) UseMethod("density")
# density.default <- function(x, ...) density.default(x,...)















