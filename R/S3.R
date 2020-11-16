utils::globalVariables(c("."))

"+.ggList" <- function (e1, e2){
  pp <- e1
  if(is.ggplot(pp)) plotList <- list(pp)
  else if(is.list(pp)) plotList <- pp
  else stop("Can't handle an object of class ", class(pp))

  for(i in 1:length(plotList)){
    p <- plotList[[i]]
    if(is.ggplot(p)) plotList[[i]] <- p + e2
  }

  if(is.ggplot(pp)) plotList[[1]]
  else plotList
}

"%++%" <- `+.ggList`

print.ranked.list <-function(x,...)  print(x$matlist, ...)
print.aggregated.list <-function(x,...)  print(x$matlist, ...)
print.aggregated <-function(x,...)  print(x$mat,...)
print.ranked <-function(x,...)  print(x$mat[order(x$mat$rank),],...)
print.ggList <- function(x, ...) {
  for(i in 1:length(x)) print(x[[i]])
}
