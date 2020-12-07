winner <- function(x,...) UseMethod("winner")
winner.default <- function(x, ...) stop("not implemented for this class")

winner.ranked.list <-winner.bootstrap.list <-function(x,...){
  lapply(x$matlist, function(z) z[which(z$rank==min(z$rank)),,drop=F])
}




