utils::globalVariables(c("."))
 
rank <- function(object,...) UseMethod("rank")
rank.default <- function(object, ...) base::rank(object,...)  #stats::aggregate



test <- function(x,...) UseMethod("test")
test.default <- function(x, ...) stop("not implemented for this class")



compareRanks <- function(x,...) UseMethod("compareRanks")
compareRanks.default <- function(x, ...) stop("not implemented for this class")

rankFrequencies <- function(object,...) UseMethod("rankFrequencies")
rankFrequencies.default <- function(object, ...) stop("not implemented for this class")

winnerFrequencies <- function(object,...) UseMethod("winnerFrequencies")
winnerFrequencies.default <- function(object, ...) stop("not implemented for this class")

winner <- function(x,...) UseMethod("winner")
winner.default <- function(x, ...) stop("not implemented for this class")

# select <- function(object,...) UseMethod("select")
# select.default <- function(object, ...) stop("not implemented for this class")
 select.if <- function(object,...) UseMethod("select.if")
 select.if.default <- function(object, ...) stop("not implemented for this class")


#print.aggregated(x,...) print(as.data.frame(x),...)
print.ranked.list <-function(x,...)  print(x$matlist, ...)
print.aggregated.list <-function(x,...)  print(x$matlist, ...)
print.aggregated <-function(x,...)  print(x$mat,...)
print.ranked <-function(x,...)  print(x$mat[order(x$mat$rank),],...)



# density <- function(x,...) UseMethod("density")
# density.default <- function(x, ...) density.default(x,...)


decision <- function(x,...) UseMethod("decision")
decision.default <- function(x, ...) stop("not implemented for this class")

lineplot <- function(x,...) UseMethod("lineplot")
lineplot.default <- function(x, ...) stop("not implemented for this class")

methodsplot <- function(x,...) UseMethod("methodsplot")
methodsplot.default <- function(x, ...) stop("not implemented for this class")



relation_dissimilarity <- function(x,...) UseMethod("relation_dissimilarity")
relation_dissimilarity.default <- function(x, ...) relations::relation_dissimilarity(x,  ...) 





consensus <- function(object,...) UseMethod("consensus")
consensus.default <- function(object, ...) stop("not implemented for this class")



