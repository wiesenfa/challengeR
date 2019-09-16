utils::globalVariables(c("."))
 
rank <- function(object,...) UseMethod("rank")
rank.default <- function(object, ...) base::rank(object,...)  #stats::aggregate

Rank <- function(object,...) UseMethod("Rank")
Rank.default <- function(object, ...) rank(object,...)  #base::rank

Aggregate <- function(object,...) UseMethod("Aggregate")
Aggregate.default <- function(object, ...) aggregate(object,...)  #stats::aggregate

test <- function(x,...) UseMethod("test")
test.default <- function(x, ...) stop("not implemented for this class")


bootstrap <- function(object,...) UseMethod("bootstrap")
bootstrap.default <- function(object, ...) stop("not implemented for this class")

compareRanks <- function(x,...) UseMethod("compareRanks")
compareRanks.default <- function(x, ...) stop("not implemented for this class")

rankFrequencies <- function(object,...) UseMethod("rankFrequencies")
rankFrequencies.default <- function(object, ...) stop("not implemented for this class")

winnerFrequencies <- function(object,...) UseMethod("winnerFrequencies")
winnerFrequencies.default <- function(object, ...) stop("not implemented for this class")

Boxplot <- function(x,...) UseMethod("Boxplot")
Boxplot.default <- function(x, ...) stop("not implemented for this class")

ggBoxplot <- function(x,...) UseMethod("ggBoxplot")
ggBoxplot.default <- function(x, ...) stop("not implemented for this class")

winner <- function(x,...) UseMethod("winner")
winner.default <- function(x, ...) stop("not implemented for this class")

# select <- function(object,...) UseMethod("select")
# select.default <- function(object, ...) stop("not implemented for this class")
 select.if <- function(object,...) UseMethod("select.if")
 select.if.default <- function(object, ...) stop("not implemented for this class")


#print.aggregated(x,...) print(as.data.frame(x),...)
print.ranked.list <-function(x,...)  print(x$matlist, ...)
print.aggregated.list <-function(x,...)  print(x$matlist, ...)
print.aggregated <-function(x,...)  print(x$mat, ...)
print.ranked <-function(x,...)  print(x$mat, ...)


podium <- function(x,...) UseMethod("podium")
podium.default <- function(x, ...) stop("not implemented for this class")

#density <- function(x,...) UseMethod("density")
#density.default <- function(x, ...) density.default(x,...)


decision <- function(x,...) UseMethod("decision")
decision.default <- function(x, ...) stop("not implemented for this class")

lineplot <- function(x,...) UseMethod("lineplot")
lineplot.default <- function(x, ...) stop("not implemented for this class")

methodsplot <- function(x,...) UseMethod("methodsplot")
methodsplot.default <- function(x, ...) stop("not implemented for this class")

violin <- function(x,...) UseMethod("violin")
violin.default <- function(x, ...) stop("not implemented for this class")

stability2 <- function(x,...) UseMethod("stability2")
stability2.default <- function(x, ...) stop("not implemented for this class")

relation_dissimilarity <- function(x,...) UseMethod("relation_dissimilarity")
relation_dissimilarity.default <- function(x, ...) relations::relation_dissimilarity(x,  ...) 

rankingHeatmap <- function(x,...) UseMethod("rankingHeatmap")
rankingHeatmap.default <- function(x, ...) stop("not implemented for this class")

stability1 <- function(x,...) UseMethod("stability1")
stability1.default <- function(x, ...) stop("not implemented for this class")

report <- function(object,...) UseMethod("report")
report.default <- function(object, ...) stop("not implemented for this class")

consensus <- function(object,...) UseMethod("consensus")
consensus.default <- function(object, ...) stop("not implemented for this class")



