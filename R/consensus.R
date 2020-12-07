#' @export
consensus <- function(object,...) UseMethod("consensus")

#' @export
consensus.default <- function(object, ...) stop("not implemented for this class")

#' Computes a consensus ranking
#'
#' Computes a consensus ranking (rank aggregation) across tasks.
#'
#' @param object The ranked asssessment data set.
#' @param method A string specifying the method to derive the consensus ranking, see [relations::consensus()] for the methods. Consensus ranking according to mean ranks across tasks if method="euclidean" where in case of ties (equal ranks for multiple algorithms) the average rank is used, i.e. ties.method="average".
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#' @export
consensus.ranked.list=function(object,
                               method,
                               ...){
  relensemble= relation_ensemble(list = as.relation(object))
  cons=relation_consensus(relensemble,
                          method = method,...) # consensus ranking according to mean ranks across tasks if method="euclidean".
  # See ?relation_consensus for different methods to derive consensus ranking
  res=sort(relation_scores(cons,
                           decreasing=FALSE)) # note that there may be ties (i.e. some algorithms have identical mean rank)
  attr(res,"method")=method
  res
  }
