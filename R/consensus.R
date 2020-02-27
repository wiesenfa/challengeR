consensus <- function(object,...) UseMethod("consensus")
consensus.default <- function(object, ...) stop("not implemented for this class")

consensus.ranked.list=function(object,
                               method,
                               ...){
  relensemble= relation_ensemble(list = as.relation(object)) 
  cons=relation_consensus(relensemble, 
                          method = method,...) # consensus ranking according to mean ranks across tasks if method="euclidean. 
  # See ?relation_consensus for different methods to derive consensus ranking
  res=sort(relation_scores(cons,
                           decreasing=FALSE)) # note that there may be ties (i.e. some algorithms have identical mean rank)
  attr(res,"method")=method
  res
  }
