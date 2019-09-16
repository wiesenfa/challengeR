consensus.ranked.list=function(object,method,...){
  relensemble= relation_ensemble(list = as.relation(object)) 
  cons=relation_consensus(relensemble, method = "euclidean",...) # consensus ranking according to mean ranks across tasks. 
  # See ?relation_consensus for different methods to derive consensus ranking
  sort(relation_scores(cons,decreasing=FALSE)) # note that there are ties (i.e. some algorithms have identical mean rank)
}
