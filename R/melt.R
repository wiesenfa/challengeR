melt.ranked.list=melt.aggregated.list=function(object,...){
  matlist=lapply(object$matlist, function(z){
    z$algorithm=rownames(z)
    z
  })
  melt(matlist,id.vars="algorithm",...)
}
