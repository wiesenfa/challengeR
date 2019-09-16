winner.ranked <-winner.bootstrap <-function(x,...){
  x=x$mat
 #   res=x[x$rank==1,]
    res=x[which(x$rank==min(x$rank)),,drop=F]
 #   res[,-which(colnames(res)=="rank")]
    res
}


winner.ranked.list <-winner.bootstrap.list <-function(x,...){
  lapply(x$matlist, function(z) z[which(z$rank==min(z$rank)),,drop=F])
}




