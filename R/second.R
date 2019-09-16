second <-
function(x){
    x=x$mat
    res=x[x$rank==2,]
    res[,-which(colnames(res)=="rank")]
  }
