rank.aggregated.list <-function(object,#x,
         ties.method="min",inverseOrder,...){
  
  call=match.call(expand.dots = T)  
  if (missing(inverseOrder)){
    if (!is.null(attr(object$data,"inverseOrder"))) inverseOrder=attr(object$data,"inverseOrder")
    else stop("inverseOrder has to be provided either in as.challenge() or rank()")

    if (object$isSignificance) inverseOrder=TRUE  # smallBetter (inverseOrder) already taken care of by one-sided test nature of signficance
  }  
  matlist=object$matlist

  matlist=lapply(matlist, function(y){
        if (nrow(y)>0) r=rankNA2(y[,ncol(y)],ties.method=ties.method,inverseOrder=inverseOrder)
        else r=NULL
        res=cbind(y,rank=r)
        res
  })
  
  res=list(matlist=matlist,
           data=object$data,
            call=list(object$call,call),
      FUN =  . %>% (object$FUN) %>%  (call)
      )
  class(res)=c("ranked.list",class(res))
  res

  res
}

rank.aggregatedRanks.list <-function(object,
         ties.method="min",...){
  
  call=match.call(expand.dots = T)  
  matlist=object$matlist

  matlist=lapply(matlist, function(y){
        if (nrow(y)>0) r=rankNA2(y[,ncol(y)],ties.method=ties.method,inverseOrder=FALSE)
        else r=NULL
        res=cbind(y,rank=r)
        res
  })
  
  res=list(matlist=matlist,
           data=object$data,
            call=list(object$call,call),
      FUN =  . %>% (object$FUN) %>%  (call)
      )
  class(res)=c("ranked.list",class(res))
  res

  res
}
