rank.aggregated.list <-function(object,
                                ties.method="min",
                                largeBetter,
                                ...){
  
  call=match.call(expand.dots = F)
  if (missing(largeBetter)){
    if (!is.null(attr(object$data,"largeBetter"))) largeBetter=attr(object$data,"largeBetter")
    else stop("largeBetter has to be provided either in as.challenge() or rank()")

    if (object$isSignificance) largeBetter=TRUE  # smallBetter (largeBetter) already taken care of by one-sided test nature of signficance
  }  
  
  call=call("rank.aggregated.list",
            object=call$object,
            ties.method=ties.method,
            largeBetter=largeBetter)
  
  matlist=object$matlist

  matlist=lapply(matlist, 
                 function(y){
                   if (nrow(y)>0) r=rankNA2(y[,ncol(y)],
                                            ties.method=ties.method,
                                            largeBetter=largeBetter)
                   else r=NULL
                   res=cbind(y,rank=r)
                   res
                   })
  
  res=list(matlist=matlist,
           data=object$data,
           call=list(object$call,call),
           FUN =  . %>% (object$FUN) %>%  (call),
           FUN.list=c(object$FUN.list,
                      "rank")
      )
  class(res)=c("ranked.list",class(res))

  res
}

rank.aggregatedRanks.list <-function(object,
                                     ties.method="min",
                                     ...){
  
  call=match.call(expand.dots = F)  
  call=call("rank.aggregatedRanks.list",
            object=call$object,
            ties.method=ties.method)
  matlist=object$matlist

  matlist=lapply(matlist, function(y){
        if (nrow(y)>0) r=rankNA2(y[,ncol(y)],
                                 ties.method=ties.method,
                                 largeBetter=FALSE)
        else r=NULL
        res=cbind(y,rank=r)
        res
  })
  
  res=list(matlist=matlist,
           data=object$data,
           call=list(object$call,call),
           FUN =  . %>% (object$FUN) %>%  (call),
           FUN.list=c(object$FUN.list,
                      "rank")
           )
  class(res)=c("ranked.list",class(res))
  res

  res
}
