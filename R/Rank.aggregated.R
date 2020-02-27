
rank.aggregated <-function(object,
         ties.method="min",largeBetter,...){
  call=match.call(expand.dots = F)
  if (missing(largeBetter)){
    if (!is.null(attr(object$data,"largeBetter"))) largeBetter=attr(object$data,"largeBetter")
    else stop("largeBetter has to be provided either in as.challenge() or rank()")
    
    if (object$isSignificance) largeBetter=TRUE  # smallBetter (largeBetter) already taken care of by one-sided test nature of signficance
    }  
  call=call("rank.aggregated",object=call$object,ties.method=ties.method,largeBetter=largeBetter)
  mat=object$mat
  
  if (nrow(mat)>0) r=rankNA2(mat[,ncol(mat)],ties.method=ties.method,largeBetter=largeBetter)
  else r=NULL
  
  res=list(mat=cbind(mat,rank=r),
           data=object$data,
            call=list(object$call,call),
      FUN =  . %>% (object$FUN) %>%  (call),
      FUN.list=c(object$FUN.list,"rank")
      )
  class(res)=c("ranked",class(res))
  res
}



rank.aggregatedRanks <-function(object,
         ties.method="min",...){
  call=match.call(expand.dots = F)  
  call=call("rank.aggregatedRanks",object=call$object,ties.method=ties.method)
  mat=object$mat
  
  if (nrow(mat)>0) r=rankNA2(mat[,ncol(mat)],ties.method=ties.method,largeBetter=FALSE)
  else r=NULL
  
  res=list(mat=cbind(mat,rank=r),
           data=object$data,
            call=list(object$call,call),
      FUN =  . %>% (object$FUN) %>%  (call),
      FUN.list=c(object$FUN.list,"rank")
      )
  class(res)=c("ranked",class(res))
  res
}



