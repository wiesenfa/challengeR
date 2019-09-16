
rank.aggregated <-function(object,#x,
         ties.method="min",inverseOrder,...){
  call=match.call(expand.dots = T)  
  if (missing(inverseOrder)){
    if (!is.null(attr(object$data,"inverseOrder"))) inverseOrder=attr(object$data,"inverseOrder")
    else stop("inverseOrder has to be provided either in as.challenge() or rank()")
  }  
  mat=object$mat
  
  if (nrow(mat)>0) r=rankNA2(mat[,ncol(mat)],ties.method=ties.method,inverseOrder=inverseOrder)
  else r=NULL
  
  res=list(mat=cbind(mat,rank=r),
           data=object$data,
            call=list(object$call,call),
      FUN =  . %>% (object$FUN) %>%  (call)
      )
  class(res)=c("ranked",class(res))
  res
}



rank.aggregatedRanks <-function(object,#x,
         ties.method="min",...){
  call=match.call(expand.dots = T)  
  mat=object$mat
  
  if (nrow(mat)>0) r=rankNA2(mat[,ncol(mat)],ties.method=ties.method,inverseOrder=FALSE)
  else r=NULL
  
  res=list(mat=cbind(mat,rank=r),
           data=object$data,
            call=list(object$call,call),
      FUN =  . %>% (object$FUN) %>%  (call)
      )
  class(res)=c("ranked",class(res))
  res
}



