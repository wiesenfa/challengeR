select.if.comparedRanks.list=function(object,FUN,...){
  #if (!missing(FUN)) 
    res=object[sapply(object, function(x) do.call(FUN,args=list(x=x$mat)))]
  #if (!missing(which)) res=object[which]
   class(res)="comparedRanks.list"
  res
}

select.if.list=function(object,FUN,...){
  res=object[sapply(object, function(x) do.call(FUN,args=list(x=x)))]
  res
}



select.if.aggregated.list=select.if.ranked.list=function(object,FUN,...){
  call=match.call(expand.dots = T)  
  matlist=object$matlist
  #if (!missing(FUN)) 
    matlist=matlist[sapply(matlist, function(x) do.call(FUN,args=list(x=x)))]
  #if (!missing(which)) matlist=matlist[which]
  
  res=list(matlist=matlist,
           call=list(object$call,call),
           data=object$data,
       FUN =  . %>% (object$FUN) %>%  (call)
      )

  class(res)=class(object)
  res
    
}


