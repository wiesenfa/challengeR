subset.comparedRanks.list=function(x,subset,...){
  res=x[subset]
  class(res)="comparedRanks.list"
  res
}

subset.list=function(x,subset,...){
  x[subset]
}


subset.aggregated.list=subset.ranked.list=function(x,subset,...){
  call=match.call(expand.dots = T)  
  matlist=x$matlist[subset]
  res=list(matlist=matlist,
           call=list(x$call,call),
           data=x$data,
       FUN =  . %>% (x$FUN) %>%  (call)
      )

  class(res)=class(x)
  res
 
}
