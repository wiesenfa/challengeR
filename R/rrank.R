rank.challenge=function(object,x,
         ties.method="min",...){
  call=as.list(match.call())
  if (!is.null(attr(object,"annotator"))) {
  call2=call("Rank",object=call$object, x=attr(object,"value"), 
             annotator=c(attr(object,"annotator")),
         ties.method=ties.method,largeBetter=attr(object,"largeBetter") 
           )
  res1=do.call("Rank",list(object=object,x=attr(object,"value"),
                           annotator=c(attr(object,"annotator")),
         ties.method=ties.method,largeBetter=attr(object,"largeBetter") 
         ))
    
  } else {
  call2=call("Rank",object=call$object, x=attr(object,"value"), 
         ties.method=ties.method,largeBetter=attr(object,"largeBetter") 
           )
  res1=do.call("Rank",list(object=object,x=attr(object,"value"),
         ties.method=ties.method,largeBetter=attr(object,"largeBetter") 
         ))
    
  }
  
  if (inherits(object,"list")){    
    res=list(FUN = . %>% (call2),
                call=list(call2),
             FUN.list=list("rank"),
          data=object,
           matlist=res1$matlist)
    
    class(res)=c("ranked.list",class(res))
  } else {
    res=list(FUN = . %>% (call2),
                call=list(call2),
             FUN.list=list("rank"),
          data=object,
           mat=res1$mat)
    
    class(res)=c("ranked",class(res))
    
  }  
  res
}


