rank.challenge=function(object,x,by,
         ties.method="min",...){
  call=as.list(match.call())
  # if (inherits(object,"list")) res=do.call("Rank.list",list(object=object,x=attr(object,"value"),by=c(attr(object,"annotator"),attr(object,"case")),FUN=call$FUN,
  #        ties.method=ties.method,inverseOrder=attr(object,"inverseOrder") 
  #        ))
  # else res=do.call("Rank.data.frame",list(object=object,x=attr(object,"value"),by=c(attr(object,"annotator"),attr(object,"case")),FUN=call$FUN,
  #        ties.method=ties.method,inverseOrder=attr(object,"inverseOrder") 
  #        ))
  if (!missing(by)) {
  call2=call("Rank",object=call$object, x=attr(object,"value"), #IS ALSO CASE NEEDED?:
#             by=c(attr(object,"annotator"),attr(object,"case")),
             by=c(attr(object,"annotator")),
         ties.method=ties.method,inverseOrder=attr(object,"inverseOrder") 
           )
  res1=do.call("Rank",list(object=object,x=attr(object,"value"),
                           by=c(attr(object,"annotator")),#IS ALSO CASE NEEDED?:
#                           by=c(attr(object,"annotator"),attr(object,"case")),
         ties.method=ties.method,inverseOrder=attr(object,"inverseOrder") 
         ))
    
  } else {
  call2=call("Rank",object=call$object, x=attr(object,"value"), 
         ties.method=ties.method,inverseOrder=attr(object,"inverseOrder") 
           )
  res1=do.call("Rank",list(object=object,x=attr(object,"value"),
         ties.method=ties.method,inverseOrder=attr(object,"inverseOrder") 
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


