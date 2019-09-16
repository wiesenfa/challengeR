Rank.data.frame <-function(object,x,by,
                           ties.method="min",inverseOrder=FALSE,...){
  call=match.call(expand.dots = T)  
  if (missing(by)){
    r=rankNA2(object[,x],ties.method=ties.method,inverseOrder=inverseOrder)
    res=cbind(object,rank=r)
  } else {
    if (length(by)==1) by=object[,by] else by=as.list(object[,by])
    xx=split(object,by) #  xx=split(object,lapply(by,function(z) object[,z]))
    res=bind_rows(lapply(xx,function(xxx){
      r=rankNA2(xxx[,x],ties.method=ties.method,inverseOrder=inverseOrder)
      res=cbind(xxx,rank=r)
      res
    }
    ))
    
  }
  
  res=list(FUN = . %>% (call),
           call=list(call),
           data=object,
           mat=res)
  class(res)=c("ranked",class(res))
  
  res
}



Rank.list <-
  function(object,x,by,
           ties.method="min",inverseOrder=FALSE,...){
    
    call=match.call(expand.dots = T)  
    by.missing=missing(by)
    
    matlist=lapply(object, function(y){
      if (by.missing){
        r=rankNA2(y[,x],ties.method=ties.method,inverseOrder=inverseOrder)
        res=cbind(y,rank=r)
        class(res)[2]="ranked"
        res
      } else {
        xx=split(y,as.list(y[,by])) #  xx=split(object,lapply(by,function(z) object[,z]))
        #if (length(by)==1) xx=split(object,object[,by])
        #else xx=split(object,as.list(object[,by]))
        temp=bind_rows(lapply(xx,function(xxx){
          r=rankNA2(xxx[,x],ties.method=ties.method,inverseOrder=inverseOrder)
          res=cbind(xxx,rank=r)
          res
        }
        ))
        
        class(temp)[2]="ranked"
        temp
      }
    }  )
    res=list(FUN = . %>% (call),
             call=list(call),
             data=object,
             matlist=matlist)
    
    class(res)=c("ranked.list",class(res))
    res
   }


