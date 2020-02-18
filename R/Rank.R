Rank.data.frame <-function(object,x,by,
                           ties.method="min",inverseOrder=FALSE,...){
  call=match.call(expand.dots = T)  
  if (missing(by)){
    res=bind_rows(lapply(split(object,object[[attr(object,"case")]]), function(object.case) 
                              cbind(object.case,
                                    rank=rankNA2(object.case[[x]],ties.method = ties.method,inverseOrder = inverseOrder))
                         )
                  )
   } else {
    if (length(by)==1) by=object[,by] else by=as.list(object[,by])
    byAnnotator=split(object,by) #  xx=split(object,lapply(by,function(z) object[,z]))
    res=bind_rows(lapply(byAnnotator,function(annotator){
       bind_rows(lapply(split(annotator,annotator[[attr(object,"case")]]), function(annotator.case) 
                                cbind(annotator.case,
                                      rank=rankNA2(annotator.case[[x]],ties.method = ties.method,inverseOrder = inverseOrder))
                        )
                 )
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
    
    matlist=lapply(object, function(task){
      if (by.missing){
         res=bind_rows(lapply(split(task,task[[attr(object,"case")]]),function(task.case) 
          cbind(task.case,rank=rankNA2(task.case[[x]],ties.method = ties.method,inverseOrder = inverseOrder))))
        class(res)[2]="ranked"
        res
      } else {
        byAnnotator=split(task,as.list(task[,by])) #  xx=split(object,lapply(by,function(z) object[,z]))
        #if (length(by)==1) xx=split(object,object[,by])
        #else xx=split(object,as.list(object[,by]))
        temp=bind_rows(lapply(byAnnotator,function(annotator){
           bind_rows(lapply(split(annotator,annotator[[attr(object,"case")]]),function(annotator.case) 
            cbind(annotator.case,rank=rankNA2(annotator.case[[x]],ties.method = ties.method,inverseOrder = inverseOrder))))
          
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


