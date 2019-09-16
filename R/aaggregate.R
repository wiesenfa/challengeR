test.challenge=function(x,...) aggregate.challenge(x=x,FUN="significance",...)


aggregate.challenge=function(x,FUN=mean,
                             na.treat=0, #either "na.rm", numeric value or function
                             alpha=0.05,p.adjust.method="none",# only needed for significance 
                             parallel=FALSE,progress="none",...){
  call=as.list(match.call())
  res1=do.call("Aggregate",list(object=x,x=attr(x,"value"),algorithm=attr(x,"algorithm"),FUN=call$FUN,
                                na.treat=na.treat,
                                parallel=parallel,progress=progress,
                                dataset_id=attr(x,"case"), 
                                alpha=alpha, p.adjust.method=p.adjust.method,
                                inverseOrder=attr(x,"inverseOrder") # only needed for significance 
  ))
  
  call2=call("Aggregate",object=call$x, x=attr(x,"value"),algorithm=attr(x,"algorithm"),FUN=call$FUN,
             na.treat=na.treat,
             parallel=parallel,progress=progress,
             dataset_id=attr(x,"case"), 
             alpha=alpha, p.adjust.method=p.adjust.method, inverseOrder=attr(x,"inverseOrder") # only needed for significance 
  )
  if (inherits(x,"list")){    
    res=list(FUN = . %>% (call2),
             call=list(call2),
             data=x,
             matlist=res1$matlist)
    
    class(res)=c("aggregated.list",class(res))
  } else {
    res=list(FUN = . %>% (call2),
             call=list(call2),
             data=x,
             mat=res1$mat)
    
    class(res)=c("aggregated",class(res))
    
  }  
  res

}


aggregate.ranked <-function(x,
                            FUN=mean, ...                      ){
  algorithm=attr(x$data,"algorithm")
  mat=x$mat
  call=match.call(expand.dots = T)  
  what="rank"
  xmean <- aggregate(mat[,what], by=list(mat[,algorithm]), FUN=function(z) do.call(FUN,args=list(x=z,na.rm=TRUE)))
  names(xmean)=c(algorithm,paste0(what,"_",strsplit(capture.output(suppressWarnings(print(methods(FUN),byclass=T)))[1]," ")[[1]][2]))
  rownames(xmean)=xmean[,1]
  xmean=xmean[,-1,drop=F]
  res=list(FUN = . %>% (x$FUN) %>%  (call),
           call=list(x$call,call),
           data=x$data,
           mat=xmean)
  class(res)=c("aggregatedRanks",class(res))
  res
}



aggregate.ranked.list <-function(x,#algorithm,
                                 FUN=mean,         ...            ){
  call=match.call(expand.dots = T)  
  
  #if (missing(algorithm)) 
    algorithm=attr(x$data,"algorithm")
  resmatlist=Aggregate.list(x$matlist,x="rank",algorithm=algorithm,FUN=FUN,...)$matlist
  resmatlist=lapply(resmatlist,function(z) as.data.frame(z))
  res=list(matlist=resmatlist,
           call=list(x$call,call),
           data=x$data,
           FUN =  . %>% (x$FUN) %>%  (call)
  )
  class(res)=c("aggregatedRanks.list",class(res))
  res
  
}





aggregate.bootstrap.list <-function(x,what="metric",FUN=mean,
                                    ...            ){
  call=match.call(expand.dots = T)  
  if (is.character(FUN)) FUN=try(eval(parse(text=FUN)),silent = T)
  FUNname=as.character(call$FUN)
  
  if (!is.function(FUN)) stop("FUN has to be a function (possibly as character)")
  matlist=llply(1:length(x$bootsrappedRank), function(i.piece){ 
    if (what=="ranks") xmean <- as.data.frame(apply(x$bootsrappedRank[[i.piece]],1,FUN=FUN))
    else xmean <- as.data.frame(apply(x$bootsrappedAggregate[[i.piece]],1,FUN=FUN)) 
    names(xmean)=paste0(what,"_",FUNname)
    #    class(xmean)=c("aggregated",class(xmean))
    xmean
  }
  )
  
  
  names(matlist)=names(x$bootsrappedRank)
  res=list(FUN = . %>% (call),
           call=list(call),
           data=x,
           matlist=matlist)
  
  class(res)=c("aggregated.list",class(res))
  res
}

aggregate.bootstrap<-function(x,what="metric",FUN=mean,
                              ...            ){
  call=match.call(expand.dots = T)  
  if (is.character(FUN)) FUN=try(eval(parse(text=FUN)),silent = T)
  FUNname=as.character(call$FUN)
  
  if (!is.function(FUN)) stop("FUN has to be a function (possibly as character)")
  
  if (what=="ranks") xmean <- as.data.frame(apply(x$bootsrappedRank,1,FUN=FUN))
  else xmean <- as.data.frame(apply(x$bootsrappedAggregate,1,FUN=FUN)) 
  names(xmean)=paste0(what,"_",FUNname)
  #    class(xmean)=c("aggregated",class(xmean))
  res=list(FUN = . %>% (call),
           call=list(call),
           data=x,
           mat=xmean)
  
  class(res)=c("aggregated",class(res))
  res
}

