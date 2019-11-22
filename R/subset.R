subset.comparedRanks.list=function(x,subset,...){
  res=x[subset]
  class(res)="comparedRanks.list"
  res
}

subset.list=function(x,subset,...){
  x[subset]
}

subset.bootstrap.list=function(x,subset,...){
  res=list(bootsrappedRanks=x$bootsrappedRanks[subset],
           bootsrappedAggregate=x$bootsrappedAggregate[subset],
           matlist=x$matlist[subset],
           data=x$data[subset],
           FUN=x$FUN
  )
  
  attrib=attributes(x$data)
  attrib$names=attr(res$data,"names")
  attributes(res$data)=attrib
  class(res)="bootstrap.list"
  res
  
}

subset.ranked.list=function(x,subset,...){
  res=list(matlist=x$matlist[subset],
           data=x$data[subset],
           call=x$call,
           FUN=x$FUN
  )
  
  attrib=attributes(x$data)
  attrib$names=attr(res$data,"names")
  attributes(res$data)=attrib
  class(res)="ranked.list"
  res
  
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
