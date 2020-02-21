subset.comparedRanks.list=function(x,tasks,...){
  res=x[tasks]
  class(res)="comparedRanks.list"
  res
}

subset.list=function(x,tasks,...){
  x[tasks]
}

subset.bootstrap.list=function(x,tasks,...){
  if (!is.null(as.list(match.call(expand.dots = T))$top)) stop("Subset of algorithms only sensible for single task challenges.")
  res=list(bootsrappedRanks=x$bootsrappedRanks[tasks],
           bootsrappedAggregate=x$bootsrappedAggregate[tasks],
           matlist=x$matlist[tasks],
           data=x$data[tasks],
           FUN=x$FUN
  )
  
  attrib=attributes(x$data)
  attrib$names=attr(res$data,"names")
  attributes(res$data)=attrib
  class(res)="bootstrap.list"
  res
  
}

subset.ranked.list=function(x,tasks,...){
  if (!is.null(as.list(match.call(expand.dots = T))$top)) stop("Subset of algorithms only sensible for single task challenges.")
  res=list(matlist=x$matlist[tasks],
           data=x$data[tasks],
           call=x$call,
           FUN=x$FUN,
           FUN.list=x$FUN.list
  )
  
  attrib=attributes(x$data)
  attrib$names=attr(res$data,"names")
  attributes(res$data)=attrib
  class(res)=c("ranked.list","list")
  res
  
}

subset.aggregated.list=function(x,tasks,...){
  call=match.call(expand.dots = T)  
  if (!is.null(as.list(call$top))) stop("Subset of algorithms only sensible for single task challenges.")
  matlist=x$matlist[tasks]
  res=list(matlist=matlist,
           call=list(x$call,call),
           data=x$data,
       FUN =  . %>% (x$FUN) %>%  (call)
      )

  class(res)=class(x)
  res
 
}


which.top=function(object,top){
  mat=object$mat[object$mat$rank<=top,]
  rownames(mat)#[order(mat$rank)]
}

subset.ranked=function(x,top,...){
  objectTop=x
  objectTop$mat=objectTop$mat[objectTop$mat$rank<=top,]
  objectTop$data=objectTop$data[objectTop$data[[attr(objectTop$data,"algorithm")]]%in% rownames(objectTop$mat),]
  objectTop$fulldata=x$data
  objectTop
}


subset.bootstrap=function(x,top,...){
  objectTop=x
  objectTop$mat=objectTop$mat[objectTop$mat$rank<=top,]
  objectTop$data=objectTop$data[objectTop$data[[attr(objectTop$data,"algorithm")]]%in% rownames(objectTop$mat),]
  objectTop$fulldata=x$data
  objectTop$bootsrappedRanks=objectTop$bootsrappedRanks[rownames(objectTop$mat),]
  objectTop$bootsrappedAggregate=objectTop$bootsrappedAggregate[rownames(objectTop$mat),]
  objectTop
}

