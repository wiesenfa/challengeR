subset.comparedRanks.list=function(x,
                                   tasks,...){
  res=x[tasks]
  class(res)="comparedRanks.list"
  res
}

subset.list=function(x,
                     tasks,...){
  x[tasks]
}

subset.aggregated.list=function(x,
                                tasks,...){
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


which.top=function(object,
                   top){
  mat=object$mat[object$mat$rank<=top,]
  rownames(mat)#[order(mat$rank)]
}

subset.ranked.list=function(x,
                            top,...) {

  if (length(x$matlist) != 1) {
    stop("Subset of algorithms only sensible for single-task challenges.")
  }

  taskMat <- x$matlist[[1]]
  taskData <- x$data[[1]]
  objectTop=x
  objectTop$matlist[[1]]=taskMat[taskMat$rank<=top,]

  taskMatRowNames <- rownames(objectTop$matlist[[1]])
  attribute <- attr(objectTop$data,"algorithm")

  selectedRowNames <- taskData[[attribute]] %in% taskMatRowNames
  objectTop$data[[1]] <- taskData[selectedRowNames,]
  objectTop$data[[1]][[attribute]] <- droplevels(objectTop$data[[1]][[attribute]])

  objectTop$fulldata=x$data
  objectTop
}


subset.bootstrap.list=function(x,
                               top,...) {

  if (length(x$matlist) != 1) {
    stop("Subset of algorithms only sensible for single-task challenges.")
  }

  objectTop <- subset.ranked.list(x, top = top)

  objectTop$bootsrappedRanks[[1]] <- objectTop$bootsrappedRanks[[1]][rownames(objectTop$matlist[[1]]),]
  objectTop$bootsrappedAggregate[[1]] <- objectTop$bootsrappedAggregate[[1]][rownames(objectTop$matlist[[1]]),]
  objectTop
}

