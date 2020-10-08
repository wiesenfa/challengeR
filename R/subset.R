subset <- function(x,...) UseMethod("subset")
subset.default <- function(x, ...) base::subset(x, ...)


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



subset.ranked.list <- function(x,
                               top,
                               tasks,...) {
  
#  if (!missing(tasks) & length(x$matlist) == 1) stop("Subset of tasks only sensible for multi task challenges.")
  if (!missing(top) & length(x$matlist) != 1)  stop("Subset of algorithms only sensible for single-task challenges. Otherwise no consensus ranking is possible.")
  
  if (!missing(top)){
    taskMat <- x$matlist[[1]]
    taskData <- x$data[[1]]
    objectTop=x
    objectTop$matlist[[1]]=taskMat[taskMat$rank<=top,]
    
    taskMatRowNames <- rownames(objectTop$matlist[[1]])
    attribute <- attr(objectTop$data,"algorithm")
    
    selectedRowNames <- taskData[[attribute]] %in% taskMatRowNames
    objectTop$data[[1]] <- taskData[selectedRowNames,]
    if (is.factor(objectTop$data[[1]][[attribute]])) objectTop$data[[1]][[attribute]] <- droplevels(objectTop$data[[1]][[attribute]])
    
    objectTop$fulldata=x$data
    return(objectTop)
  } else if (!missing(tasks)){
    
    if (is.character(tasks) && any(!tasks%in%names(x$matlist))) {
      stop("There is/are no task(s) called ",paste(tasks[!tasks%in%names(x$matlist)],collapse = " and "),".")  
    }
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
    return(res)
  }
}




subset.bootstrap.list=function(x,
                               top,
                               tasks, ...) {
  
 # if (!missing(tasks) & length(x$matlist) == 1) stop("Subset of tasks only sensible for multi task challenges.")
  if (!missing(top) & length(x$matlist) != 1)  stop("Subset of algorithms only sensible for single-task challenges. Otherwise no consensus ranking is possible.")
  
  if (!missing(top)){
    objectTop <- subset.ranked.list(x, top = top)
    
    objectTop$bootsrappedRanks[[1]] <- objectTop$bootsrappedRanks[[1]][rownames(objectTop$matlist[[1]]),]
    objectTop$bootsrappedAggregate[[1]] <- objectTop$bootsrappedAggregate[[1]][rownames(objectTop$matlist[[1]]),]
    return(objectTop)
  } else if (!missing(tasks)){
    if (is.character(tasks) && any(!tasks%in%names(x$matlist))) {
      stop("There is/are no task(s) called ",paste(tasks[!tasks%in%names(x$matlist)],collapse = " and "),".")  
    }
    
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
    return(res)
  }
}


