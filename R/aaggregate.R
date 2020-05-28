test <- function(x,...) UseMethod("test")
test.default <- function(x, ...) stop("not implemented for this class")
test.challenge=function(x,...) aggregate.challenge(x=x,
                                                   FUN="significance",...)


#' Title
#'
#' @param x
#' @param FUN
#' @param na.treat
#' @param alpha
#' @param p.adjust.method
#' @param parallel
#' @param progress
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
aggregate.challenge=function(x,
                             FUN=mean,
                             na.treat, #either "na.rm", numeric value or function
                             alpha=0.05, p.adjust.method="none",# only needed for significance
                             parallel=FALSE,
                             progress="none",...){
  call=as.list(match.call())

  if (missing(na.treat)){ #na.treat only optional if no missing values in data set
    if (!inherits(x,"list")){
      if (!any(is.na(x[,attr(x, "value")]))) na.treat="na.rm" # there are no missings so set na.treat by dummy "na.rm" has no effect
    } else {
      if (!any(sapply(x,
                      function(task) any(is.na(task[,attr(x, "value")]))))) na.treat="na.rm" # there are no missings so set na.treat by dummy "na.rm" has no effect
    }
  }

  res1=do.call("Aggregate",list(object=x,
                                x=attr(x,"value"),
                                algorithm=attr(x,"algorithm"),
                                FUN=call$FUN,
                                na.treat=na.treat,
                                parallel=parallel,
                                progress=progress,
                                case=attr(x,"case"),
                                alpha=alpha, p.adjust.method=p.adjust.method,
                                largeBetter=attr(x,"largeBetter") # only needed for significance
  ))

  call2=call("Aggregate",
             object=call$x,
             x=attr(x,"value"),
             algorithm=attr(x,"algorithm"),
             FUN=call$FUN,
             na.treat=na.treat,
             parallel=parallel,progress=progress,
             case=attr(x,"case"),
             alpha=alpha, p.adjust.method=p.adjust.method,
             largeBetter=attr(x,"largeBetter") # only needed for significance
  )

  if (inherits(x,"list")){
    res=list(FUN = . %>% (call2),
             call=list(call2),
             FUN.list=list(FUN),
             data=x,
             matlist=res1$matlist,
             isSignificance=res1$isSignificance)

    class(res)=c("aggregated.list",class(res))
  } else {
    res=list(FUN = . %>% (call2),
             call=list(call2),
             FUN.list=list(FUN),
             data=x,
             mat=res1$mat,
             isSignificance=res1$isSignificance)

    class(res)=c("aggregated",class(res))

  }
  res

}


aggregate.ranked.list <-function(x,
                                 FUN=mean,
                                 ...){
  call=match.call(expand.dots = F)
  call=call("aggregate.ranked.list",
            x=call$x,
            FUN=FUN)

  algorithm=attr(x$data,"algorithm")
  resmatlist=Aggregate.list(x$matlist,
                            x="rank",
                            algorithm=algorithm,
                            FUN=FUN,...)$matlist
  resmatlist=lapply(resmatlist,
                    function(z) as.data.frame(z))
  res=list(matlist=resmatlist,
           call=c(x$call,call),
           data=x$data,
           FUN =  . %>% (x$FUN) %>%  (call),
           FUN.list=c(x$FUN.list,FUN)
  )
  class(res)=c("aggregatedRanks.list",class(res))
  res

}


aggregate.bootstrap.list <-function(x,
                                    what="metric",
                                    FUN=mean,
                                    ...){
  call=match.call(expand.dots = T)
  if (is.character(FUN)) FUN=try(eval(parse(text=FUN)),
                                 silent = T)
  FUNname=as.character(call$FUN)

  if (!is.function(FUN)) stop("FUN has to be a function (possibly as character)")
  matlist=llply(1:length(x$bootsrappedRank),
                function(i.piece){
                  if (what=="ranks") xmean <- as.data.frame(apply(x$bootsrappedRank[[i.piece]],1,FUN=FUN))
                  else xmean <- as.data.frame(apply(x$bootsrappedAggregate[[i.piece]],1,FUN=FUN))
                  names(xmean)=paste0(what,"_",FUNname)
                 xmean
                })


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

  if (what=="ranks") xmean <- as.data.frame(apply(x$bootsrappedRank,
                                                  1,
                                                  FUN=FUN))
  else xmean <- as.data.frame(apply(x$bootsrappedAggregate,
                                    1,
                                    FUN=FUN))
  names(xmean)=paste0(what,"_",FUNname)
  res=list(FUN = . %>% (call),
           call=list(call),
           data=x,
           mat=xmean)

  class(res)=c("aggregated",class(res))
  res
}
