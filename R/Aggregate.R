

Aggregate.data.frame <-function(object,x,algorithm, FUN=mean,
         na.treat="na.rm", #can be na.rm, numeric value or function
         dataset_id, alpha=0.05, p.adjust.method="none",alternative="one.sided",test.fun=function(x,y) wilcox.test(x,y,
                                                                                                                   alternative = alternative,exact=FALSE,
                                                                                                                   paired = TRUE)$p.value,inverseOrder=TRUE, # only needed for significance 
          ...                      ){
  call=match.call(expand.dots = T)  
  if (is.numeric(na.treat)) object[,x][is.na(object[,x])]=na.treat
  else if (is.function(na.treat)) object[,x][is.na(object[,x])]=na.treat(object[,x])
  else if (na.treat=="na.rm") object=object[complete.cases(object),]
  
  if (is.character(FUN) && FUN=="significance"){
    if(missing(dataset_id)| missing(inverseOrder)| missing(alpha)) stop("If FUN='significance' arguments dataset_id, inverseOrder and alpha need to be given")
    if (length(unique(object[[algorithm]]))<=1){
        warning("only one ", algorithm, " available")
        agg=data.frame()
    } else {
       agg=significance(object, x, algorithm, dataset_id, alpha, inverseOrder,p.adjust.method=p.adjust.method,alternative=alternative,...)
    }

  } else {
      FUNname=as.character(call$FUN)
      if (is.character(FUN))  FUN=try(eval(parse(text=FUN)),silent = T)
      
    if (!is.function(FUN)) stop("FUN has to be a function (possibly as character) or 'significance'")
    #if (missing(x) & "rank" %in% colnames(object)) x="rank"
#      agg <- aggregate(object[,x], by=list(object[,algorithm]), FUN=function(z) do.call(FUN,args=list(x=z,na.rm=(na.treat=="na.rm"))))
      agg <- aggregate(object[,x], by=list(object[,algorithm]), FUN=function(z) do.call(FUN,args=list(x=z)))
      #    names(agg)=c(algorithm,paste0(x,"_",strsplit(capture.output(suppressWarnings(print(methods(FUN),byclass=T)))[1]," ")[[1]][2]))
    names(agg)=c(algorithm,paste0(x,"_",FUNname))
    rownames(agg)=agg[,1]
    agg=agg[,-1,drop=F]
  }
  res=list(FUN = . %>% (call),
           call=list(call),
           data=object,
           mat=agg)
  class(res)=c("aggregated",class(res))
  res
  
}


Aggregate.list <-function(object,x,algorithm,FUN=mean,
         na.treat="na.rm",
         parallel=FALSE,progress="none",
         dataset_id, alpha=0.05, p.adjust.method="none",alternative="one.sided",test.fun=function(x,y) wilcox.test(x,y,
                                                                                                                   alternative = alternative,exact=FALSE,
                                                                                                                   paired = TRUE)$p.value,inverseOrder=TRUE, # only needed for significance 
         ...            ){
  call=match.call(expand.dots = T)  
  #if (missing(x) &  all(sapply(object, function(z) ("rank" %in% colnames(z))))) x="rank"
  if (is.character(FUN) && FUN=="significance"){
    if(missing(dataset_id)| missing(inverseOrder)| missing(alpha)) stop("If FUN='significance' arguments dataset_id, inverseOrder and alpha need to be given")
    matlist=llply(1:length(object), function(id){ #lapply(object, function(y){
      piece=object[[id]]
      if (length(unique(piece[[algorithm]]))<=1){
        warning("only one ", algorithm, " available in element ", names(object)[id])
         return(data.frame("prop_significance"=rep(NA,length(unique(piece[[algorithm]]))),row.names = unique(piece[[algorithm]])))  
      } 
      if (is.numeric(na.treat)) piece[,x][is.na(piece[,x])]=na.treat
      else if (is.function(na.treat)) piece[,x][is.na(piece[,x])]=na.treat(piece[,x])
      xmean <- significance(piece, x, algorithm, dataset_id, alpha, p.adjust.method=p.adjust.method,inverseOrder,alternative=alternative,...)
      class(xmean)=c("aggregated",class(xmean))
      xmean
    }, .parallel=parallel,.progress=progress
  )

  } else {
    if (is.character(FUN)) FUN=try(eval(parse(text=FUN)),silent = T)
    FUNname=as.character(call$FUN)
    if (!is.function(FUN)) stop("FUN has to be a function (possibly as character) or 'significance'")

    matlist=llply(object, function(piece){ #lapply(object, function(piece){
          if (is.numeric(na.treat)) piece[,x][is.na(piece[,x])]=na.treat
          else if (is.function(na.treat)) piece[,x][is.na(piece[,x])]=na.treat(piece[,x])
          else if (na.treat=="na.rm") piece=piece[complete.cases(piece),]
          
#          xmean <- aggregate(piece[,x], by=list(piece[,algorithm]), FUN=function(z) do.call(FUN,args=list(x=z,na.rm=(na.treat=="na.rm"))))
          xmean <- aggregate(piece[,x], by=list(piece[,algorithm]), FUN=function(z) do.call(FUN,args=list(x=z)))
          #          names(xmean)=c(algorithm,paste0(x,"_",strsplit(capture.output(suppressWarnings(print(methods(FUN),byclass=T)))[1]," ")[[1]][2]))
          names(xmean)=c(algorithm,paste0(x,"_",FUNname))
          rownames(xmean)=xmean[,1]
          xmean=xmean[,-1,drop=F]
       #   class(xmean)=c("aggregated",class(xmean))
          xmean
        }, .parallel=parallel,.progress=progress
      )
  }
    names(matlist)=names(object)
   res=list(FUN = . %>% (call),
            call=list(call),
      data=object,
       matlist=matlist)

  class(res)=c("aggregated.list",class(res))
  res
}







