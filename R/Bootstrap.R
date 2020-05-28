bootstrap <- function(object,...) UseMethod("bootstrap")
bootstrap.default <- function(object, ...) stop("not implemented for this class")


bootstrap.ranked.list=function(object,
                               nboot,
                               parallel=FALSE,
                               progress="text",
                               ...){
  algorithm=attr(object$data,"algorithm")
  by=attr(object$data,"case")

  # exclude if only 1 data set or less than 3 algorithms
  tidy.data.id=sapply(object$data,
                      function(data.subset) {
                        ifelse((length(unique(data.subset[[by]]))==1 |  length(unique(data.subset[[algorithm]]))<=2 ),
                               yes=FALSE,
                               no=TRUE)
                        })
  tidy.data=object$data[tidy.data.id]
  tidy.matlist=object$matlist[tidy.data.id]

  res= llply(1:nboot,
             function(it){
               # draw 1 sample for each task
               bootDatalist = lapply(tidy.data, function(data.subset) {
                 index = unique(data.subset[[by]])

                 # bootIndex=sample(index,size=length(index),replace=TRUE)
                 # bootData=bind_rows(lapply(bootIndex,function(zz) data.subset[data.subset[[by]]==zz,]))
                 # faster:
                 bootIndex = data.frame(sample(index,
                                               size = length(index),
                                               replace = TRUE))
                 colnames(bootIndex) = by
                 bootData = merge(bootIndex,
                                  data.subset,
                                  by = by)
                 bootData
               })
               attr(bootDatalist, "inverseOrder") = attr(object$data, "inverseOrder")
               attr(bootDatalist, "algorithm") = attr(object$data, "algorithm")
               attr(bootDatalist, "case") = attr(object$data, "case")
               attr(bootDatalist, "check") = FALSE
               object$FUN(bootDatalist)$mat
             },
             .parallel = parallel,
             .progress = progress)



  # rankmatlist=lapply(res[[1]],function(z) z[,"rank",drop=F])
  # for (j in 2:length(res)){
  #   rankmatlist=merge.list(rankmatlist,lapply(res[[j]],function(z) z[,"rank",drop=F]),by="row.names", suffixes = c(paste0(".",j-1),paste0(".",j)))
  #   rankmatlist=lapply(rankmatlist, function(z) {
  #     rownames(z)=z[,"Row.names"]
  #     z=z[,-1]
  #     })
  # }
  #
  # aggmatlist=lapply(res[[1]],function(z) z[,-2,drop=F])
  # for (j in 2:length(res)){
  #   aggmatlist=merge.list(aggmatlist,lapply(res[[j]],function(z) z[,-2,drop=F]),by="row.names", suffixes = c(paste0(".",j-1),paste0(".",j)))
  #   aggmatlist=lapply(aggmatlist, function(z) {
  #     rownames(z)=z[,"Row.names"]
  #     z=z[,-1]
  #     })
  # }
  rankmatlist = lapply(res[[1]],
                       function(z) z[, "rank", drop = F]
                       )
  for (j in 2:length(res)) {
    rankmatlist = quickmerge.list(rankmatlist,
                                  lapply(res[[j]],
                                         function(z)  z[, "rank", drop = F]))
  }

  aggmatlist = lapply(res[[1]],
                      function(z) z[, -2, drop = F])
  for (j in 2:length(res)) {
    aggmatlist = quickmerge.list(aggmatlist,
                                 lapply(res[[j]],
                                        function(z) z[, -2, drop = F]))
  }

  final=list(bootsrappedRanks=rankmatlist,
             bootsrappedAggregate=aggmatlist,
             data=object$data,
             matlist=tidy.matlist,
             FUN=object$FUN,
             FUN.list=object$FUN.list)
  class(final)=c("bootstrap.list")
  final
}






####################################################################################################
# deprecate following functions?



rankFrequencies <- function(object,...) UseMethod("rankFrequencies")
rankFrequencies.default <- function(object, ...) stop("not implemented for this class")

rankFrequencies.bootstrap=function(object, who,...){
  if (is.data.frame(who)) who=rownames(who)
  if (length(who)==1){
    res=table(t(object$bootsrappedRanks[rownames(object$bootsrappedRanks)==who,]))
    cat("\n",who,"\n")
    print(res)
  } else {
    res=lapply(who, function(w){
      rr=table(t(object$bootsrappedRanks[rownames(object$bootsrappedRanks)==w,]))
    cat(w,"\n")
      print(rr)
      cat("\n")
      rr
    })
  }
  res=c(list(rankFrequencies=res),object)
  invisible(res)
}

rankFrequencies.bootstrap.list=function(object, who,...){
  if (is.data.frame(who)) who=rownames(who)
  res=lapply(object$bootsrappedRanks,function(bootMat){
    if (length(who)==1){
      res=table(t(bootMat[rownames(bootMat)==who,]))
      cat("\n",who,"\n")
      print(res)
    } else {
      res=lapply(who, function(w){
        rr=table(t(bootMat[rownames(bootMat)==w,]))
        cat(w,"\n")
        print(rr)
        cat("\n")
        rr
      })
    }
    res
  })
  res=c(list(rankFrequencies=res),object)
  invisible(res)
}




winnerFrequencies <- function(object,...) UseMethod("winnerFrequencies")
winnerFrequencies.default <- function(object, ...) stop("not implemented for this class")

# Achtung: bester rank muss ==1 sein und nicht z.B. 1.5
winnerFrequencies.bootstrap=function(object,...){
  rankings_dicho=ifelse(object$bootsrappedRanks==1,1,0)
  winnerFrequencies=data.frame(winnerFrequency=rowSums(rankings_dicho),row.names = rownames(object$bootsrappedRanks))
  res=merge(object$mat,winnerFrequencies,by="row.names",...)
  rownames(res)=res[,1]
  res=res[,-1]
  # res=c(res=res,object)
  # class(res)="bootstrapResults"
  res
}

winnerFrequencies.bootstrap.list=function(object,...){
  res=lapply(1:length(object$bootsrappedRanks),function(id){
    rankings_dicho=ifelse(object$bootsrappedRanks[[id]]==1,1,0)
    winnerFrequencies=data.frame(winnerFrequency=rowSums(rankings_dicho),row.names = rownames(object$bootsrappedRanks[[id]]))
    res=merge(object$matlist[[id]],winnerFrequencies,by="row.names",...)
    rownames(res)=res[,1]
    res=res[,-1]
    res
  })
  names(res)=names(object$bootsrappedRanks)
  res
}
