bootstrap.ranked=function(object,
                          nboot, parallel=FALSE,progress="text",...){
  data=object$data
  algorithm=attr(data,"algorithm")
  by=attr(data,"case")
  index=unique(data[[by]])
  # stop if only 1 data set or less than 3 algorithms
    if (length(index)==1 |  length(unique(data[[algorithm]]))<=2 ) stop("There need to be at least 2 unique values in ", by, "and at least 3 unique in ",algorithm)

  drawsample=function(piece,...){ #lapply(1:nboot, function(it){
    bootIndex=data.frame(sample(index,size=length(index),replace=TRUE))
    colnames(bootIndex)=by
    bootData=merge(bootIndex,data,by=by)
    # bootIndex=sample(index,size=length(index),replace=TRUE)
    # bootData=bind_rows(lapply(bootIndex,function(zz) data[data[[by]]==zz,]))
    #3rd alternative (untested)
    # bootIndex=sample(index,size=length(index),replace=TRUE)
    #bootData=bind_rows(split(data,data[[by]])[bootIndex]))
    attr(bootData,"inverseOrder")=attr(object$data,"inverseOrder")
    attr(bootData,"algorithm")=attr(object$data,"algorithm")
    attr(bootData,"case")=attr(object$data,"case")
    object$FUN(bootData)$mat
  }
  res=#suppressWarnings(
    llply(.data=1:nboot, .fun =drawsample , .parallel=parallel,.progress=progress)
    #)

  rankmat=res[[1]][,-1,drop=F]
  for (j in 2:length(res)){
    rankmat=merge(rankmat,res[[j]][,-1,drop=F],by="row.names", suffixes = c(paste0(".",j-1),paste0(".",j))) #maybe replayce by plyr::join() which is supposed to be faster
    rownames(rankmat)=rankmat[,"Row.names"]
    rankmat=rankmat[,-1]
  }
  aggmat=res[[1]][,-2,drop=F]
  for (j in 2:length(res)){
    aggmat=merge(aggmat,res[[j]][,-2,drop=F],by="row.names", suffixes = c(paste0(".",j-1),paste0(".",j))) 
    rownames(aggmat)=aggmat[,"Row.names"]
    aggmat=aggmat[,-1]
  }
  
  res=list(bootsrappedRanks=rankmat,bootsrappedAggregate=aggmat, data=data,mat=object$mat,FUN=object$FUN,FUN.list=object$FUN.list)
  class(res)="bootstrap"
  res
}



bootstrap.ranked.list=function(object,#algorithm, by,
                               nboot,
         parallel=FALSE,progress="text",...){
  algorithm=attr(object$data,"algorithm")
  by=attr(object$data,"case")

  # exclude if only 1 data set or less than 3 algorithms
    tidy.data.id=sapply(object$data, function(data.subset) {
      ifelse((length(unique(data.subset[[by]]))==1 |  length(unique(data.subset[[algorithm]]))<=2 ),yes=FALSE,no=TRUE)
    })
    tidy.data=object$data[tidy.data.id]
    tidy.matlist=object$matlist[tidy.data.id]
 #   class(tidy.matlist)="ranked.list"
    
  res=#suppressWarnings(
    llply(1:nboot, function(it){
    # draw 1 sample for each task
    bootDatalist=lapply(tidy.data, function(data.subset) {
        index=unique(data.subset[[by]])
 
        # bootIndex=sample(index,size=length(index),replace=TRUE)
        # bootData=bind_rows(lapply(bootIndex,function(zz) data.subset[data.subset[[by]]==zz,]))
        # faster:
        bootIndex=data.frame(sample(index,size=length(index),replace=TRUE))
        colnames(bootIndex)=by
        bootData=merge(bootIndex,data.subset,by=by)
        bootData
    })
attr(bootDatalist,"inverseOrder")=attr(object$data,"inverseOrder")
attr(bootDatalist,"algorithm")=attr(object$data,"algorithm")
attr(bootDatalist,"case")=attr(object$data,"case")
object$FUN(bootDatalist)$mat
  }, .parallel=parallel,.progress=progress)
  #)
    

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
  rankmatlist=lapply(res[[1]],function(z) z[,"rank",drop=F])
  for (j in 2:length(res)){
    rankmatlist=quickmerge.list(rankmatlist,lapply(res[[j]],function(z) z[,"rank",drop=F]))
  }

  aggmatlist=lapply(res[[1]],function(z) z[,-2,drop=F])
  for (j in 2:length(res)){
    aggmatlist=quickmerge.list(aggmatlist,lapply(res[[j]],function(z) z[,-2,drop=F]))
  }


  final=list(bootsrappedRanks=rankmatlist,bootsrappedAggregate=aggmatlist, data=object$data,matlist=tidy.matlist,FUN=object$FUN,FUN.list=object$FUN.list)
#  c(res,list(winnerFrequencies=winnerFrequencies(res)))
  class(final)=c("bootstrap.list")
  final
}


# rankFrequencies.bootstrap=function(object, who){
#   if (is.data.frame(who)) who=rownames(who)
#   
#   if (!inherits(object,"list")){
#     if (length(who)==1){
#       res=table(t(object$bootsrappedRanks[rownames(object$bootsrappedRanks)==who,]))
#       cat("\n",who,"\n")
#       print(res)
#     } else {
#       res=lapply(who, function(w){
#         rr=table(t(object$bootsrappedRanks[rownames(object$bootsrappedRanks)==w,]))
#         cat(w,"\n")
#         print(rr)
#         cat("\n")
#         rr
#       })
#     }
#   } else {
#     
#   }
#   res=c(res=res,object)
# 
#     
#   }
#    # class(res)="bootstrapResults"
#   invisible(res)
# }

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
  # class(res)="bootstrapResults"
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
  # class(res)="bootstrapResults"
  invisible(res)
}




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



