# Copyright (c) German Cancer Research Center (DKFZ)
# All rights reserved.
#
# This file is part of challengeR.
#
# challengeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# challengeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with challengeR. If not, see <https://www.gnu.org/licenses/>.

bootstrap <- function(object,...) UseMethod("bootstrap")
bootstrap.default <- function(object, ...) stop("not implemented for this class")


#' Performs bootstrapping
#'
#' Performs bootstrapping on a ranked assessment data set and applies the ranking method to each bootstrap sample. One bootstrap sample of
#' a task with \code{n} cases consists of \code{n} cases randomly drawn with replacement from this task.
#' A total of \code{nboot} of these bootstrap samples are drawn.
#'
#' To ensure reproducibility, please use the doRNG package on Windows or RNG kind = "L'Ecuyer-CMRG" in set.seed(), e.g. set.seed(1, kind = "L'Ecuyer-CMRG").
#'
#' @param object The ranked assessment data set.
#' @param nboot The number of bootstrap samples.
#' @param parallel A boolean specifying whether parallel processing should be enabled.
#' @param progress A string specifying the type of progress indication.
#' @param ... Further arguments passed to or from other functions.
#'
#' @return An S3 object of class "bootstrap.list" to represent a bootstrapped, ranked assessment data set.
#'
#' @examples
#'
#' \dontrun{
#'  # perform bootstrapping with 1000 bootstrap samples using one CPU
#'  set.seed(123, kind="L'Ecuyer-CMRG")
#'  ranking_bootstrapped <- bootstrap(ranking, nboot = 1000)
#' }
#'
#' \dontrun{
#'  # perform bootstrapping using multiple CPUs (here: 8 CPUs)
#'  library(doParallel)
#'  library(doRNG)
#'  registerDoParallel(cores = 8)
#'  registerDoRNG(123)
#'  ranking_bootstrapped <- bootstrap(ranking, nboot = 1000, parallel = TRUE, progress = "none")
#'  stopImplicitCluster()
#' }
#'
#' @export
bootstrap.ranked.list=function(object,
                               nboot,
                               parallel=FALSE,
                               progress="text",
                               ...){
  algorithm=attr(object$data,"algorithm")
  by=attr(object$data,"case")

  # exclude if only 1 test case or only 1 algorithm
  tidy.data.id=sapply(object$data,
                      function(data.subset) {
                        ifelse((length(unique(data.subset[[by]]))==1 |  length(unique(data.subset[[algorithm]]))<=1 ),
                               yes=FALSE,
                               no=TRUE)
                        })
  
  if (sum(tidy.data.id)==0) {
    if (length(object$matlist)>1) stop("All tasks only contained 1 test case. Bootstrapping with 1 test case not sensible.")
    else stop("Only 1 test case included. Bootstrapping with 1 test case not sensible.")
  }
  if (sum(tidy.data.id)<length(object$matlist)) message("Task(s) ", 
                                                        paste(names(tidy.data.id)[!tidy.data.id], collapse = ", "),
                                                        " with only 1 test case excluded from bootstrapping.")   
  
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
