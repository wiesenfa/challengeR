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

rank.aggregated.list <-function(object,
                                ties.method="min",
                                smallBetter,
                                ...){

  call=match.call(expand.dots = F)
  if (missing(smallBetter)){
    if (!is.null(attr(object$data,"smallBetter"))) smallBetter=attr(object$data,"smallBetter")
    else stop("smallBetter has to be provided either in as.challenge() or rank()")

    if (object$isSignificance) smallBetter=FALSE  # smallBetter already taken care of by one-sided test nature of signficance
  }

  call=call("rank.aggregated.list",
            object=call$object,
            ties.method=ties.method,
            smallBetter=smallBetter)

  matlist=object$matlist

  matlist=lapply(matlist,
                 function(y){
                   if (nrow(y)>0) r=rankNA2(y[,ncol(y)],
                                            ties.method=ties.method,
                                            smallBetter=smallBetter)
                   else r=NULL
                   res=cbind(y,rank=r)
                   res
                   })

  res=list(matlist=matlist,
           data=object$data,
           call=list(object$call,call),
           FUN =  . %>% (object$FUN) %>%  (call),
           FUN.list=c(object$FUN.list,
                      "rank")
      )
  class(res)=c("ranked.list",class(res))

  res
}

rank.aggregatedRanks.list <-function(object,
                                     ties.method="min",
                                     ...){

  call=match.call(expand.dots = F)
  call=call("rank.aggregatedRanks.list",
            object=call$object,
            ties.method=ties.method)
  matlist=object$matlist

  matlist=lapply(matlist, function(y){
        if (nrow(y)>0) r=rankNA2(y[,ncol(y)],
                                 ties.method=ties.method,
                                 smallBetter=TRUE)
        else r=NULL
        res=cbind(y,rank=r)
        res
  })

  res=list(matlist=matlist,
           data=object$data,
           call=list(object$call,call),
           FUN =  . %>% (object$FUN) %>%  (call),
           FUN.list=c(object$FUN.list,
                      "rank")
           )
  class(res)=c("ranked.list",class(res))
  res

  res
}
