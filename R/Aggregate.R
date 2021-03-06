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

Aggregate <- function(object,...) UseMethod("Aggregate")
Aggregate.default <- function(object, ...) aggregate(object,...)  #stats::aggregate

Aggregate.list <-function(object,
                          x,
                          algorithm,
                          FUN = mean,
                          na.treat = "na.rm",
                          parallel = FALSE,
                          progress = "none",
                          case,
                          alpha = 0.05,
                          p.adjust.method = "none",
                          alternative = "one.sided",
                          test.fun = function(x, y) wilcox.test(x,
                                                                y,
                                                                alternative = alternative,
                                                                exact = FALSE,
                                                                paired = TRUE)$p.value,
                          smallBetter = FALSE,   # only needed for significance
                          ...            ) {
  call=match.call(expand.dots = T)
  if (is.character(FUN) && FUN=="significance"){
    if(missing(case)|  missing(smallBetter)|  missing(alpha)) stop("If FUN='significance' arguments case, smallBetter and alpha need to be given")
    matlist=llply(1:length(object),
                  function(id){
                    piece=object[[id]]
                    if (length(unique(piece[[algorithm]]))<=1){
                      warning("Only one algorithm available in task '", names(object)[id], "'.")
                      return(data.frame("prop_significance"=rep(NA,length(unique(piece[[algorithm]]))),
                                        row.names = unique(piece[[algorithm]])))
                    }
                    if (is.numeric(na.treat)) piece[,x][is.na(piece[,x])]=na.treat
                    else if (is.function(na.treat)) piece[,x][is.na(piece[,x])]=na.treat(piece[,x][is.na(piece[,x])])
                    else if (na.treat=="na.rm") piece=piece[!is.na(piece[,x]),]
                    else stop("Argument 'na.treat' is invalid. It can be 'na.rm', numeric value or function.")

                    xmean <- significance(piece,
                                          x,
                                          algorithm,
                                          case,
                                          alpha,
                                          p.adjust.method=p.adjust.method,
                                          smallBetter,
                                          alternative=alternative,
                                          ...)
                    class(xmean)=c("aggregated",
                                   class(xmean))
                    xmean
                    },
                  .parallel=parallel,
                  .progress=progress
    )
    isSignificance=TRUE

  } else {
    if (is.function(FUN)) FUNname <-gsub('\")',"",gsub('UseMethod(\"',"",deparse(functionBody(FUN)),fixed = T),fixed=T)
    else if (is.character(FUN)) FUNname=FUN

    if (is.character(FUN)) FUN=try(eval(parse(text=FUN)),
                                   silent = T)
    if (!is.function(FUN)) stop("FUN has to be a function (possibly as character) or 'significance'")

    matlist=llply(object,
                  function(piece){
                    if (is.numeric(na.treat)) piece[,x][is.na(piece[,x])]=na.treat
                    else if (is.function(na.treat)) piece[,x][is.na(piece[,x])]=na.treat(piece[,x][is.na(piece[,x])])
                    else if (na.treat=="na.rm") piece=piece[!is.na(piece[,x]),]
                    else stop("Argument 'na.treat' is invalid. It can be 'na.rm', numeric value or function.")

                    xmean <- aggregate(piece[,x],
                                       by=list(piece[,algorithm]),
                                       FUN=function(z) do.call(FUN,args=list(x=z)))
                    names(xmean)=c(algorithm,
                                   paste0(x,"_",FUNname))
                    rownames(xmean)=xmean[,1]
                    xmean=xmean[,-1,drop=F]
                    xmean
                  },
                  .parallel=parallel,
                  .progress=progress
                  )
    isSignificance=FALSE
  }
  names(matlist)=names(object)
  res=list(FUN = . %>% (call),
           FUN.list=list(FUN),
           call=list(call),
           data=object,
           matlist=matlist,
           isSignificance=isSignificance
           )

  class(res)=c("aggregated.list",class(res))
  res
}
