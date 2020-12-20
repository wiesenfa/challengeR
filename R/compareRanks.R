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

kendall=function(a,b) cor(a,b,method="kendall")
spearmansWeightedFootrule=function(a,b)  sum(abs(a-b)/pmin(a,b))
spearmansFootrule=function(a,b)  sum(abs(a-b))
# SpearmansFootrule=function(a,b)  sum(abs(match(a, b) - a))
# SpearmansWeightedFootrule=function(a,b)  sum(abs(match(a, b) - a)/pmin(a,b))


compareRanks <- function(x,...) UseMethod("compareRanks")
compareRanks.default <- function(x, ...) stop("not implemented for this class")


 compareRanks.ranked.list <-function(x,
                                     y,
                                     FUN=kendall,...){
    matlist=merge.list(x$matlist,
                       y$matlist
                       ,...)
    res=lapply(1:length(matlist), 
               function(z){
                 tau=FUN(matlist[[z]][,"rank.1"],
                         matlist[[z]][,"rank.2"])
                 res=list(tau=tau,
                          mat=matlist[[z]])
                 class(res)="comparedRanks"
                 res
                 })
    names(res)=names(matlist)
    class(res)="comparedRanks.list"
   res
  }




print.comparedRanks <-
function(x,...)  {
  cat("\n")
  print(x$mat)
  cat("\nKendall's tau =",x$tau,"\n-------------------------------------------------------\n")
 }


