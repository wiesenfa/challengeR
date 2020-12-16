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

select.if <- function(object,...) UseMethod("select.if")
select.if.default <- function(object, ...) stop("not implemented for this class")

select.if.comparedRanks.list=function(object,FUN,...){
  #if (!missing(FUN)) 
    res=object[sapply(object, function(x) do.call(FUN,args=list(x=x$mat)))]
  #if (!missing(which)) res=object[which]
   class(res)="comparedRanks.list"
  res
}

select.if.list=function(object,FUN,...){
  res=object[sapply(object, function(x) do.call(FUN,args=list(x=x)))]
  res
}



select.if.aggregated.list=select.if.ranked.list=function(object,FUN,...){
  call=match.call(expand.dots = T)  
  matlist=object$matlist
  #if (!missing(FUN)) 
    matlist=matlist[sapply(matlist, function(x) do.call(FUN,args=list(x=x)))]
  #if (!missing(which)) matlist=matlist[which]
  
  res=list(matlist=matlist,
           call=list(object$call,call),
           data=object$data,
       FUN =  . %>% (object$FUN) %>%  (call)
      )

  class(res)=class(object)
  res
    
}


