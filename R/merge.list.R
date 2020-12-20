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

merge.list=function(x,y,by="row.names",suffixes = c(".1",".2"),...){
  if (is.list(x) & is.list(y)){
    #if (!all.equal(names(x),names(y))) stop("list elements must have same names and lists must have same length")
    common.elements=intersect(names(x),names(y))

    res=lapply(common.elements, function(z){
      merge(x[[z]],y[[z]],by=by,suffixes=suffixes,...)
    })
    names(res)=common.elements
    res

  } else stop("Comparison of a list and a data.frame under construction")
}

quickmerge.list=function(x,y){
  if (is.list(x) & is.list(y)){
    #if (!all.equal(names(x),names(y))) stop("list elements must have same names and lists must have same length")
    common.elements=intersect(names(x),names(y))

    res=lapply(common.elements, function(z){
      dat1=x[[z]]
      dat2=y[[z]]
      dat1=dat1[order(rownames(dat1)),,drop=F]
      dat2=dat2[order(rownames(dat2)),,drop=F]
      if (all(rownames(dat1)==rownames(dat2))) {
          qq=cbind(dat1,dat2)
          rownames(qq)=rownames(dat1)
          qq
      }
      else {
        id=intersect(rownames(dat1),rownames(dat2))
        dat1=dat1[match(id,rownames(dat1)),]
        dat2=dat2[match(id,rownames(dat2)),,drop=F]
        qq=cbind(dat1,dat2)
        rownames(qq)=rownames(dat1)
        qq
      }
    })
    names(res)=common.elements
    res

  } else stop("Comparison of a list and a data.frame under construction")
}
