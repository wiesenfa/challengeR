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

relation_dissimilarity <- function(x,...) UseMethod("relation_dissimilarity")
relation_dissimilarity.default <- function(x, ...) relations::relation_dissimilarity(x,  ...) 

relation_dissimilarity.ranked.list=function(x,
                                            method=kendall,
                                            ...){  #method in kendall, spearmansFootrule, spearmansWeightedFootrule or any other function with two arguments
  tt=names(x$matlist)
  n.tt=length(tt)
  tau=matrix(NA,n.tt,n.tt)
  colnames(tau)=rownames(tau)=tt
  aa=melt(x,
          measure.vars="rank")
  for (i in 1:n.tt){
    for (j in 1:n.tt){
      temp=aa%>%
        filter(L1==as.character(tt[i]))%>% 
        right_join(aa%>%
                     filter(L1==as.character(tt[j])),
                   by="algorithm")
      tau[i,j]=method(temp$value.x,
                      temp$value.y) 
    }
  }
  
  if (method(1:2,1:2)==1 & method(2:1,1:2)==-1)  as.dist(1-tau)  #if two identical objects yield value of 1, method seems to be a correlation
  else as.dist(tau) #distance
}


as.relation.ranked.list=function(x,...){
 res= lapply(x$matlist,function(z){
    r=z[,"rank"]
    names(r)=rownames(z)
    as.relation(r)
  } )
 class(res)="relation_ensemble"
 res
}
