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

# to link with benchmark package (CRAN archived)

as.warehouse.challenge=function(x,...){
  x$.ds="data"
  x$.perf="perf"
  form=as.formula(paste(attr(x,"case"),attr(x,"algorithm"),".perf",".ds",sep="~"))
   ar=acast(x,form,value.var = attr(x,"value"))
   
#   ar=acast(dd,case~alg_name~score~subtask,value.var = attr(object,"value"))
  names(dimnames(ar))  =c("samp", "alg" , "perf", "ds")
  w=benchmark::as.warehouse.array4dim(ar)
  apm <- w$viewAlgorithmPerformance(performances = "perf",datasets="data")
  attr(apm,"challenge")=attributes(x)[-(1:2)]
 apm
}


