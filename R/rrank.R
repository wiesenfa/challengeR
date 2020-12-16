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

rank <- function(object,...) UseMethod("rank")
rank.default <- function(object, ...) base::rank(object,...)  #stats::aggregate

rank.challenge=function(object,
                        x,
                        ties.method="min",...){
  call=as.list(match.call())
  if (!is.null(attr(object,"annotator"))) {
    call2=call("Rank",
               object=call$object,
               x=attr(object,"value"),
               annotator=c(attr(object,"annotator")),
               ties.method=ties.method,
               smallBetter=attr(object,"smallBetter")
             )
    res1=do.call("Rank",list(object=object,
                             x=attr(object,"value"),
                             annotator=c(attr(object,"annotator")),
                             ties.method=ties.method,
                             smallBetter=attr(object,"smallBetter")
           ))

  } else {
    call2=call("Rank",
               object=call$object,
               x=attr(object,"value"),
               ties.method=ties.method,
               smallBetter=attr(object,"smallBetter")
             )
    res1=do.call("Rank",list(object=object,
                             x=attr(object,"value"),
                             ties.method=ties.method,
                             smallBetter=attr(object,"smallBetter")
           ))

  }

  res=list(FUN = . %>% (call2),
           call=list(call2),
           FUN.list=list("rank"),
           data=object,
           matlist=res1$matlist)

  class(res)=c("ranked.list",class(res))
  res
}
