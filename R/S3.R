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

utils::globalVariables(c("."))

"+.ggList" <- function (e1, e2){
  pp <- e1
  if(is.ggplot(pp)) plotList <- list(pp)
  else if(is.list(pp)) plotList <- pp
  else stop("Can't handle an object of class ", class(pp))

  for(i in 1:length(plotList)){
    p <- plotList[[i]]
    if(is.ggplot(p)) plotList[[i]] <- p + e2
  }

  if(is.ggplot(pp)) plotList[[1]]
  else plotList
}

"%++%" <- `+.ggList`

print.ranked.list <-function(x,...)  print(x$matlist, ...)
print.aggregated.list <-function(x,...)  print(x$matlist, ...)
print.aggregated <-function(x,...)  print(x$mat,...)
print.ranked <-function(x,...)  print(x$mat[order(x$mat$rank),],...)
print.ggList <- function(x, ...) {
  for(i in 1:length(x)) print(x[[i]])
}
