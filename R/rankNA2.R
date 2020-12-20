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

rankNA2 <-
function(x,ties.method="min",smallBetter=TRUE){
    r=rank((-1)^(!smallBetter)*x,ties.method=ties.method,na.last="keep")  #xtfrm maybe faster alternative
    if (any(is.na(x))){
        maxrank=ifelse(all(is.na(x)), yes=0, no=max(r,na.rm=TRUE))
        if (ties.method%in%c("min","random")) r[is.na(x)]<-maxrank+1
        if (ties.method=="average") r[is.na(x)]<-maxrank+mean(1:sum(is.na(x)))
    }
    r
}
