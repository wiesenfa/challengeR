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

default_colors=
  function(n = length(algorithms), algorithms = NULL) {
    # Based on ggplot2:::ScaleHue
    h <- c(0, 360) + 15
    l <- 65
    c <- 100
    
    start <-0# 1
    direction <- 1
    
    rotate <- function(x) (x + start) %% 360 * direction
    
    if ( (diff(h) %% 360) < 1 ) {
      h[2] <- h[2] - 360 / n
    }
    
    structure(grDevices::hcl(h = rotate(seq(h[1], h[2], length = n)),
                             c = c, l = l),
              names = algorithms)
  }
