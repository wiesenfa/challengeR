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

.onAttach <- function (lib, pkg) {
  ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"),"Version")
  ver <- as.character(ver)
  packageStartupMessage("\nchallengeR ",
                        ver,
                        " loaded. \n",
                        #                       "Note: Layouting in case of many algorithms or tasks is not yet optimized. Please be patient, we are steadily working on improving the package",
                        "\nPlease cite as:\n   Wiesenfarth, M., Reinke, A., Landman, B.A., Cardoso, M.J., Maier-Hein, L. and Kopp-Schneider, A. (2019). Methods and open-source toolkit for analyzing and visualizing challenge results. arXiv preprint arXiv:1910.05121\n",
                        domain = NULL,
                        appendLF = TRUE)
}

.onLoad <- function(...) {
}

.onUnload <- function (libpath) {
}
