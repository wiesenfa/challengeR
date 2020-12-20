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

test_that("cluster analysis network plot raises error for single-task data set", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  expect_error(network(ranking, edge.col=grDevices::grey.colors, edge.lwd=1, cols=NULL),
               "The cluster analysis is only sensible for more than two tasks.", fixed=TRUE)
})

test_that("cluster analysis network plot raises error for multi-task data set containing two tasks", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.6, case="C1"),
                       data.frame(algo="A3", value=0.4, case="C1"),
                       data.frame(algo="A1", value=0.2, case="C2"),
                       data.frame(algo="A2", value=0.1, case="C2"),
                       data.frame(algo="A3", value=0.0, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A2", value=0.3, case="C1"),
                       data.frame(algo="A3", value=0.4, case="C1"),
                       data.frame(algo="A1", value=0.7, case="C2"),
                       data.frame(algo="A2", value=0.8, case="C2"),
                       data.frame(algo="A3", value=0.9, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  expect_error(network(ranking, edge.col=grDevices::grey.colors, edge.lwd=1, cols=NULL),
               "The cluster analysis is only sensible for more than two tasks.", fixed=TRUE)
})

test_that("cluster analysis network plot returns a network object for multi-task data set containing three tasks", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                        data.frame(algo="A1", value=0.8, case="C1"),
                        data.frame(algo="A2", value=0.6, case="C1"),
                        data.frame(algo="A3", value=0.4, case="C1"),
                        data.frame(algo="A1", value=0.2, case="C2"),
                        data.frame(algo="A2", value=0.1, case="C2"),
                        data.frame(algo="A3", value=0.0, case="C2")
                      ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A2", value=0.3, case="C1"),
                       data.frame(algo="A3", value=0.4, case="C1"),
                       data.frame(algo="A1", value=0.7, case="C2"),
                       data.frame(algo="A2", value=0.8, case="C2"),
                       data.frame(algo="A3", value=0.9, case="C2")
                     ))
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.2, case="C1"),
                       data.frame(algo="A3", value=0.3, case="C1"),
                       data.frame(algo="A1", value=0.6, case="C2"),
                       data.frame(algo="A2", value=0.7, case="C2"),
                       data.frame(algo="A3", value=0.8, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  actualPlot <- network(ranking, edge.col=grDevices::grey.colors, edge.lwd=1, cols=NULL)
  expect_is(actualPlot, "network")
})
