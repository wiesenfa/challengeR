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

test_that("extraction of task subset works for multi-task data set", {
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

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  rankingSubset <- subset(ranking, tasks=c("T2"))

  expect_equal(length(rankingSubset$matlist), 1)
  expect_is(rankingSubset$matlist$T2, "data.frame")

  expect_equal(length(rankingSubset$data), 1)
  expect_is(rankingSubset$data$T2, "data.frame")
})

test_that("extraction of task subset works for single-task data set", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  rankingSubset <- subset(ranking, tasks=c("T1"))

  expect_equal(length(rankingSubset$matlist), 1)
  expect_is(rankingSubset$matlist$T1, "data.frame")

  expect_equal(length(rankingSubset$data), 1)
  expect_is(rankingSubset$data$T1, "data.frame")
})

test_that("extraction of task subset raises an error for invalid task name", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  expect_error(subset(ranking, tasks=c("T1x")),
               "There is/are no task(s) called T1x.", fixed=TRUE)
})

test_that("extraction of task subset from bootstrap ranking works for multi-task data set", {
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

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  rankingBootstrappedSubset <- subset(rankingBootstrapped, tasks=c("T2"))

  expect_equal(length(rankingBootstrappedSubset$matlist), 1)
  expect_is(rankingBootstrappedSubset$matlist$T2, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$data), 1)
  expect_is(rankingBootstrappedSubset$data$T2, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$bootstrappedRanks), 1)
  expect_is(rankingBootstrappedSubset$bootstrappedRanks$T2, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$bootstrappedAggregate), 1)
  expect_is(rankingBootstrappedSubset$bootstrappedAggregate$T2, "data.frame")
})

test_that("extraction of task subset from bootstrap ranking works for single-task data set", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  rankingBootstrappedSubset <- subset(rankingBootstrapped, tasks=c("T1"))

  expect_equal(length(rankingBootstrappedSubset$matlist), 1)
  expect_is(rankingBootstrappedSubset$matlist$T1, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$data), 1)
  expect_is(rankingBootstrappedSubset$data$T1, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$bootstrappedRanks), 1)
  expect_is(rankingBootstrappedSubset$bootstrappedRanks$T1, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$bootstrappedAggregate), 1)
  expect_is(rankingBootstrappedSubset$bootstrappedAggregate$T1, "data.frame")
})

test_that("extraction of task subset from bootstrap ranking raises an error for invalid task name", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  expect_error(subset(rankingBootstrapped, tasks=c("T1x")),
               "There is/are no task(s) called T1x.", fixed=TRUE)

})

