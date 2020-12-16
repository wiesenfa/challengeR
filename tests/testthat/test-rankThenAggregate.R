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

test_that("rank-then-aggregate by mean works with two algorithms for one case, small values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(rank_mean = 1, rank = 1),
    "A2" = data.frame(rank_mean = 2, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate by mean works with two algorithms (reverse order) for one case, small values are better", {
  data <- rbind(
            data.frame(algo = "A2", value = 0.8, case = "C1"),
            data.frame(algo = "A1", value = 0.6, case = "C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm = "algo", case = "case", value = "value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRanking <- rbind("A2" = data.frame(rank_mean = 2, rank = 2),
                           "A1" = data.frame(rank_mean = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate by mean works with two algorithms for one case, large values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(rank_mean = 2, rank = 2),
    "A2" = data.frame(rank_mean = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate by mean works with two algorithms (reverse order) for one case, large values are better", {
  data <- rbind(
    data.frame(algo = "A2", value = 0.8, case = "C1"),
    data.frame(algo = "A1", value = 0.6, case = "C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm = "algo", case = "case", value = "value", smallBetter = FALSE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRanking <- rbind("A2" = data.frame(rank_mean = 1, rank = 1),
                           "A1" = data.frame(rank_mean = 2, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate raises error for invalid aggregation function", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%rankThenAggregate(FUN = meanx),
               "object 'meanx' not found", fixed = TRUE)
})

test_that("rank-then-aggregate by mean works with two algorithms for one case and 'min' as ties method", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean, ties.method = "min")

  expectedRanking <- rbind(
    "A1" = data.frame(rank_mean = 1, rank = 1),
    "A2" = data.frame(rank_mean = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate by mean works with two algorithms for one case and 'max' as ties method", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean, ties.method = "max")

  expectedRanking <- rbind(
    "A1" = data.frame(rank_mean = 2, rank = 2),
    "A2" = data.frame(rank_mean = 2, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate raises error for invalid ties method", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%rankThenAggregate(FUN = mean, ties.method = "maxx"),
               "'arg' should be one of \"average\", \"first\", \"last\", \"random\", \"max\", \"min\"", fixed = TRUE)
})

test_that("rank-then-aggregate raises error for invalid ties method even when no ties present", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%rankThenAggregate(FUN = mean, ties.method = "maxx"),
               "'arg' should be one of \"average\", \"first\", \"last\", \"random\", \"max\", \"min\"", fixed = TRUE)
})

test_that("rank-then-aggregate by mean works with two algorithms for two cases", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A1", value=0.4, case="C2"),
    data.frame(algo="A2", value=0.8, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(rank_mean = 1, rank = 1),
    "A2" = data.frame(rank_mean = 2, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate by median works with two algorithms for two cases", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A1", value=0.4, case="C2"),
    data.frame(algo="A2", value=0.8, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = median)

  expectedRanking <- rbind(
    "A1" = data.frame(rank_median = 1, rank = 1),
    "A2" = data.frame(rank_median = 2, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate by mean works with one algorithm for one case", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(rank_mean = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate assigns worst rank for NA", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(rank_mean = 2, rank = 2),
    "A2" = data.frame(rank_mean = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("rank-then-aggregate raises error for unused NA treatment argument", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  expect_error(challenge%>%rankThenAggregate(FUN = mean, na.treat = 0),
               "unused argument (na.treat = 0)", fixed = TRUE)
})

test_that("rank-then-aggregate by mean works for multi-task challenge (2 tasks in data set), no missing data", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.6, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.5, case="C1"),
                       data.frame(algo="A2", value=0.4, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRankingTask1 <- rbind(
    "A1" = data.frame(rank_mean = 1, rank = 1),
    "A2" = data.frame(rank_mean = 2, rank = 2))

  expectedRankingTask2 <- rbind(
    "A1" = data.frame(rank_mean = 2, rank = 2),
    "A2" = data.frame(rank_mean = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRankingTask1)
  expect_equal(ranking$matlist$T2, expectedRankingTask2)
})

test_that("rank-then-aggregate assigns worst rank for NA in multi-task challenge (2 tasks in data set)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.6, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=NA, case="C1"),
                       data.frame(algo="A2", value=0.4, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%rankThenAggregate(FUN = mean)

  expectedRankingTask1 <- rbind(
    "A1" = data.frame(rank_mean = 1, rank = 1),
    "A2" = data.frame(rank_mean = 2, rank = 2))

  expectedRankingTask2 <- rbind(
    "A1" = data.frame(rank_mean = 2, rank = 2),
    "A2" = data.frame(rank_mean = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRankingTask1)
  expect_equal(ranking$matlist$T2, expectedRankingTask2)
})

test_that("rank-then-aggregate raises error for unused NA treatment argument in multi-task challenge (2 tasks in data set)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.6, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=NA, case="C1"),
                       data.frame(algo="A2", value=0.4, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%rankThenAggregate(FUN = mean, na.treat = "na.rm"),
               "unused argument (na.treat = \"na.rm\")", fixed = TRUE)
})
