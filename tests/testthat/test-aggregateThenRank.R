test_that("aggregate-then-rank by mean works with two algorithms for one case, small values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 1),
    "A2" = data.frame(value_mean = 0.8, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank by mean works with two algorithms (reverse order) for one case, small values are better", {
  data <- rbind(
            data.frame(algo = "A2", value = 0.8, case = "C1"),
            data.frame(algo = "A1", value = 0.6, case = "C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm = "algo", case = "case", value = "value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind("A2" = data.frame(value_mean = 0.8, rank = 2),
                           "A1" = data.frame(value_mean = 0.6, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank by mean works with two algorithms for one case, large values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 2),
    "A2" = data.frame(value_mean = 0.8, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank by mean works with two algorithms (reverse order) for one case, large values are better", {
  data <- rbind(
    data.frame(algo = "A2", value = 0.8, case = "C1"),
    data.frame(algo = "A1", value = 0.6, case = "C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm = "algo", case = "case", value = "value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind("A2" = data.frame(value_mean = 0.8, rank = 1),
                           "A1" = data.frame(value_mean = 0.6, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank raises error for invalid aggregation function", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%aggregateThenRank(FUN = meanx),
               "object 'meanx' not found", fixed = TRUE)
})

test_that("aggregate-then-rank by mean works with two algorithms for one case and 'min' as ties method", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean, ties.method = "min")

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 1),
    "A2" = data.frame(value_mean = 0.6, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank by mean works with two algorithms for one case and 'max' as ties method", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean, ties.method = "max")

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 2),
    "A2" = data.frame(value_mean = 0.6, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank raises error for invalid ties method", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%aggregateThenRank(FUN = mean, ties.method = "maxx"),
               "'arg' should be one of \"average\", \"first\", \"last\", \"random\", \"max\", \"min\"", fixed = TRUE)
})

test_that("aggregate-then-rank raises error for invalid ties method even when no ties present", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%aggregateThenRank(FUN = mean, ties.method = "maxx"),
               "'arg' should be one of \"average\", \"first\", \"last\", \"random\", \"max\", \"min\"", fixed = TRUE)
})

test_that("aggregate-then-rank by mean works with two algorithms for two cases", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A1", value=0.4, case="C2"),
    data.frame(algo="A2", value=0.8, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.5, rank = 1),
    "A2" = data.frame(value_mean = 0.9, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank by median works with two algorithms for two cases", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A1", value=0.4, case="C2"),
    data.frame(algo="A2", value=0.8, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = median)

  expectedRanking <- rbind(
    "A1" = data.frame(value_median = 0.5, rank = 1),
    "A2" = data.frame(value_median = 0.9, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank by mean works with one algorithm for one case", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank raises error when no NA treatment specified but NAs are contained", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  expect_error(challenge%>%aggregateThenRank(FUN = mean),
               "argument \"na.treat\" is missing, with no default", fixed = TRUE)
})

test_that("aggregate-then-rank raises error when invalid NA treatment specified and NAs are contained", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  expect_error(challenge%>%aggregateThenRank(FUN = mean, na.treat = "na.rmx"),
               "Argument 'na.treat' is invalid. It can be 'na.rm', numeric value or function.", fixed = TRUE)
})

test_that("specified NA treatment does not influence ranking when no NAs are contained", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean, na.treat = 0)

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 2),
    "A2" = data.frame(value_mean = 0.8, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("NAs are replaced by numeric value", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean, na.treat = 0)

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = 0.0, rank = 2),
    "A2" = data.frame(value_mean = 0.8, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("NAs are replaced by function value", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  replacementFunction <- function(x) { -1 }

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean, na.treat = replacementFunction)

  expectedRanking <- rbind(
    "A1" = data.frame(value_mean = -1.0, rank = 2),
    "A2" = data.frame(value_mean = 0.8, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("NAs are removed", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean, na.treat = "na.rm")

  expectedRanking <- rbind(
    "A2" = data.frame(value_mean = 0.8, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("aggregate-then-rank by mean works for multi-task challenge (2 tasks in data set), no missing data", {
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

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRankingTask1 <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 1),
    "A2" = data.frame(value_mean = 0.8, rank = 2))

  expectedRankingTask2 <- rbind(
    "A1" = data.frame(value_mean = 0.5, rank = 2),
    "A2" = data.frame(value_mean = 0.4, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRankingTask1)
  expect_equal(ranking$matlist$T2, expectedRankingTask2)
})

test_that("NAs are replaced by numeric value in multi-task challenge (2 tasks in data set)", {
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

  ranking <- challenge%>%aggregateThenRank(FUN = mean, na.treat = 100)

  expectedRankingTask1 <- rbind(
    "A1" = data.frame(value_mean = 0.6, rank = 1),
    "A2" = data.frame(value_mean = 0.8, rank = 2))

  expectedRankingTask2 <- rbind(
    "A1" = data.frame(value_mean = 100.0, rank = 2),
    "A2" = data.frame(value_mean = 0.4, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRankingTask1)
  expect_equal(ranking$matlist$T2, expectedRankingTask2)
})

test_that("aggregate-then-rank raises error when no NA treatment specified but NAs are contained in multi-task challenge (2 tasks in data set)", {
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

  expect_error(challenge%>%aggregateThenRank(FUN = mean),
               "argument \"na.treat\" is missing, with no default", fixed = TRUE)
})
