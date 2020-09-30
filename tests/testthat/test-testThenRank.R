test_that("test-then-rank raises warning for one case", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_warning(ranking <- challenge%>%testThenRank(),
                 "Only one case in task.", fixed = TRUE)

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("test-then-rank raises warning for one algorithm", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_warning(ranking <- challenge%>%testThenRank(),
                 "Only one algorithm available in task 'T1'.", fixed = TRUE)
})

test_that("test-then-rank works with two algorithms, small values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.2, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank()

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 1, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("test-then-rank works with two algorithms, large values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.2, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%testThenRank()

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 2),
    "A2" = data.frame(prop_significance = 1, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("test-then-rank works for ties method 'max'", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A1", value=0.6, case="C2"),
    data.frame(algo="A2", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.8, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank(ties.method = "max")

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 2),
    "A2" = data.frame(prop_significance = 0, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("test-then-rank raises error for invalid ties method", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A1", value=0.6, case="C2"),
    data.frame(algo="A2", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.8, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%testThenRank(ties.method = "maxx"),
               "'arg' should be one of \"average\", \"first\", \"last\", \"random\", \"max\", \"min\"", fixed = TRUE)
})

test_that("test-then-rank raises error for invalid ties method even when no ties present", {
  data <- rbind(
    data.frame(algo="A1", value=0.2, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%testThenRank(ties.method = "maxx"),
               "'arg' should be one of \"average\", \"first\", \"last\", \"random\", \"max\", \"min\"", fixed = TRUE)
})

test_that("test-then-rank raises error when no NA treatment specified but NAs are contained", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%testThenRank(),
               "argument \"na.treat\" is missing, with no default", fixed = TRUE)
})

test_that("test-then-rank raises error when invalid NA treatment specified and NAs are contained", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%testThenRank(na.treat = "na.rmx"),
               "Argument 'na.treat' is invalid. It can be 'na.rm', numeric value or function.", fixed = TRUE)
})

test_that("specified NA treatment does not influence ranking when no NAs are contained", {
  data <- rbind(
    data.frame(algo="A1", value=0.2, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank(na.treat = 0)

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 1, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("NAs are replaced by numeric value", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank(na.treat = 100.0)

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("NAs are replaced by function value", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  replacementFunction <- function(x) { 0.0 }

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank(na.treat = replacementFunction)

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 1, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 2))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("NAs are removed", {
  data <- rbind(
    data.frame(algo="A1", value=NA, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A1", value=0.2, case="C3"),
    data.frame(algo="A1", value=0.2, case="C4"),
    data.frame(algo="A2", value=1.0, case="C1"),
    data.frame(algo="A2", value=1.0, case="C2"),
    data.frame(algo="A2", value=1.0, case="C3"),
    data.frame(algo="A2", value=1.0, case="C4"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank(na.treat = "na.rm")

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})

test_that("test-then-rank works for multi-task data set with no missing data", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A1", value=0.2, case="C2"),
                       data.frame(algo="A1", value=0.2, case="C3"),
                       data.frame(algo="A1", value=0.2, case="C4"),
                       data.frame(algo="A2", value=1.0, case="C1"),
                       data.frame(algo="A2", value=1.0, case="C2"),
                       data.frame(algo="A2", value=1.0, case="C3"),
                       data.frame(algo="A2", value=1.0, case="C4")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.6, case="C1"),
                       data.frame(algo="A1", value=0.6, case="C2"),
                       data.frame(algo="A2", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank()

  expectedRankingTask1 <- rbind(
    "A1" = data.frame(prop_significance = 1, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 2))

  expectedRankingTask2 <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRankingTask1)
  expect_equal(ranking$matlist$T2, expectedRankingTask2)
})

test_that("NAs are replaced by numeric value in multi-task data set", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=NA, case="C1"),
                       data.frame(algo="A1", value=0.2, case="C2"),
                       data.frame(algo="A1", value=0.2, case="C3"),
                       data.frame(algo="A1", value=0.2, case="C4"),
                       data.frame(algo="A2", value=1.0, case="C1"),
                       data.frame(algo="A2", value=1.0, case="C2"),
                       data.frame(algo="A2", value=1.0, case="C3"),
                       data.frame(algo="A2", value=1.0, case="C4")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.6, case="C1"),
                       data.frame(algo="A1", value=0.6, case="C2"),
                       data.frame(algo="A2", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%testThenRank(na.treat = 0)

  expectedRankingTask1 <- rbind(
    "A1" = data.frame(prop_significance = 1, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 2))

  expectedRankingTask2 <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRankingTask1)
  expect_equal(ranking$matlist$T2, expectedRankingTask2)
})

test_that("test-then-rank raises error when no NA treatment specified but NAs are contained in multi-task data set", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A1", value=0.2, case="C2"),
                       data.frame(algo="A1", value=0.2, case="C3"),
                       data.frame(algo="A1", value=0.2, case="C4"),
                       data.frame(algo="A2", value=1.0, case="C1"),
                       data.frame(algo="A2", value=1.0, case="C2"),
                       data.frame(algo="A2", value=1.0, case="C3"),
                       data.frame(algo="A2", value=1.0, case="C4")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.6, case="C1"),
                       data.frame(algo="A1", value=0.6, case="C2"),
                       data.frame(algo="A2", value=NA, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_error(challenge%>%testThenRank(),
               "argument \"na.treat\" is missing, with no default", fixed = TRUE)
})
