test_that("aggregate-than-rank by mean works with two algorithms for one case, small values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(value_FUN = 0.6, rank = 1),
    "A2" = data.frame(value_FUN = 0.8, rank = 2))

  expect_equal(ranking$mat, expectedRanking)
})

test_that("aggregate-than-rank by mean works with two algorithms (reverse order) for one case, small values are better", {
  data <- rbind(
            data.frame(algo = "A2", value = 0.8, case = "C1"),
            data.frame(algo = "A1", value = 0.6, case = "C1"))

  challenge <- as.challenge(data, algorithm = "algo", case = "case", value = "value", smallBetter = TRUE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind("A2" = data.frame(value_FUN = 0.8, rank = 2),
                           "A1" = data.frame(value_FUN = 0.6, rank = 1))

  expect_equal(ranking$mat, expectedRanking)
})

test_that("aggregate-than-rank by mean works with two algorithms for one case, large values are better", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind(
    "A1" = data.frame(value_FUN = 0.6, rank = 2),
    "A2" = data.frame(value_FUN = 0.8, rank = 1))

  expect_equal(ranking$mat, expectedRanking)
})

test_that("aggregate-than-rank by mean works with two algorithms (reverse order) for one case, large values are better", {
  data <- rbind(
    data.frame(algo = "A2", value = 0.8, case = "C1"),
    data.frame(algo = "A1", value = 0.6, case = "C1"))

  challenge <- as.challenge(data, algorithm = "algo", case = "case", value = "value", smallBetter = FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN = mean)

  expectedRanking <- rbind("A2" = data.frame(value_FUN = 0.8, rank = 1),
                           "A1" = data.frame(value_FUN = 0.6, rank = 2))

  expect_equal(ranking$mat, expectedRanking)
})
