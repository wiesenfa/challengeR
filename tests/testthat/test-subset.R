test_that("top 2 performing algorithms are extracted and data set is reduced respectively", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  rankingSubset <- subset(ranking, top=2)

  expectedRankingSubset <- rbind(
    "A1" = data.frame(value_FUN = 0.5, rank = 1),
    "A2" = data.frame(value_FUN = 0.35, rank = 2))

  expect_equal(rankingSubset$matlist$T1, expectedRankingSubset)

  expect_equal(as.vector(rankingSubset$data$T1$algo), c("A1", "A2", "A1", "A2"))
  expect_equal(as.vector(rankingSubset$data$T1$value), c(0.8, 0.6, 0.2, 0.1))
  expect_equal(as.vector(rankingSubset$data$T1$case), c("C1", "C1", "C2", "C2"))
  expect_equal(as.vector(rankingSubset$data$T1$task), c("T1", "T1", "T1", "T1"))
})

test_that("extraction of subset raises error for multi-task data set", {
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

  expect_error(subset(ranking, top=2),
               "Subset of algorithms only sensible for single-task challenges.", fixed=TRUE)
})

test_that("extraction of subset returns all algorithms even when more are requested", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  rankingSubset <- subset(ranking, top=4)

  expectedRankingSubset <- rbind(
    "A1" = data.frame(value_FUN = 0.5, rank = 1),
    "A2" = data.frame(value_FUN = 0.35, rank = 2),
    "A3" = data.frame(value_FUN = 0.2, rank = 3))

  expect_equal(rankingSubset$matlist$T1, expectedRankingSubset)
})

test_that("extraction of subset returns more algorithms then requested when ties are present", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"),
    data.frame(algo="A3", value=0.8, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.2, case="C2"),
    data.frame(algo="A3", value=0.2, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  rankingSubset <- subset(ranking, top=2)

  expectedRankingSubset <- rbind(
    "A1" = data.frame(value_FUN = 0.5, rank = 1),
    "A2" = data.frame(value_FUN = 0.5, rank = 1),
    "A3" = data.frame(value_FUN = 0.5, rank = 1))

  expect_equal(rankingSubset$matlist$T1, expectedRankingSubset)
})
