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

  rankingSubset <- taskSubset(ranking, tasks=c("T2"))

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

  rankingSubset <- taskSubset(ranking, tasks=c("T1"))

  expect_equal(length(rankingSubset$matlist), 1)
  expect_is(rankingSubset$matlist$T1, "data.frame")

  expect_equal(length(rankingSubset$data), 1)
  expect_is(rankingSubset$data$T1, "data.frame")
})

test_that("extraction of task subset does not raise an error for invalid task name", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=mean, ties.method="min")

  rankingSubset <- taskSubset(ranking, tasks=c("T1x"))

  expect_equal(length(rankingSubset$matlist), 1)
  expect_equal(rankingSubset$matlist$T1, NULL)

  expect_equal(length(rankingSubset$data), 1)
  expect_equal(rankingSubset$data$T1, NULL)
})
