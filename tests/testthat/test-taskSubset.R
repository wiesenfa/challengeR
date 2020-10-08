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

  expect_equal(length(rankingBootstrappedSubset$bootsrappedRanks), 1)
  expect_is(rankingBootstrappedSubset$bootsrappedRanks$T2, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$bootsrappedAggregate), 1)
  expect_is(rankingBootstrappedSubset$bootsrappedAggregate$T2, "data.frame")
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

  expect_equal(length(rankingBootstrappedSubset$bootsrappedRanks), 1)
  expect_is(rankingBootstrappedSubset$bootsrappedRanks$T1, "data.frame")

  expect_equal(length(rankingBootstrappedSubset$bootsrappedAggregate), 1)
  expect_is(rankingBootstrappedSubset$bootsrappedAggregate$T1, "data.frame")
})

test_that("extraction of task subset from bootstrap ranking does not raise an error for invalid task name", {
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

  rankingBootstrappedSubset <- subset(rankingBootstrapped, tasks=c("T1x"))

  expect_equal(length(rankingBootstrappedSubset$matlist), 1)
  expect_equal(rankingBootstrappedSubset$matlist$T1, NULL)

  expect_equal(length(rankingBootstrappedSubset$data), 1)
  expect_equal(rankingBootstrappedSubset$data$T1, NULL)

  expect_equal(length(rankingBootstrappedSubset$bootsrappedRanks), 1)
  expect_equal(rankingBootstrappedSubset$bootsrappedRanks$T1, NULL)

  expect_equal(length(rankingBootstrappedSubset$bootsrappedAggregate), 1)
  expect_equal(rankingBootstrappedSubset$bootsrappedAggregate$T1, NULL)
})

