test_that("blob plot for visualizing ranking stability by algorithm raises error for single-task data set", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A3", value=0.4, case="C1"),
    data.frame(algo="A1", value=0.2, case="C2"),
    data.frame(algo="A2", value=0.1, case="C2"),
    data.frame(algo="A3", value=0.0, case="C2"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  expect_error(stabilityByAlgorithm(rankingBootstrapped),
               "The stability of rankings by algorithm cannot be computed for less than two tasks.", fixed=TRUE)
})

test_that("blob plot for visualizing ranking stability by algorithm returns one plot for multi-task data set", {
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

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  actualPlot <- stabilityByAlgorithm(rankingBootstrapped)
  expect_is(actualPlot, "ggplot")
})
