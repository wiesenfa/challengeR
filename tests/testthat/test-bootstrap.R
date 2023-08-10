test_that("single-task bootstrapping with 1 test case stopped with message", {
  dataTask1 <- cbind(task="T1",
                   rbind(
                     data.frame(algo="A1", value=0.8, case="C1"),
                     data.frame(algo="A2", value=0.6, case="C1")
                   ))


challenge <- as.challenge(dataTask1,  algorithm="algo", case="case", value="value", smallBetter=FALSE)

ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

set.seed(1)
expect_error(rankingBootstrapped <- ranking%>%bootstrap(nboot=10),
             "Only 1 test case included. Bootstrapping with 1 test case not sensible.", fixed = TRUE)
})


test_that("multi-task bootstrapping, all tasks with 1 test case stopped with message", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.6, case="C1")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A2", value=0.3, case="C1")
                     ))
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  set.seed(1)
  expect_error(rankingBootstrapped <- ranking%>%bootstrap(nboot=10),
               "All tasks only contained 1 test case. Bootstrapping with 1 test case not sensible.", fixed = TRUE)
})


test_that("multi-task bootstrapping, only one task with >1 test case continued with message", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.6, case="C1")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A2", value=0.3, case="C1"),
                       data.frame(algo="A1", value=0.2, case="C2"),
                       data.frame(algo="A2", value=0.3, case="C2")
                     ))
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  set.seed(1)
  expect_message(rankingBootstrapped <- ranking%>%bootstrap(nboot=3),
               "Task(s) T1, T3 with only 1 test case excluded from bootstrapping.", fixed = TRUE)
})


test_that("two sequential bootstrappings yield same results", {
  data <- read.csv(system.file("extdata", "data_matrix.csv", package="challengeR", mustWork=TRUE))

  challenge <- as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%rankThenAggregate(FUN=mean, ties.method="min")

  set.seed(123, kind="L'Ecuyer-CMRG")
  rankingBootstrapped1 <- ranking%>%bootstrap(nboot=10)

  set.seed(123, kind="L'Ecuyer-CMRG")
  rankingBootstrapped2 <- ranking%>%bootstrap(nboot=10)

  expect_equal(rankingBootstrapped1, rankingBootstrapped2)
})


test_that("two parallel bootstrappings yield same results", {
  data <- read.csv(system.file("extdata", "data_matrix.csv", package="challengeR", mustWork=TRUE))

  challenge <- as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%rankThenAggregate(FUN=mean, ties.method="min")

  library(doParallel)
  library(doRNG)
  numCores <- detectCores(logical=FALSE)
  registerDoParallel(cores=numCores)

  registerDoRNG(123)
  rankingBootstrapped1 <- ranking%>%bootstrap(nboot=10, parallel=TRUE, progress="none")

  registerDoRNG(123)
  rankingBootstrapped2 <- ranking%>%bootstrap(nboot=10, parallel=TRUE, progress="none")

  stopImplicitCluster()

  expect_equal(rankingBootstrapped1, rankingBootstrapped2)
})
