test_that("Single task bootstrapping with 1 test case stopped with message", {
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


test_that("Multi task bootstrapping, all tasks with 1 test case stopped with message", {
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


test_that("Multi task bootstrapping, only one task with >1 test case continued with message", {
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


