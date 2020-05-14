test_that("PDF report for single-task data set without bootstrapping is created", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  ranking %>%
    report(title="Test Challenge",
           file="testthat_single_task_no_bootstrapping",
           format="PDF",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_no_bootstrapping.pdf"))

  # Clean up
  if (file.exists("testthat_single_task_no_bootstrapping.pdf")) {
    file.remove("testthat_single_task_no_bootstrapping.pdf")
  }

})

test_that("HTML report for single-task data set without bootstrapping is created", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  ranking %>%
    report(title="Test Challenge",
           file="testthat_single_task_no_bootstrapping",
           format="HTML",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_no_bootstrapping.html"))

  # Clean up
  if (file.exists("testthat_single_task_no_bootstrapping.html")) {
    file.remove("testthat_single_task_no_bootstrapping.html")
  }

})

test_that("Word report for single-task data set without bootstrapping is created", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  ranking %>%
    report(title="Test Challenge",
           file="testthat_single_task_no_bootstrapping",
           format="Word",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_no_bootstrapping.docx"))

  # Clean up
  if (file.exists("testthat_single_task_no_bootstrapping.docx")) {
    file.remove("testthat_single_task_no_bootstrapping.docx")
  }

})

test_that("PDF report for single-task data set with bootstrapping is created", {
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

  rankingBootstrapped %>%
    report(title="Test Challenge",
           file="testthat_single_task_bootstrapping",
           format="PDF",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_bootstrapping.pdf"))

  # Clean up
  if (file.exists("testthat_single_task_bootstrapping.pdf")) {
    file.remove("testthat_single_task_bootstrapping.pdf")
  }

})

test_that("HTML report for single-task data set with bootstrapping is created", {
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

  rankingBootstrapped %>%
    report(title="Test Challenge",
           file="testthat_single_task_bootstrapping",
           format="HTML",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_bootstrapping.html"))

  # Clean up
  if (file.exists("testthat_single_task_bootstrapping.html")) {
    file.remove("testthat_single_task_bootstrapping.html")
  }

})

test_that("Word report for single-task data set with bootstrapping is created", {
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

  rankingBootstrapped %>%
    report(title="Test Challenge",
           file="testthat_single_task_bootstrapping",
           format="Word",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_bootstrapping.docx"))

  # Clean up
  if (file.exists("testthat_single_task_bootstrapping.docx")) {
    file.remove("testthat_single_task_bootstrapping.docx")
  }

})

test_that("PDF report for multi-task data set without bootstrapping is created", {
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

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  meanRanks <- ranking%>%consensus(method = "euclidean")

  ranking %>%
    report(consensus=meanRanks,
           title="Test Challenge",
           file="testthat_multi_task_no_bootstrapping",
           format="PDF",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_multi_task_no_bootstrapping.pdf"))

  # Clean up
  if (file.exists("testthat_multi_task_no_bootstrapping.pdf")) {
    file.remove("testthat_multi_task_no_bootstrapping.pdf")
  }

})

test_that("HTML report for multi-task data set without bootstrapping is created", {
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

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  meanRanks <- ranking%>%consensus(method = "euclidean")

  ranking %>%
    report(consensus=meanRanks,
           title="Test Challenge",
           file="testthat_multi_task_no_bootstrapping",
           format="HTML",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_multi_task_no_bootstrapping.html"))

  # Clean up
  if (file.exists("testthat_multi_task_no_bootstrapping.html")) {
    file.remove("testthat_multi_task_no_bootstrapping.html")
  }

})

test_that("Word report for multi-task data set without bootstrapping is created", {
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

  data <- rbind(dataTask1, dataTask2)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  meanRanks <- ranking%>%consensus(method = "euclidean")

  ranking %>%
    report(consensus=meanRanks,
           title="Test Challenge",
           file="testthat_multi_task_no_bootstrapping",
           format="Word",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_multi_task_no_bootstrapping.docx"))

  # Clean up
  if (file.exists("testthat_multi_task_no_bootstrapping.docx")) {
    file.remove("testthat_multi_task_no_bootstrapping.docx")
  }

})
