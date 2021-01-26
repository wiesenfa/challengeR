# Copyright (c) German Cancer Research Center (DKFZ)
# All rights reserved.
#
# This file is part of challengeR.
#
# challengeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# challengeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with challengeR. If not, see <https://www.gnu.org/licenses/>.

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
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

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
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

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
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.8, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

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

test_that("PDF report for multi-task data set with bootstrapping is created", {
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
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.2, case="C1"),
                       data.frame(algo="A3", value=0.3, case="C1"),
                       data.frame(algo="A1", value=0.6, case="C2"),
                       data.frame(algo="A2", value=0.7, case="C2"),
                       data.frame(algo="A3", value=0.8, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  meanRanks <- ranking%>%consensus(method = "euclidean")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  rankingBootstrapped %>%
    report(consensus=meanRanks,
           title="Test Challenge",
           file="testthat_multi_task_bootstrapping",
           format="PDF",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_multi_task_bootstrapping.pdf"))

  # Clean up
  if (file.exists("testthat_multi_task_bootstrapping.pdf")) {
    file.remove("testthat_multi_task_bootstrapping.pdf")
  }

})

test_that("HTML report for multi-task data set with bootstrapping is created", {
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
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.2, case="C1"),
                       data.frame(algo="A3", value=0.3, case="C1"),
                       data.frame(algo="A1", value=0.6, case="C2"),
                       data.frame(algo="A2", value=0.7, case="C2"),
                       data.frame(algo="A3", value=0.8, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  meanRanks <- ranking%>%consensus(method = "euclidean")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  rankingBootstrapped %>%
    report(consensus=meanRanks,
           title="Test Challenge",
           file="testthat_multi_task_bootstrapping",
           format="HTML",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_multi_task_bootstrapping.html"))

  # Clean up
  if (file.exists("testthat_multi_task_bootstrapping.html")) {
    file.remove("testthat_multi_task_bootstrapping.html")
  }

})

test_that("Word report for multi-task data set with bootstrapping is created", {
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
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.1, case="C1"),
                       data.frame(algo="A2", value=0.2, case="C1"),
                       data.frame(algo="A3", value=0.3, case="C1"),
                       data.frame(algo="A1", value=0.6, case="C2"),
                       data.frame(algo="A2", value=0.7, case="C2"),
                       data.frame(algo="A3", value=0.8, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  meanRanks <- ranking%>%consensus(method = "euclidean")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  rankingBootstrapped %>%
    report(consensus=meanRanks,
           title="Test Challenge",
           file="testthat_multi_task_bootstrapping",
           format="Word",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_multi_task_bootstrapping.docx"))

  # Clean up
  if (file.exists("testthat_multi_task_bootstrapping.docx")) {
    file.remove("testthat_multi_task_bootstrapping.docx")
  }

})

test_that("PDF report for multi-task data set with bootstrapping is created (#algorithms < #tasks)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.80, case="C1"),
                       data.frame(algo="A2", value=0.60, case="C1"),
                       data.frame(algo="A1", value=0.85, case="C2"),
                       data.frame(algo="A2", value=0.65, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.20, case="C1"),
                       data.frame(algo="A2", value=0.30, case="C1"),
                       data.frame(algo="A1", value=0.25, case="C2"),
                       data.frame(algo="A2", value=0.35, case="C2")
                     ))
  dataTask3 <- cbind(task="T3",
                     rbind(
                       data.frame(algo="A1", value=0.10, case="C1"),
                       data.frame(algo="A2", value=0.80, case="C1"),
                       data.frame(algo="A1", value=0.15, case="C2"),
                       data.frame(algo="A2", value=0.85, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2, dataTask3)

  challenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")

  meanRanks <- ranking%>%consensus(method = "euclidean")

  set.seed(1)
  rankingBootstrapped <- ranking%>%bootstrap(nboot=10)

  rankingBootstrapped %>%
    report(consensus=meanRanks,
           title="Test Challenge",
           file="testthat_multi_task_bootstrapping_more_tasks_than_algorithms",
           format="PDF",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_multi_task_bootstrapping_more_tasks_than_algorithms.pdf"))

  # Clean up
  if (file.exists("testthat_multi_task_bootstrapping_more_tasks_than_algorithms.pdf")) {
    file.remove("testthat_multi_task_bootstrapping_more_tasks_than_algorithms.pdf")
  }

})
