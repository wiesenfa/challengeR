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

test_that("empty attribute 'taskName' raises error for single-task challenge", {
  data <- rbind(
      data.frame(algo="A1", value=0.8, case="C1"),
      data.frame(algo="A2", value=0.6, case="C1"))

  expect_error(as.challenge(data, taskName="", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Argument 'taskName' is empty.", fixed=TRUE)
})

test_that("only whitespaces in attribute 'taskName' raises error for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  expect_error(as.challenge(data, taskName="  ", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Argument 'taskName' is empty.", fixed=TRUE)
})

test_that("attributes are set for single-task challenge with specified task name", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  expect_equal(attr(actualChallenge, "annotator"), NULL)
  expect_equal(attr(actualChallenge, "by"), "task")
  expect_equal(attr(actualChallenge, "smallBetter"), FALSE)
  expect_equal(attr(actualChallenge, "check"), TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C1"))
  expect_equal(as.vector(actualChallenge$T1$task), c("T1", "T1"))

  # expect that there's no attribute "task"
  expect_equal(attr(actualChallenge, "task"), NULL)
  expect_equal(attr(actualChallenge$T1, "task"), NULL)
  expect_equal(attr(actualChallenge$T2, "task"), NULL)
})

test_that("attributes are set for single-task challenge with dummy task name", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE)

  expect_equal(attr(actualChallenge, "annotator"), NULL)
  expect_equal(attr(actualChallenge, "by"), "task")
  expect_equal(attr(actualChallenge, "smallBetter"), FALSE)
  expect_equal(attr(actualChallenge, "check"), TRUE)

  expect_equal(as.vector(actualChallenge$dummyTask$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$dummyTask$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$dummyTask$case), c("C1", "C1"))
  expect_equal(as.vector(actualChallenge$dummyTask$task), c("dummyTask", "dummyTask"))

  # expect that there's no attribute "task"
  expect_equal(attr(actualChallenge, "task"), NULL)
  expect_equal(attr(actualChallenge$dummyTask, "task"), NULL)
  expect_equal(attr(actualChallenge$dummyTask, "task"), NULL)
})

test_that("leading and trailing whitespaces are trimmed for attribute 'taskName'", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  actualChallenge <- as.challenge(data, taskName=" T1  ", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A2"))
})

test_that("attributes are set for multi-task challenge", {
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

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=TRUE)

  expect_equal(attr(actualChallenge, "annotator"), NULL)
  expect_equal(attr(actualChallenge, "by"), "task")
  expect_equal(attr(actualChallenge, "smallBetter"), TRUE)
  expect_equal(attr(actualChallenge, "check"), TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C1"))
  expect_equal(as.vector(actualChallenge$T1$task), c("T1", "T1"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.2, 0.3))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C1"))
  expect_equal(as.vector(actualChallenge$T2$task), c("T2", "T2"))

  # expect that there's no attribute "task"
  expect_equal(attr(actualChallenge, "task"), NULL)
  expect_equal(attr(actualChallenge$T1, "task"), NULL)
  expect_equal(attr(actualChallenge$T2, "task"), NULL)
})

test_that("attributes are set for multi-task challenge with sanity check disabled", {
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

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=TRUE, check=FALSE)

  expect_equal(attr(actualChallenge, "annotator"), NULL)
  expect_equal(attr(actualChallenge, "by"), "task")
  expect_equal(attr(actualChallenge, "smallBetter"), TRUE)
  expect_equal(attr(actualChallenge, "check"), FALSE)
  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2", "A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6, 0.2, 0.3))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C1", "C1", "C1"))
  expect_equal(as.vector(actualChallenge$task), c("T1", "T1", "T2", "T2"))
})

test_that("attribute 'taskName' is ignored for multi-task challenge", {
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

  expect_warning(as.challenge(data, taskName="T1", by="task", algorithm="algo", case="case", value="value", smallBetter=TRUE),
                 "Argument 'taskName' is ignored for multi-task data set.", fixed=TRUE)
})

test_that("missing algorithm performances are added as NAs for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C2"))

  expect_message(actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms has been observed for all cases.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, NA, NA, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))
  expect_equal(as.vector(actualChallenge$T1$task), c("T1", "T1", "T1", "T1"))
})

test_that("multi-task data set containing one task is interpreted as single-task data set, missing algorithm performances are added", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A2", value=0.6, case="C2")
                ))

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_message(actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms has been observed for all cases.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, NA, NA, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))
})

test_that("missing algorithm performances are added as NAs for multi-task challenge (2 tasks in data set)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.6, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A1", value=0.3, case="C2"),
                       data.frame(algo="A2", value=0.4, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  expect_message(actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms has been observed for all cases in task 'T1'.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, NA, NA, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))
  expect_equal(as.vector(actualChallenge$T1$task), c("T1", "T1", "T1", "T1"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.2, 0.3, 0.4, NA))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2", "C1", "C2"))
  expect_equal(as.vector(actualChallenge$T2$task), c("T2", "T2", "T2", "T2"))
})

test_that("missing algorithm performances are not added as NA with sanity check disabled for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C2"))

  actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE, check=FALSE)

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2"))
})

test_that("missing algorithm performances are not added as NA with sanity check disabled for multi-task challenge (2 tasks in data set)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.6, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A1", value=0.3, case="C2"),
                       data.frame(algo="A2", value=0.4, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE, check=FALSE)

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2", "A1", "A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6, 0.2, 0.3, 0.4))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2", "C1", "C2", "C1"))
})

test_that("case cannot appear more than once per algorithm for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A1", value=0.8, case="C1"))

  expect_error(as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm. Please revise. Or are you considering a multi-task challenge and forgot to specify argument 'by'?\nCase(s): C1", fixed=TRUE)
})

test_that("multi-task data set containing one task is interpreted as single-task data set, case cannot appear more than once per algorithm", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A1", value=0.8, case="C1")
                ))

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_error(as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm. Please revise. Or are you considering a multi-task challenge and forgot to specify argument 'by'?\nCase(s): C1", fixed=TRUE)
})

test_that("case cannot appear more than once per algorithm for multi-task challenge (1 task in data set)", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A1", value=0.8, case="C1")
                ))

  expect_error(as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm. Please revise. Or are you considering a multi-task challenge and forgot to specify argument 'by'?\nCase(s): C1", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.7, case="C1"),
    data.frame(algo="A1", value=0.5, case="C2"),
    data.frame(algo="A2", value=0.6, case="C2"),
    data.frame(algo="A2", value=0.6, case="C2"))

  expect_error(as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm. Please revise. Or are you considering a multi-task challenge and forgot to specify argument 'by'?\nCase(s): C1, C2", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm for multi-task challenge (1 task in data set)", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A2", value=0.7, case="C1"),
                  data.frame(algo="A1", value=0.5, case="C2"),
                  data.frame(algo="A2", value=0.6, case="C2"),
                  data.frame(algo="A2", value=0.6, case="C2")
                ))

  expect_error(as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm. Please revise. Or are you considering a multi-task challenge and forgot to specify argument 'by'?\nCase(s): C1, C2", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm for multi-task challenge (2 tasks in data set)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1") # let T1 pass
                     ))

  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.7, case="C1"),
                       data.frame(algo="A1", value=0.5, case="C2"),
                       data.frame(algo="A2", value=0.6, case="C2"),
                       data.frame(algo="A2", value=0.6, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  expect_error(as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm in task 'T2'. Please revise.\nCase(s): C1, C2", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm when missing data was added for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C2"),
    data.frame(algo="A2", value=0.6, case="C2"))

  expect_error(as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm. Please revise. Or are you considering a multi-task challenge and forgot to specify argument 'by'?\nCase(s): C1, C2", fixed=TRUE)
})

test_that("user is notified of duplicate cases when multi-task data set is interpreted as single-task data set (2 tasks in data set)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1")
                     ))

  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_error(as.challenge(data, taskName="New task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "The following case(s) appear(s) more than once for the same algorithm. Please revise. Or are you considering a multi-task challenge and forgot to specify argument 'by'?\nCase(s): C1", fixed=TRUE)
})

test_that("user is notified of missing algorithm performance when multi-task data set is interpreted as single-task data set (2 tasks in data set)", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1")
                     ))

  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A2", value=0.6, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_message(as.challenge(data, taskName="New task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms has been observed for all cases.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)
})

test_that("NAs are replaced by numeric value for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A1", value=NA, case="C2"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A2", value=NA, case="C2"))

  actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=0)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.0, 0.6, 0.0))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))
})

test_that("NAs are replaced by numeric value for multi-task challenge", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A1", value=NA, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A2", value=NA, case="C1"),
                       data.frame(algo="A2", value=0.5, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=0)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.0))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A2", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.0, 0.5))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2"))
})

test_that("NAs are replaced by function value for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A1", value=NA, case="C2"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A2", value=NA, case="C2"))

  replacementFunction <- function(x) { 2 }

  actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=replacementFunction)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 2.0, 0.6, 2.0))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))
})

test_that("NAs are replaced by function value for multi-task challenge", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A1", value=NA, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A2", value=NA, case="C1"),
                       data.frame(algo="A2", value=0.5, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  replacementFunction <- function(x) { 2 }

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=replacementFunction)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 2.0))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A2", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(2.0, 0.5))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2"))
})

test_that("NAs are removed for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A1", value=NA, case="C2"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A2", value=NA, case="C2"))

  actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat="na.rm")

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C1"))
})

test_that("NAs are removed for multi-task challenge", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A1", value=NA, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A2", value=NA, case="C1"),
                       data.frame(algo="A2", value=0.5, case="C2")
                     ))

  data <- rbind(dataTask1, dataTask2)

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat="na.rm")

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.5))
  expect_equal(as.vector(actualChallenge$T2$case), c("C2"))
})

test_that("automatically added NAs are replaced by numeric value for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C2"))

  expect_message(actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=0),
                 "Performance of not all algorithms has been observed for all cases.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.0, 0.0, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))
})

test_that("automatically added NAs are replaced by numeric value for multi-task challenge", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.6, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A1", value=0.3, case="C2"),
                       data.frame(algo="A2", value=0.4, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  expect_message(actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=0),
                 "Performance of not all algorithms has been observed for all cases in task 'T1'.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.0, 0.0, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.2, 0.3, 0.4, 0.0))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2", "C1", "C2"))
})

test_that("automatically added NAs are removed for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C2"))

  expect_message(actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat="na.rm"),
                 "Performance of not all algorithms has been observed for all cases.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2"))
})

test_that("automatically added NAs are removed for multi-task challenge", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.8, case="C1"),
                       data.frame(algo="A2", value=0.6, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.2, case="C1"),
                       data.frame(algo="A1", value=0.3, case="C2"),
                       data.frame(algo="A2", value=0.4, case="C1")
                     ))

  data <- rbind(dataTask1, dataTask2)

  expect_message(actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat="na.rm"),
                 "Performance of not all algorithms has been observed for all cases in task 'T1'.\nTherefore, missings have been inserted in the following cases:", fixed=TRUE)

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A1", "A1", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.2, 0.3, 0.4))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2", "C1"))
})

test_that("class of 'algorithm' column must be 'factor' for single-task challenge", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A1", value=0.7, case="C2"),
    data.frame(algo="A2", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.5, case="C2"))
  
  actualChallenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=0)
  
  expect_equal(class(actualChallenge$T1$algo), "factor")
})  

test_that("class of 'algorithm' column must be 'factor' for multi-task challenge", {
  dataTask1 <- cbind(task="T1",
                     rbind(
                       data.frame(algo="A1", value=0.81, case="C1"),
                       data.frame(algo="A2", value=0.72, case="C1"),
                       data.frame(algo="A1", value=0.65, case="C2"),
                       data.frame(algo="A2", value=0.95, case="C2")
                     ))
  dataTask2 <- cbind(task="T2",
                     rbind(
                       data.frame(algo="A1", value=0.75, case="C1"),
                       data.frame(algo="A2", value=0.82, case="C1"),
                       data.frame(algo="A1", value=0.66, case="C2"),
                       data.frame(algo="A2", value=0.84, case="C2")
                     ))
  
  data <- rbind(dataTask1, dataTask2)
  
  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=TRUE)
  
  expect_equal(class(actualChallenge$T1$algo), "factor")
  expect_equal(class(actualChallenge$T2$algo), "factor")
})


