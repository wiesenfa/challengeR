test_that("attributes are set for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A1", value=0.4, case="C2"),
            data.frame(algo="A2", value=0.6, case="C1"),
            data.frame(algo="A2", value=0.7, case="C2"))

  actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE)

  expect_equal(attr(actualChallenge, "annotator"), NULL)
  expect_equal(attr(actualChallenge, "by"), NULL)
  expect_equal(attr(actualChallenge, "largeBetter"), TRUE)
  expect_equal(attr(actualChallenge, "check"), TRUE)
  expect_equal(as.vector(actualChallenge$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.4, 0.6, 0.7))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2", "C1", "C2"))
})

test_that("attributes are set for multi-task challenge with sanity check enabled", {
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

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=TRUE, check=TRUE)

  expect_equal(attr(actualChallenge, "annotator"), NULL)
  expect_equal(attr(actualChallenge, "by"), "task")
  expect_equal(attr(actualChallenge, "largeBetter"), FALSE)
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
  expect_equal(attr(actualChallenge, "largeBetter"), FALSE)
  expect_equal(attr(actualChallenge, "check"), FALSE)
  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2", "A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6, 0.2, 0.3))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C1", "C1", "C1"))
  expect_equal(as.vector(actualChallenge$task), c("T1", "T1", "T2", "T2"))
})

test_that("missing algorithm performances are added as NA with sanity check enabled for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A2", value=0.6, case="C2"))

  expect_message(actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, NA, NA, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2", "C1", "C2"))
})

test_that("missing algorithm performances are not added as NA with sanity check disabled for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A2", value=0.6, case="C2"))

  actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE, check=FALSE)

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2"))
})

test_that("missing algorithm performances are added as NA with sanity check enabled for multi-task challenge (1 task in data set)", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A2", value=0.6, case="C2")
                ))

  expect_message(actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms is observed for all cases in task T1. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, NA, NA, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))
})

test_that("missing algorithm performances are not added as NA with sanity check disabled for multi-task challenge (1 task in data set)", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A2", value=0.6, case="C2")
                ))

  actualChallenge <- as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE, check=FALSE)

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2"))
})

test_that("missing algorithm performances are added as NA with sanity check enabled for multi-task challenge (2 tasks in data set)", {
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
                 "Performance of not all algorithms is observed for all cases in task T1. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, NA, NA, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.2, 0.3, 0.4, NA))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2", "C1", "C2"))
})

test_that("case cannot appear more than once per algorithm with sanity check enabled for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A1", value=0.8, case="C1"))

  expect_error(as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm with sanity check enabled for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A2", value=0.7, case="C1"),
            data.frame(algo="A1", value=0.5, case="C2"),
            data.frame(algo="A2", value=0.6, case="C2"),
            data.frame(algo="A2", value=0.6, case="C2"))

  expect_error(as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm when missing data was added with sanity check enabled for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A2", value=0.6, case="C2"),
            data.frame(algo="A2", value=0.6, case="C2"))

  expect_error(as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("case cannot appear more than once per algorithm with sanity check enabled for multi-task challenge (1 task in data set)", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A1", value=0.8, case="C1")
                ))

  expect_error(as.challenge(data, by="task", algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm in task T1", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm with sanity check enabled for multi-task challenge (1 task in data set)", {
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
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm in task T1", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm with sanity check enabled for multi-task challenge (2 tasks in data set)", {
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
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm in task T2", fixed=TRUE)
})

test_that("multi-task data set containing one task is interpreted as single-task data set, missing algorithm performances are added", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A2", value=0.6, case="C2")
                ))

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_message(actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, NA, NA, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2", "C1", "C2"))
})

test_that("multi-task data set containing one task is interpreted as single-task data set, case cannot appear more than once per algorithm", {
  data <- cbind(task="T1",
                rbind(
                  data.frame(algo="A1", value=0.8, case="C1"),
                  data.frame(algo="A1", value=0.8, case="C1")
                ))

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_error(as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm", fixed=TRUE)
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
  expect_error(as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm", fixed=TRUE)
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
  expect_message(as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE),
               "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:", fixed=TRUE)
})

test_that("NAs are replaced by numeric value for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A1", value=NA, case="C2"),
            data.frame(algo="A2", value=0.6, case="C1"),
            data.frame(algo="A2", value=NA, case="C2"))

  actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=0)

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.0, 0.6, 0.0))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2", "C1", "C2"))
})

test_that("NAs are replaced by function value for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A1", value=NA, case="C2"),
            data.frame(algo="A2", value=0.6, case="C1"),
            data.frame(algo="A2", value=NA, case="C2"))

  replacementFunction <- function(x) { 2 }

  actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=replacementFunction)

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 2.0, 0.6, 2.0))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2", "C1", "C2"))
})

test_that("NAs are removed for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A1", value=NA, case="C2"),
            data.frame(algo="A2", value=0.6, case="C1"),
            data.frame(algo="A2", value=NA, case="C2"))

  actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat="na.rm")

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C1"))
})

test_that("automatically added NAs are replaced by numeric value for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A2", value=0.6, case="C2"))

  expect_message(actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat=0),
                 "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.0, 0.0, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2", "C1", "C2"))
})

test_that("automatically added NAs are removed for single-task challenge", {
  data <- rbind(
            data.frame(algo="A1", value=0.8, case="C1"),
            data.frame(algo="A2", value=0.6, case="C2"))

  expect_message(actualChallenge <- as.challenge(data, algorithm="algo", case="case", value="value", smallBetter=FALSE, na.treat="na.rm"),
                 "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$case), c("C1", "C2"))
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
                 "Performance of not all algorithms is observed for all cases in task T1. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.0, 0.0, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2", "C1", "C2"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A1", "A1", "A2", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.2, 0.3, 0.4, 0.0))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2", "C1", "C2"))
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
                 "Performance of not all algorithms is observed for all cases in task T1. Inserted as missings in following cases:")

  expect_equal(as.vector(actualChallenge$T1$algo), c("A1", "A2"))
  expect_equal(as.vector(actualChallenge$T1$value), c(0.8, 0.6))
  expect_equal(as.vector(actualChallenge$T1$case), c("C1", "C2"))

  expect_equal(as.vector(actualChallenge$T2$algo), c("A1", "A1", "A2"))
  expect_equal(as.vector(actualChallenge$T2$value), c(0.2, 0.3, 0.4))
  expect_equal(as.vector(actualChallenge$T2$case), c("C1", "C2", "C1"))
})