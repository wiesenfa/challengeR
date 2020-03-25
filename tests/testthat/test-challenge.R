test_that("missing algorithm performances are added as NA with sanity check enabled for single-task challenge", {
  data=rbind(
         data.frame(alg_name="A1", value=0.8, case="C1"),
         data.frame(alg_name="A2", value=0.6, case="C2")
       )

  expect_message(actualChallenge <- as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")

  expectedAlgNames <- c("A1", "A1", "A2", "A2")
  expectedValues <- c(0.8, NA, NA, 0.6)
  expectedCases <- c("C1", "C2", "C1", "C2")

  expect_equal(as.vector(actualChallenge$alg_name), expectedAlgNames)
  expect_equal(as.vector(actualChallenge$value), expectedValues)
  expect_equal(as.vector(actualChallenge$case), expectedCases)
})

test_that("missing algorithm performances are not added as NA with sanity check disabled for single-task challenge", {
  data=rbind(
         data.frame(alg_name="A1", value=0.8, case="C1"),
         data.frame(alg_name="A2", value=0.6, case="C2")
       )

  actualChallenge <- as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE, check=FALSE)

  expectedAlgNames <- c("A1", "A2")
  expectedValues <- c(0.8, 0.6)
  expectedCases <- c("C1", "C2")

  expect_equal(as.vector(actualChallenge$alg_name), expectedAlgNames)
  expect_equal(as.vector(actualChallenge$value), expectedValues)
  expect_equal(as.vector(actualChallenge$case), expectedCases)
})

test_that("missing algorithm performances are added as NA with sanity check enabled for multi-task challenge (1 task in data set)", {
  data=cbind(task="T1",
             rbind(
               data.frame(alg_name="A1", value=0.8, case="C1"),
               data.frame(alg_name="A2", value=0.6, case="C2")
             ))

  expect_message(actualChallenge <- as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms is observed for all cases in task T1. Inserted as missings in following cases:")

  expectedAlgNames <- c("A1", "A1", "A2", "A2")
  expectedValues <- c(0.8, NA, NA, 0.6)
  expectedCases <- c("C1", "C2", "C1", "C2")

  expect_equal(as.vector(actualChallenge$T1$alg_name), expectedAlgNames)
  expect_equal(as.vector(actualChallenge$T1$value), expectedValues)
  expect_equal(as.vector(actualChallenge$T1$case), expectedCases)
})

test_that("missing algorithm performances are not added as NA with sanity check disabled for multi-task challenge (1 task in data set)", {
  data=cbind(task="T1",
             rbind(
               data.frame(alg_name="A1", value=0.8, case="C1"),
               data.frame(alg_name="A2", value=0.6, case="C2")
             ))

  actualChallenge <- as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE, check=FALSE)

  expectedAlgNames <- c("A1", "A2")
  expectedValues <- c(0.8, 0.6)
  expectedCases <- c("C1", "C2")

  expect_equal(as.vector(actualChallenge$alg_name), expectedAlgNames)
  expect_equal(as.vector(actualChallenge$value), expectedValues)
  expect_equal(as.vector(actualChallenge$case), expectedCases)
})

test_that("missing algorithm performances are added as NA with sanity check enabled for multi-task challenge (2 tasks in data set)", {
  dataT1=cbind(task="T1",
               rbind(
                 data.frame(alg_name="A1", value=0.8, case="C1"),
                 data.frame(alg_name="A2", value=0.6, case="C2")
               ))
  dataT2=cbind(task="T2",
               rbind(
                 data.frame(alg_name="A1", value=0.2, case="C1"),
                 data.frame(alg_name="A1", value=0.3, case="C2"),
                 data.frame(alg_name="A2", value=0.4, case="C1")
               ))

  data=rbind(dataT1, dataT2)

  expect_message(actualChallenge <- as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms is observed for all cases in task T1. Inserted as missings in following cases:")

  expectedAlgNamesT1 <- c("A1", "A1", "A2", "A2")
  expectedValuesT1 <- c(0.8, NA, NA, 0.6)
  expectedCasesT1 <- c("C1", "C2", "C1", "C2")

  expect_equal(as.vector(actualChallenge$T1$alg_name), expectedAlgNamesT1)
  expect_equal(as.vector(actualChallenge$T1$value), expectedValuesT1)
  expect_equal(as.vector(actualChallenge$T1$case), expectedCasesT1)

  expectedAlgNamesT2 <- c("A1", "A1", "A2", "A2")
  expectedValuesT2 <- c(0.2, 0.3, 0.4, NA)
  expectedCasesT2 <- c("C1", "C2", "C1", "C2")

  expect_equal(as.vector(actualChallenge$T2$alg_name), expectedAlgNamesT2)
  expect_equal(as.vector(actualChallenge$T2$value), expectedValuesT2)
  expect_equal(as.vector(actualChallenge$T2$case), expectedCasesT2)
})

test_that("case cannot appear more than once per algorithm with sanity check enabled for single-task challenge", {
  data=rbind(
    data.frame(alg_name="A1", value=0.8, case="C1"),
    data.frame(alg_name="A1", value=0.8, case="C1")
  )

  expect_error(as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm with sanity check enabled for single-task challenge", {
  data=rbind(
    data.frame(alg_name="A1", value=0.8, case="C1"),
    data.frame(alg_name="A1", value=0.8, case="C1"),
    data.frame(alg_name="A2", value=0.7, case="C1"),
    data.frame(alg_name="A1", value=0.5, case="C2"),
    data.frame(alg_name="A2", value=0.6, case="C2"),
    data.frame(alg_name="A2", value=0.6, case="C2")
  )

  expect_error(as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm when missing data was added with sanity check enabled for single-task challenge", {
  data=rbind(
    data.frame(alg_name="A1", value=0.8, case="C1"),
    data.frame(alg_name="A1", value=0.8, case="C1"),
    data.frame(alg_name="A2", value=0.6, case="C2"),
    data.frame(alg_name="A2", value=0.6, case="C2")
  )

  expect_error(as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("case cannot appear more than once per algorithm with sanity check enabled for multi-task challenge (1 task in data set)", {
  data=cbind(task="T1",
             rbind(
               data.frame(alg_name="A1", value=0.8, case="C1"),
               data.frame(alg_name="A1", value=0.8, case="C1")
             ))

  expect_error(as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm in task T1", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm with sanity check enabled for multi-task challenge (1 task in data set)", {
  data=cbind(task="T1",
             rbind(
               data.frame(alg_name="A1", value=0.8, case="C1"),
               data.frame(alg_name="A1", value=0.8, case="C1"),
               data.frame(alg_name="A2", value=0.7, case="C1"),
               data.frame(alg_name="A1", value=0.5, case="C2"),
               data.frame(alg_name="A2", value=0.6, case="C2"),
               data.frame(alg_name="A2", value=0.6, case="C2")
             ))

  expect_error(as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm in task T1", fixed=TRUE)
})

test_that("cases cannot appear more than once per algorithm with sanity check enabled for multi-task challenge (2 tasks in data set)", {
  dataTask1=cbind(task="T1",
                  rbind(
                    data.frame(alg_name="A1", value=0.8, case="C1") # let T1 pass
                  ))

  dataTask2=cbind(task="T2",
                  rbind(
                    data.frame(alg_name="A1", value=0.8, case="C1"),
                    data.frame(alg_name="A1", value=0.8, case="C1"),
                    data.frame(alg_name="A2", value=0.7, case="C1"),
                    data.frame(alg_name="A1", value=0.5, case="C2"),
                    data.frame(alg_name="A2", value=0.6, case="C2"),
                    data.frame(alg_name="A2", value=0.6, case="C2")
                  ))

  data=rbind(dataTask1, dataTask2)

  expect_error(as.challenge(data, by="task", algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1, C2) appear(s) more than once for the same algorithm in task T2", fixed=TRUE)
})

test_that("multi-task data set containing one task is interpreted as single-task data set, missing algorithm performances are added", {
  data=cbind(task="T1",
             rbind(
               data.frame(alg_name="A1", value=0.8, case="C1"),
               data.frame(alg_name="A2", value=0.6, case="C2")
             ))

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_message(actualChallenge <- as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
                 "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")

  expectedAlgNames <- c("A1", "A1", "A2", "A2")
  expectedValues <- c(0.8, NA, NA, 0.6)
  expectedCases <- c("C1", "C2", "C1", "C2")

  expect_equal(as.vector(actualChallenge$alg_name), expectedAlgNames)
  expect_equal(as.vector(actualChallenge$value), expectedValues)
  expect_equal(as.vector(actualChallenge$case), expectedCases)
})

test_that("multi-task data set containing one task is interpreted as single-task data set, case cannot appear more than once per algorithm", {
  data=cbind(task="T1",
             rbind(
               data.frame(alg_name="A1", value=0.8, case="C1"),
               data.frame(alg_name="A1", value=0.8, case="C1")
             ))

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_error(as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("user is notified of duplicate cases when multi-task data set is interpreted as single-task data set (2 tasks in data set)", {
  dataTask1=cbind(task="T1",
                  rbind(
                    data.frame(alg_name="A1", value=0.8, case="C1")
                  ))

  dataTask2=cbind(task="T2",
                  rbind(
                    data.frame(alg_name="A1", value=0.8, case="C1")
                  ))

  data=rbind(dataTask1, dataTask2)

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_error(as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Case(s) (C1) appear(s) more than once for the same algorithm", fixed=TRUE)
})

test_that("user is notified of missing algorithm performance when multi-task data set is interpreted as single-task data set (2 tasks in data set)", {
  dataTask1=cbind(task="T1",
                  rbind(
                    data.frame(alg_name="A1", value=0.8, case="C1")
                  ))

  dataTask2=cbind(task="T2",
                  rbind(
                    data.frame(alg_name="A2", value=0.6, case="C2")
                  ))

  data=rbind(dataTask1, dataTask2)

  # do not specify parameter "by" to interpret multi-task data set as single-task data set
  expect_message(as.challenge(data, algorithm="alg_name", case="case", value="value", smallBetter=FALSE),
               "Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:", fixed=TRUE)
})
