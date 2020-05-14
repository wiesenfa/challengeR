test_that("report for single-task data set without bootstrapping is created", {
  data <- rbind(
    data.frame(algo="A1", value=0.8, case="C1"),
    data.frame(algo="A2", value=0.6, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter=FALSE)

  ranking <- challenge%>%aggregateThenRank(FUN=median, ties.method="min")


  # Create PDF report
  ranking %>%
    report(title="Test Challenge",
           file = "testthat_single_task_no_bootstrapping",
           format = "PDF",
           latex_engine="pdflatex",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_no_bootstrapping.pdf"))

  # Clean up
  if (file.exists("testthat_single_task_no_bootstrapping.pdf")) {
    file.remove("testthat_single_task_no_bootstrapping.pdf")
  }


  # Create HTML report
  ranking %>%
    report(title="Test Challenge",
           file = "testthat_single_task_no_bootstrapping",
           format = "HTML",
           latex_engine="pdflatex",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_no_bootstrapping.html"))

  # Clean up
  if (file.exists("testthat_single_task_no_bootstrapping.html")) {
    file.remove("testthat_single_task_no_bootstrapping.html")
  }


  # Create Word report
  ranking %>%
    report(title="Test Challenge",
           file = "testthat_single_task_no_bootstrapping",
           format = "Word",
           latex_engine="pdflatex",
           clean=TRUE,
           open=FALSE)

  expect_true(file.exists("testthat_single_task_no_bootstrapping.docx"))

  # Clean up
  if (file.exists("testthat_single_task_no_bootstrapping.docx")) {
    file.remove("testthat_single_task_no_bootstrapping.docx")
  }

})
