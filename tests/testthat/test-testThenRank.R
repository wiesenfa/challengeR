test_that("test-then-rank raises warning for one case", {
  data <- rbind(
    data.frame(algo="A1", value=0.6, case="C1"),
    data.frame(algo="A2", value=0.8, case="C1"))

  challenge <- as.challenge(data, taskName="T1", algorithm="algo", case="case", value="value", smallBetter = TRUE)

  expect_warning(ranking <- challenge%>%testThenRank(),
                 "Only one case in task.", fixed = TRUE)

  expectedRanking <- rbind(
    "A1" = data.frame(prop_significance = 0, rank = 1),
    "A2" = data.frame(prop_significance = 0, rank = 1))

  expect_equal(ranking$matlist$T1, expectedRanking)
})
