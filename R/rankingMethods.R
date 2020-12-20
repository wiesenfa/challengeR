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

#' Performs ranking via aggregate-then-rank
#'
#' Performs ranking by first aggregating performance values across all cases (e.g., with the mean, median or another quantile) for each algorithm.
#' This aggregate is then used to compute a rank for each algorithm.
#'
#' @param object The challenge object.
#' @param FUN The aggregation function, e.g. mean, median, min, max, function(x), quantile(x, probs=0.05).
#' @param ties.method A string specifying how ties are treated, see [base::rank()].
#' @param ... Further arguments passed to or from other functions.
#'
#' @return An S3 object of class "ranked.list" to represent a ranked assessment data set.
#'
#' @examples
#'
#' \dontrun{
#'  aggregateThenRank(challenge, FUN = mean, ties.method = "average", na.treat = 0)
#' }
#'
#' @family ranking functions
#' @export
aggregateThenRank=function(object,FUN,ties.method = "min",...){
  object %>%
    aggregate(FUN=FUN,...) %>%
    rank(ties.method = ties.method)
}

#' Performs ranking via test-then-rank
#'
#' Computes statistical hypothesis tests based on Wilcoxon signed rank test for each possible
#' pair of algorithms to assess differences in metric values between the algorithms.
#' Then ranking is performed according to the number of significant one-sided test results.
#' If algorithms have the same number of significant test results, then they obtain the same rank.
#'
#' @param object The challenge object.
#' @param ties.method A string specifying how ties are treated, see [base::rank()].
#' @param ... Further arguments passed to or from other functions.
#'
#' @return An S3 object of class "ranked.list" to represent a ranked assessment data set.
#'
#' @examples
#' \dontrun{
#'  testThenRank(challenge,
#'               alpha=0.05, # significance level
#'               p.adjust.method="none", # method for adjustment for multiple testing, see ?p.adjust
#'               na.treat = 0)
#' }
#'
#' @family ranking functions
#' @export
testThenRank=function(object, ties.method = "min",...){
  object %>%
    aggregate(FUN="significance",...) %>%
    rank(ties.method = ties.method)
}

#' Performs ranking via rank-then-aggregate
#'
#' Performs ranking by first computing a rank for each case for each algorithm ("rank first").
#' The final rank is based on the aggregated ranks for the cases. This ranking method handles missing values implicitly
#' by assigning the worst rank to missing algorithm performances.
#'
#'
#' @param object The challenge object.
#' @param FUN The aggregation function, e.g., mean, median, min, max, function(x), quantile(x, probs=0.05).
#' @param ties.method A string specifying how ties are treated, see [base::rank()].
#'
#' @return An S3 object of class "ranked.list" to represent a ranked assessment data set.
#'
#' @examples
#' \dontrun{
#'  rankThenAggregate(challenge, FUN = mean)
#' }
#'
#' @family ranking functions
#' @export
rankThenAggregate=function(object,
                           FUN,
                           ties.method = "min"
                           ){
  object %>%
        rank(ties.method = ties.method)%>%
          aggregate(FUN=FUN) %>%
          rank(ties.method = ties.method) # small rank is always best, i.e. smallBetter always TRUE
}
