#' Performs ranking via aggregate-then-rank
#'
#' Performs ranking by first aggregating performance values across all cases (e.g., with the mean, median or another quantile) for each algorithm.
#' This aggregate is then used to compute a rank for each algorithm.
#'
#' @param object The challenge object.
#' @param FUN The aggregation function, e.g. mean, median, min, max, function(x), quantile(x, probs=0.05).
#' @param ties.method A string specifying how ties are treated, see \code{\link{base::rank}}.
#'
#' @return An S3 object of class "ranked.list" to represent a ranked assessment data set.
#'
#' @examples
#'
#' \dontrun{
#'  aggregateThenRank(challenge, FUN = mean, ties.method="average", na.treat = 0)
#' }
#'
#' @family ranking functions
#' @export
aggregateThenRank=function(object,FUN,ties.method = "min",...){
  object %>%
    aggregate(FUN=FUN,...) %>%
    rank(ties.method = ties.method)
}

testThenRank=function(object, ties.method = "min",...){
  object %>%
    aggregate(FUN="significance",...) %>%
    rank(ties.method = ties.method)
}

rankThenAggregate=function(object,
                           FUN,
                           ties.method = "min"
                           ){
  object %>%
        rank(ties.method = ties.method)%>%
          aggregate(FUN=FUN) %>%
          rank(ties.method = ties.method) # small rank is always best, i.e. smallBetter always TRUE
}
