% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability.R
\name{stabilityByTask.bootstrap.list}
\alias{stabilityByTask.bootstrap.list}
\title{Creates blob plots stratified by task}
\usage{
\method{stabilityByTask}{bootstrap.list}(
  x,
  ordering,
  probs = c(0.025, 0.975),
  max_size = 3,
  size.ranks = 0.3 * theme_get()$text$size,
  shape = 4,
  showLabelForSingleTask = FALSE,
  ...
)
}
\arguments{
\item{x}{The bootstrapped, ranked assessment data set.}

\item{showLabelForSingleTask}{A boolean specifying whether the task name should be used as title for a single-task data set.}

\item{...}{Further arguments passed to or from other functions.}
}
\value{

}
\description{
Creates blob plots for each task from a bootstrapped, ranked assessment data set.
}
\examples{

}
\seealso{
\code{browseVignettes("challengeR")}

Other functions to visualize ranking stability: 
\code{\link{methodsplot.challenge}()},
\code{\link{significanceMap.ranked.list}()},
\code{\link{violin.bootstrap.list}()}

Other functions to visualize cross-task insights: 
\code{\link{dendrogram.ranked.list}()},
\code{\link{stability.ranked.list}()},
\code{\link{stabilityByAlgorithm.bootstrap.list}()}
}
\concept{functions to visualize cross-task insights}
\concept{functions to visualize ranking stability}
