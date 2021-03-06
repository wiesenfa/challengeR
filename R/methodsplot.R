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

#' @export
methodsplot <- function(x,...) UseMethod("methodsplot")

#' @export
methodsplot.default <- function(x, ...) stop("not implemented for this class")

#' Creates line plots
#'
#' Create line plots that visualize the robustness of ranking across different ranking methods from a challenge object.
#'
#' @param x The challenge object.
#' @param na.treat Indicates how missing perfomance values are treated if sanity check is enabled. It can be 'na.rm', numeric value or function.
#'   For a numeric value or function, NAs will be replaced by the specified values. For 'na.rm', rows that contain missing values will be removed.
#' @param methods A list of ranking methods that should be incorporated.
#' @param ordering
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#'
#' @seealso `browseVignettes("challengeR")`
#'
#' @family functions to visualize ranking stability
#' @export
methodsplot.challenge=function(x,
                               na.treat=NULL,
                               methods=list(testBased=.%>%test() %>% rank(ties.method = "min"),
                                            meanThenRank=  .%>%  aggregate(  FUN="mean") %>% rank(ties.method = "min"),
                                            medianThenRank=.%>% aggregate(  FUN="median") %>% rank(ties.method = "min"),
                                            rankThenMean= .%>%rank(ties.method = "min") %>%  aggregate(  FUN="mean") %>%rank(ties.method = "min"),
                                            rankThenMedian=.%>%rank(ties.method = "min") %>%  aggregate(  FUN="median") %>%rank(ties.method = "min")
                                            ),
                               ordering, ...) {

  if (any(sapply(x,
                  function(task) any(is.na(task[,attr(x, "value")]))))) { # only if missings present, else do nothing
    if (is.null(na.treat)) {
      warning("Please specify na.treat in as.challenge()")
      return(NULL)
    } else {
      xx = melt(x,
                id.vars=c(attr(x,"value"),
                          attr(x,"algorithm") ,
                          attr(x,"case"),
                          attr(x,"annotator"),
                          attr(x,"by")
      ))

      x=as.challenge(xx,
                     value=attr(x,"value"),
                     algorithm=attr(x,"algorithm") ,
                     case=attr(x,"case"),
                     by=attr(x,"by"),
                     annotator = attr(x,"annotator"),
                     smallBetter = attr(x,"smallBetter"),
                     na.treat=na.treat)
    }
  }

  a=lapply(methods,function(fun) fun(x))
  dat=melt(a,measure.vars="rank")
  colnames(dat)[4:5]=c("task","rankingMethod")

  if (missing(ordering)){
    lev=sort(unique(dat$algorithm))
    lab=lev
  } else {
    lev=ordering
    lab=lev
  }

  dat=dat%>%
    dplyr::rename(rank=.data$value)%>%
    mutate(rank=factor(.data$rank))%>%
    mutate(task=factor(.data$task))%>%
    mutate(algorithm=factor(.data$algorithm, levels=lev,labels = lab))

  linePlot <- ggplot(data = dat) +
    aes(x = rankingMethod, y = rank, color=algorithm, group=algorithm ) +
    geom_line(size=1)+
    xlab("Ranking method")  +
    ylab("Rank")+
    theme(
      strip.placement = "outside",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

  # Create multi-panel plot with task names as titles for multi-task data set
  if (length(x) > 1) {
    linePlot <- linePlot + facet_wrap(~ task)
   }

  return(linePlot)
}

# methodsplot.ranked.list does not exist, use methodpsplot.challenge instead since consonsus ranking needed for ordering (or alphabetical ordering instead)

#similar plot to methods plot, instead of across ranking methods across tasks
lineplot <- function(x,...) UseMethod("lineplot")
lineplot.default <- function(x, ...) stop("not implemented for this class")

lineplot.challenge=function(x,
                            ordering,...){
  if (inherits(x,"list"))  {
    dat=melt(x,measure.vars="rank")
    colnames(dat)[4]=c("task")

    if (missing(ordering)){
      lev=sort(unique(dat$algorithm))
      lab=lev
    } else {
      lev=ordering
      lab=paste(1:length(ordering),ordering)
    }

    dat=dat%>%
      dplyr::rename(rank=.data$value)%>%
      mutate(rank=factor(.data$rank))%>%
      mutate(task=factor(.data$task))%>%
      mutate(algorithm=factor(.data$algorithm, levels=lev,labels = lab))

    ggplot(data = dat) +
      aes(x = task, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )

  } else stop("Only applicable to multiple tasks")
}
