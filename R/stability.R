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
stability <- function(x,...) UseMethod("stability")

#' @export
stability.default <- function(x, ...) stop("not implemented for this class")

#' @export
stabilityByAlgorithm <- function(x,...) UseMethod("stabilityByAlgorithm")

#' @export
stabilityByAlgorithm.default <- function(x, ...) stop("not implemented for this class")

#' @export
stabilityByTask <- function(x,...) UseMethod("stabilityByTask")

#' @export
stabilityByTask.default <- function(x, ...) stop("not implemented for this class")

#' Creates a blob plot across tasks
#'
#' Creates a blob plots visualizing the ranking variability across tasks.
#'
#' @param x The ranked asssessment data set.
#' @param ordering
#' @param probs
#' @param max_size
#' @param freq
#' @param shape
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#'
#' @seealso `browseVignettes("challengeR")`
#'
#' @family functions to visualize cross-task insights
#' @export
stability.ranked.list=function(x,
                               ordering,
                               probs=c(.025,.975),
                               max_size=6,
                               freq=FALSE,
                               shape=4,...) {
  if (length(x$data) < 2) {
    stop("The stability of rankings across tasks cannot be computed for less than two tasks.")
  }

  dd=melt(x,
          measure.vars="rank",
          value.name="rank") %>% dplyr::rename(task="L1")

  if (!missing(ordering)) {
    if (is.numeric(ordering) & !is.null(names(ordering)) ){
      ordering <- names(ordering)[order(ordering)]
    } else if (!is.character(ordering)){
      stop("Argument ordering has to be a named vector of ranks or a vector of algorithm names in the ranking order.")
    }
    dd=dd%>%mutate(algorithm=factor(.data$algorithm,
                                    levels=ordering))
  } else dd=dd%>%mutate(algorithm=factor(.data$algorithm))

  if (!freq) {
    p = ggplot(dd)+
          geom_count(aes(algorithm,
                         rank,
                         color=algorithm,
                         size = stat(prop*100)))
  } else {
    p=ggplot(dd)+
        geom_count(aes(algorithm,
                       rank,
                       color=algorithm ))
  }

  p+scale_size_area(max_size = max_size)+
    stat_summary(aes(algorithm, rank),
                 geom="point",
                 shape=shape,
                 fun.data=function(x) data.frame(y=median(x)),...)+
    stat_summary(aes(algorithm, rank),
                 geom="linerange",
                 fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                 ymax=quantile(x,probs[2])))+
    geom_abline(slope=1,
                color="gray",
                linetype="dotted")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    guides(size = guide_legend(title="%"))+
    scale_y_continuous(minor_breaks=NULL,
                       limits=c(1,max(5,max(dd$rank))),
                       breaks=c(1,seq(5,max(5,max(dd$rank)),by=5)))+
    xlab("Algorithm")+
    ylab("Rank")

}


rankdist.bootstrap.list=function(x,...){
  rankDist=melt(lapply(x$bootsrappedRanks,t),
                value.name="rank") %>% dplyr::rename(algorithm="Var2",task="L1")
  rankDist
}

#' Creates blob plots or stacked frequency plots stratified by algorithm
#'
#' Creates blob plots (\code{stacked = FALSE}) or stacked frequency plots (\code{stacked = TRUE}) for each algorithm
#' from a bootstrapped, ranked assessment data set.
#'
#' @param x The bootstrapped, ranked assessment data set.
#' @param ordering
#' @param stacked A boolean specifying whether a stacked frequency plot (\code{stacked = TRUE}) or blob plot (\code{stacked = FALSE}) should be created.
#' @param probs
#' @param max_size
#' @param shape
#' @param freq
#' @param single
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#'
#' @seealso `browseVignettes("challengeR")`
#'
#' @family functions to visualize cross-task insights
#' @export
stabilityByAlgorithm.bootstrap.list=function(x,
                                             ordering,
                                             stacked = FALSE,
                                             probs=c(.025,.975),#only for !stacked
                                             max_size=3,#only for !stacked
                                             shape=4,#only for !stacked
                                             freq=FALSE, #only for stacked
                                             single=FALSE,...) {

  if (length(x$data) < 2) {
    stop("The stability of rankings by algorithm cannot be computed for less than two tasks.")
  }

  rankDist=rankdist.bootstrap.list(x)

  if (!missing(ordering)) {
    if (is.numeric(ordering) & !is.null(names(ordering)) ){
      ordering <- names(ordering)[order(ordering)]
    } else if (!is.character(ordering)){
      stop("Argument ordering has to be a named vector of ranks or a vector of algorithm names in the ranking order.")
    }
    
    rankDist=rankDist%>%mutate(algorithm=factor(.data$algorithm,
                                                levels=ordering))
  }
  
  if (!stacked){
    if (single==FALSE){
      pl <- ggplot(rankDist)+
        geom_count(aes(task ,
                       rank,
                       color=algorithm,
                       size = stat(prop*100),
                       group = task ))+
        scale_size_area(max_size = max_size)+
        stat_summary(aes(task ,rank ),
                     geom="point",
                     shape=shape,
                     fun.data=function(x) data.frame(y=median(x)),...)+
        stat_summary(aes(task ,rank ),
                     geom="linerange",
                     fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                     ymax=quantile(x,probs[2])))+
        facet_wrap(vars(algorithm))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
        guides(size = guide_legend(title="%"))+
        scale_y_continuous(minor_breaks=NULL,
                           limits=c(1,max(5,max(rankDist$rank))),
                           breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))+
        xlab("Task")+
        ylab("Rank")

    } else {
      pl=list()
      for (alg in ordering){
        rankDist.alg=subset(rankDist,
                            rankDist$algorithm==alg)
        pl[[alg]]=ggplot(rankDist.alg)+
          geom_count(aes(task ,
                         rank,
                         color=algorithm,
                         size = stat(prop*100),
                         group = task ))+
          scale_size_area(max_size = max_size)+
          stat_summary(aes(task ,
                           rank ),
                       geom="point",
                       shape=shape,
                       fun.data=function(x) data.frame(y=median(x)),...)+
          stat_summary(aes(task ,rank ),
                       geom="linerange",
                       fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                       ymax=quantile(x,probs[2])))+
          facet_wrap(vars(algorithm))+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
          guides(size = guide_legend(title="%"))+
          scale_y_continuous(minor_breaks=NULL,
                             limits=c(1,max(5,max(rankDist$rank))),
                             breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))+
          xlab("Task")+
          ylab("Rank")
      }
      names(pl) = ordering
      class(pl) <- "ggList"
    }

  } else { #stacked
    rankDist=rankDist%>%
      group_by(task)%>%
      dplyr::count(.data$algorithm,
                   .data$rank)%>%
      group_by(.data$algorithm)%>%
      mutate(prop=.data$n/sum(.data$n)*100)%>%
      ungroup%>%
      data.frame%>%
      mutate(rank=as.factor(.data$rank))

    results= melt.ranked.list(x,
                              measure.vars="rank",
                              value.name="rank") %>%
      dplyr::select(-.data$variable)
    colnames(results)[3]="task"
    if (!missing(ordering)) {
      if (is.numeric(ordering) & !is.null(names(ordering)) ){
        ordering <- names(ordering)[order(ordering)]
      } else if (!is.character(ordering)){
        stop("Argument ordering has to be a named vector of ranks or a vector of algorithm names in the ranking order.")
      }
      
      results=results%>%mutate(algorithm=factor(.data$algorithm,
                                                levels=ordering))
    }
    
    if (single==FALSE){
      pl<- ggplot(rankDist) +
        facet_wrap(vars(algorithm))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

      if (freq){
        pl <- pl +   geom_bar(aes(rank,
                                  n,
                                  fill=task ),
                              position = "stack",
                              stat = "identity") +
          ylab("Frequency")
      } else {
        pl <- pl +   geom_bar(aes(rank,
                                  prop,
                                  fill=task ),
                              position = "stack",
                              stat = "identity")+
          ylab("Proportion (%)")
      }

     pl <-  pl +
        geom_vline(aes(xintercept=rank,
                       color=task),
                   size=.4,
                   linetype="dotted",
                   data=results) +
        xlab("Rank")
    } else {
      pl=list()
      for (alg in ordering){
        rankDist.alg=subset(rankDist,
                            rankDist$algorithm==alg)
        results.alg=subset(results,
                           results$algorithm==alg)
        pl[[alg]]=ggplot(rankDist.alg)+
          facet_wrap(vars(algorithm))+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

        if (freq){
          pl[[alg]] <- pl[[alg]] +   geom_bar(aes(rank,
                                                  n,
                                                  fill=task ),
                                              position = "stack",
                                              stat = "identity") +
            ylab("Frequency")
        } else {
          pl[[alg]] <- pl[[alg]] +   geom_bar(aes(rank,
                                                  prop,
                                                  fill=task ),
                                              position = "stack",
                                              stat = "identity")+
            ylab("Proportion (%)")
        }

        pl[[alg]] <- pl[[alg]] +
          geom_vline(aes(xintercept=rank,
                         color=task),
                     size=.4,
                     linetype="dotted",
                     data=results.alg) +
          xlab("Rank")
      }
      names(pl) = ordering
      class(pl) <- "ggList"
    }
  }
  pl
}

#' Creates blob plots stratified by task
#'
#' Creates blob plots for each task from a bootstrapped, ranked assessment data set.
#'
#' @param x The bootstrapped, ranked assessment data set.
#' @param ordering
#' @param probs
#' @param max_size
#' @param size.ranks
#' @param shape
#' @param showLabelForSingleTask A boolean specifying whether the task name should be used as title for a single-task data set.
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#'
#' @seealso `browseVignettes("challengeR")`
#'
#' @family functions to visualize ranking stability
#' @family functions to visualize cross-task insights
#' @export
stabilityByTask.bootstrap.list=function(x,
                                        ordering,
                                        probs=c(.025,.975),
                                        max_size=3,
                                        size.ranks=.3*theme_get()$text$size,
                                        shape=4,
                                        showLabelForSingleTask=FALSE,...){
  rankDist=rankdist.bootstrap.list(x)
  ranks=melt.ranked.list(x,
                         measure.vars="rank",
                         value.name = "full.rank")
  colnames(ranks)[4]="task"
  if (!missing(ordering)) {
    if (is.numeric(ordering) & !is.null(names(ordering)) ){
      ordering <- names(ordering)[order(ordering)]
    } else if (!is.character(ordering)){
      stop("Argument ordering has to be a named vector of ranks or a vector of algorithm names in the ranking order.")
    }
    
    ranks$algorithm=factor(ranks$algorithm,
                           levels=ordering)
    rankDist=rankDist%>%mutate(algorithm=factor(.data$algorithm,
                                                levels=ordering))
  }

  blobPlot <- ggplot(rankDist)+
    geom_count(aes(algorithm ,
                   rank,
                   color=algorithm,
                   size = stat(prop*100),
                   group = algorithm ))+
    scale_size_area(max_size = max_size)+
    geom_abline(slope=1,
                color="gray",
                linetype="dotted")+
    stat_summary(aes(algorithm ,rank ),
                 geom="point",
                 shape=shape,
                 fun.data=function(x) data.frame(y=median(x)),...)+
    stat_summary(aes(algorithm ,rank ),
                 geom="linerange",
                 fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                 ymax=quantile(x,probs[2])))+
    geom_text(aes(x=algorithm,y=1,label=full.rank),
              nudge_y=-.6,
              vjust = 0,
              size=size.ranks,
              fontface="plain",
              family="sans",
              data=ranks) +
    coord_cartesian(clip = 'off')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    guides(size = guide_legend(title="%"))+
    scale_y_continuous(minor_breaks=NULL,
                       limits=c(.4,max(5,max(rankDist$rank))),
                       breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))+
    xlab("Algorithm")+
    ylab("Rank")

  # Create multi-panel plot with task names as labels for multi-task data set or single-task data set when explicitly specified
  if (length(x$data) > 1 || showLabelForSingleTask == TRUE) {
    blobPlot <- blobPlot + facet_wrap(vars(task))
  }

  return(blobPlot)
}
