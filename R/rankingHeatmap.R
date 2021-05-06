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
rankingHeatmap <- function(x,...) UseMethod("rankingHeatmap")

#' @export
rankingHeatmap.default <- function(x, ...) stop("not implemented for this class")

#' Creates ranking heatmaps
#'
#' Creates ranking heatmaps from one or more ranked assessment data sets.
#'
#' @param x The ranked asssessment data set.
#' @param ties.method A string specifying how ties are treated, see [base::rank()].
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#'
#' @seealso `browseVignettes("challengeR")`
#'
#' @family functions to visualize assessment data
#' @export
rankingHeatmap.ranked.list=function (x,ties.method="min",...) {

  xx=x$data

  a=lapply(names(x$matlist),function(subt){
    ordering=rownames(x$matlist[[subt]])[order(x$matlist[[subt]]$rank)]

    dd=as.challenge(xx[[subt]],
                    value=attr(xx,"value"),
                    algorithm=attr(xx,"algorithm") ,
                    case=attr(xx,"case"),
                    by=attr(xx, "by"),
                    annotator = attr(xx,"annotator"),
                    smallBetter = attr(xx,"smallBetter"),
                    na.treat=x$call[[1]][[1]]$na.treat)

    rankingHeatmap(dd,
                   ordering=ordering,
                   ties.method=ties.method,...) + ggtitle(subt)
  })

  # Remove title for single-task data set
  if (length(a) == 1) {
    a[[1]]$labels$title <- NULL
    return(a[[1]])
  } else {
    names(a) = names(x$matlist)
    class(a) <- "ggList"
    return(a)
  }

}

#' Creates a ranking heatmap
#'
#' Creates a ranking heatmap from a challenge object.
#'
#' @param x The challenge object.
#' @param ordering
#' @param ties.method A string specifying how ties are treated, see [base::rank()].
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#'
#' @seealso `browseVignettes("challengeR")`
#'
#' @family functions to visualize assessment data
#' @export
rankingHeatmap.challenge=function(x,
                                  ordering,
                                  ties.method="min",...) {
  ranking=x%>%rank( ties.method = ties.method )

  task <- ranking$matlist[[1]]

  dat=as.data.frame(table(task[[attr(x,"algorithm")]],
                          task$rank,
                          dnn=c("algorithm","rank")),
                    responseName = "Count")
  dat$algorithm=factor(dat$algorithm, levels=ordering)
  ncases=length(unique(task[[attr(x,"case")]]))
  ggplot(dat)+
    geom_raster(aes(algorithm, rank, fill= Count))+
    geom_hline(yintercept = seq(1.5,
                                max(max(task$rank)-.5,
                                    1.5),
                                by=1),
               color=grey(.8),size=.3)+
    geom_vline(xintercept = seq(1.5,
                                length(unique(dat$algorithm))-.5,
                                by=1),
               color=grey(.8),size=.3)+
    scale_fill_viridis_c(direction = -1,
                         limits=c(0,ncases)
    )+
    theme(axis.text.x = element_text(angle = 90),
          aspect.ratio=1)+
    xlab("Algorithm")+
    ylab("Rank")
}
