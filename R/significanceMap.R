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
significanceMap <- function(object,...) UseMethod("significanceMap")

#' @export
significanceMap.default <- function(object, ...) stop("not implemented for this class")

#' Creates significance maps
#'
#' Creates significance maps from a ranked assessment data set.
#'
#' @param object The ranked assessment data set.
#' @param alpha A numeric values specifying the significance level.
#' @param p.adjust.method A string specifying the adjustment method for multiple testing, see [stats::p.adjust()].
#' @param order
#' @param size.rank
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
significanceMap.ranked.list=function(object,
                                     alpha=0.05,p.adjust.method="holm",
                                     order=FALSE,
                                     size.rank=.3*theme_get()$text$size,...){

  a=object$data%>%decision.challenge(na.treat=object$call[[1]][[1]]$na.treat,
                                     alpha=alpha,
                                     p.adjust.method=p.adjust.method)

  aa=lapply(a, as.relation.challenge.incidence)
  names(aa)=names(object$data)

  relensemble= do.call(relation_ensemble,args = aa)

  res=list()
  for (task in names(object$data)){
    res[[task]]=significanceMap.data.frame(object=object$matlist[[task]],
                                           relation_object=relensemble[[task]],
                                           order=order,
                                           size.rank=size.rank,...
                                           ) + ggtitle(task)

  }

  # Remove title for single-task data set
  if (length(res) == 1) {
    res[[1]]$labels$title <- NULL
    return(res[[1]])
  } else {
    names(res) = names(object$matlist)
    class(res) <- "ggList"
    return(res)
  }
 }


significanceMap.data.frame=function(object,
                                    relation_object,
                                    order=FALSE,
                                    size.rank=.3*theme_get()$text$size,...){

  object$algorithm=rownames(object)
  inc=relation_incidence(relation_object)

  if (order){
    scores=apply(inc,1,
                 function(x) sum(x==0)-1)
    scores2=apply(inc,2,
                  function(x) sum(x==1))[names(scores)]#+1-nrow(inc))
    scores=data.frame(algorithm=names(scores),
                      score=scores,
                      score2=scores2,
                      stringsAsFactors =F)
    scores=right_join(scores,
                      object,
                      by="algorithm")

    ordering= (scores[order(scores$score,
                            scores$score2,
                            scores$rank),"algorithm"])
    scores=scores[,1:3]
  } else ordering=  names(sort(t(object[,"rank",drop=F])["rank",]))

  inc=inc[ordering,]

  incidence.mat=melt(inc)
  colnames(incidence.mat)=c("algorithm","notsigPair",     "decision")
  incidence.mat$algorithm=as.character(incidence.mat$algorithm)
  incidence.mat$notsigPair=as.character(incidence.mat$notsigPair)
  incidence.mat=right_join(incidence.mat,
                           object,
                           by="algorithm")
  if (order) incidence.mat=right_join(incidence.mat,
                                      scores,
                                      by="algorithm")

  incidence.mat=incidence.mat%>%mutate(algorithm=factor(.data$algorithm,
                                                        levels=ordering),
                                       notsigPair=factor(.data$notsigPair,
                                                         levels=ordering))

  incidence.mat$decision=as.factor(incidence.mat$decision)

  p=ggplot(incidence.mat) +
    geom_raster(aes(algorithm,
                    notsigPair,
                    fill=decision),...)+
    geom_raster(aes(algorithm,algorithm),
                fill="white")+
    geom_abline(slope=1) +
    coord_cartesian(clip = 'off')+
    theme(aspect.ratio=1,
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          plot.margin=unit(c(1,1,1,1), "lines"),
          legend.position="none")+
    ylab("Algorithm")+
    xlab("Algorithm")+
    scale_fill_manual(values=cividis(2,begin=0,end=1,alpha=.7))

  fixy=0
  th_get=theme_get()
  # grid on top
    lt=th_get$panel.grid$linetype
    if (is.null(lt)) lt=th_get$line$linetype
    gridSize=c(th_get$panel.grid.major$size,th_get$panel.grid$size,th_get$line$size)[1]


  #p=p+theme(panel.background = element_rect(fill = NA),panel.ontop=TRUE) #-> grid will be on top of diagonal
  #fix:
    f=ggplot_build(p)
    p= p + geom_vline(xintercept=f$layout$panel_params[[1]]$x$breaks,
                      linetype=lt,
                      color=th_get$panel.grid$colour,
                      size=gridSize)+
      geom_hline(yintercept=f$layout$panel_params[[1]]$y$breaks,
                 linetype=lt,
                 color=th_get$panel.grid$colour,
                 size=gridSize)+
      geom_abline(slope=1)+
      geom_text(aes(x=algorithm,y=fixy,label=rank),
                nudge_y=.5,
                vjust = 0,
                size=size.rank,
                fontface="plain",family="sans"
      )


  if (order) p=  p+
      geom_text(aes(x=algorithm,y=fixy,label=score),
                nudge_y=0,
                vjust = 0, size=size.rank,
                fontface="plain",family="sans") +
      annotate("text",
               x=0,y=fixy+.5,
               vjust = 0,
               size=size.rank,
               fontface="plain",
               family="sans",
               label="original")+
      annotate("text",x=0,y=fixy,
               vjust = 0,
               size=size.rank,
               fontface="plain",family="sans",label="new")

  return(p)

}
