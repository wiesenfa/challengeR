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

#' Creates dot- and boxplots
#'
#' Creates dot- and boxplots visualizing the assessment data separately for each algorithm.
#' Boxplots representing descriptive statistics for all test cases (median, quartiles and outliers)
#' are combined with horizontally jittered dots representing individual test cases.
#'
#' @param x The ranked assessment data set.
#' @param color A string specifying the color of the dots.
#' @param jitter.width A numeric value specifying the jitter width of the dots.
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
boxplot.ranked.list=function(x,
                             jitter.width=0.25,...){
  algo=attr(x$data,"algorithm")
  value=attr(x$data,"value")
  ranking=x
  x=x$data

  for (i in names(x)) {
    x[[i]][[algo]]=factor(x[[i]][[algo]],
                          levels=rownames(ranking$matlist[[i]][order(ranking$matlist[[i]]$rank),]))
  }

  a=lapply(1:length(x),function(id){
    ggplot(data=x[[id]])+
      geom_jitter(aes_string(algo,value,color=algo),
                  position=position_jitter(width=jitter.width, height=0),
                  ...)+
      geom_boxplot(aes_string(algo,value),
                   outlier.shape = NA,fill=NA)+
      ggtitle(names(x)[id]) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0),
            legend.position="none") +
      xlab("Algorithm") +
      ylab("Metric value") 
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

boxplot.comparedRanks.list=function(x,...){
  tau=sapply(x,function(z) z$tau)
  boxplot(tau,ylim=c(0,1.0),las=2, outline=FALSE,
          ylab="Kendall's tau",...)
  stripchart(tau,
             vertical = TRUE, method = "jitter",
             pch = 21, col = "blue", add=TRUE,...)

}

boxplot.bootstrap.list=function(x,...){
  winner.noboot=winner.ranked.list(x)
  x2=winnerFrequencies(x)
  n.bootstraps= ncol(x$bootstrappedRanks[[1]])
  perc_boot_Winner=lapply(1:length(x2),function(i){
    x2.i=x2[[i]]
    winner.id=which(rownames(x2.i)%in%rownames(winner.noboot[[i]])) #could be multiple winners!!!!
    100*x2.i[winner.id,3,drop=F]/n.bootstraps
  })

  boxplot(unlist(perc_boot_Winner),ylim=c(0,100),las=2, outline=FALSE,
          ylab="% Bootstraps",xlab="Winner ranks 1",
          sub=paste(n.bootstraps,"Bootstraps"),...)
  stripchart(unlist(perc_boot_Winner),
             vertical = TRUE, method = "jitter",
             pch = 21, col = "blue", add=TRUE,...)
}
