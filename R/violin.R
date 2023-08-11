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
violin <- function(x,...) UseMethod("violin")

#' @export
violin.default <- function(x, ...) stop("not implemented for this class")

#' Creates a violin plot
#'
#' Creates a violin plot from a bootstrapped, ranked assessment data set.
#'
#' @param x The bootstrapped, ranked assessment data set.
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
violin.bootstrap.list=function(x,...){
  ken=melt(kendall.bootstrap.list(x))
  colnames(ken)[2]="Task"
  cat("\n\nSummary Kendall's tau:\n")
  ss=ken%>%group_by(Task)%>%
    summarise(mean=mean(value,na.rm=T),
              median=median(value,na.rm=T),
              q25=quantile(value,probs = .25,na.rm=T),
              q75=quantile(value,probs = .75,na.rm=T))%>%
    arrange(desc(median))

  print(knitr::kable(as.data.frame(ss)))

  # drop task if no kendall could be computed
    noResults <- sapply(split(ss,ss$Task),
                        function(x) all(is.na(x[,-1])))
    if (any(noResults)) {
      cat("\nNo Kendall's tau could be calculated for any bootstrap sample in task ",
              names(noResults)[noResults],
              " because of missing variability. Task dropped from figure.",fill=F)
      ken <- ken %>% filter(Task %in% names(noResults)[!noResults])

    }

  xAxisText <- element_blank()

  # Show task names as tick mark labels only for multi-task data set
  if (length(x$data) > 1) {
    xAxisText <- element_text(angle = 90, vjust = 0.5, hjust = 1)
  }

  ken%>%mutate(Task=factor(.data$Task,
                           levels=ss$Task))%>%
    ggplot(aes(Task,value))+
    geom_violin(alpha=.3,
                color=NA,
                na.rm=TRUE,
                fill="blue")+
    geom_boxplot(width=0.1,
                 na.rm=TRUE,
                 fill="white")+
    theme(axis.text.x = xAxisText,
          legend.position = "none")+
    ylab("Kendall's tau")+
    scale_y_continuous(limits=c(min(min(ken$value),0),
                                max(max(ken$value),1)))
}

kendall.bootstrap.list=function(x){
  ken=lapply(1:length(x$bootstrappedRanks),function(Task){
    id=match(rownames( x$bootstrappedRanks[[Task]]),
             rownames(x$matlist[[Task]]) )
    sapply(x$bootstrappedRanks[[Task]],
           function(bootSample) suppressWarnings(kendall(bootSample,
                                                         x$matlist[[Task]]$rank[id])))
  } )
  names(ken)=names((x$bootstrappedRanks))

  if (sum(is.na(x))>0){
   cat("Bootstrap samples without variability in rankings (all algorithms ranked 1) excluded.\n Frequency of such samples by task:\n",fill = T)
    sapply(ken,function(x) sum(is.na(x)))
  }


  return(ken)

}

density.bootstrap.list=function(x,...){
  ken=melt(kendall.bootstrap.list(x))
  colnames(ken)[2]="Task"

  cat("\n\nSummary Kendall's tau\n")
  ss=ken%>%group_by(Task)%>%
    summarise(mean=mean(value,na.rm=T),
              median=median(value,na.rm=T),
              q25=quantile(value,probs = .25,na.rm=T),
              q75=quantile(value,probs = .75,na.rm=T))%>%
    arrange(desc(median))

  print(as.data.frame(ss))

  ggplot(ken)+
    geom_density(aes(value,fill=Task),alpha=.3,color=NA)
}
