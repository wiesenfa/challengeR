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
podium <- function(object,...) UseMethod("podium")

#' @export
podium.default <- function(object, ...) stop("not implemented for this class")

#' Creates podium plots
#'
#' Creates podium plots from one or more ranked assessment data sets.
#'
#' @param object The ranked asssessment data set.
#' @param xlab A string specifying the x-axis label.
#' @param ylab A string specifying the y-axis label.
#' @param lines.show
#' @param lines.alpha
#' @param lines.lwd
#' @param lines.lty
#' @param lines.col
#' @param col
#' @param dots.pch
#' @param dots.cex
#' @param places.lty
#' @param places.col
#' @param legendfn
#' @param layout.heights
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
podium.ranked.list=function(object,
                            xlab = "Podium",
                            ylab = "Performance",
                            lines.show = TRUE,
                            lines.alpha = 0.2,
                            lines.lwd = 1,
                            lines.lty=1,
                            lines.col = col,
                            col,
                            dots.pch = 19,
                            dots.cex = 1,
                            places.lty = 2,
                            places.col = 1,
                            legendfn = function(algs, cols) {
                              legend("topright", algs, lwd = 1, col = cols, bg = "white")
                            },
                            layout.heights=c(1,0.4),
                            ...){
    x=object$data

    podiumPlots <- length(names(x))

    for (subt in names(x)) {
      ordering=t(object$matlist[[subt]][,"rank",drop=F])["rank",]
      if (missing(col)) col=default_colors(length(ordering),
                                           algorithms = names(ordering))

      dd=as.challenge(x[[subt]],
                      value=attr(x,"value"),
                      algorithm=attr(x,"algorithm"),
                      case=attr(x,"case"),
                      by=attr(x, "by"),
                      annotator = attr(x,"annotator"),
                      smallBetter = attr(x,"smallBetter"),
                      na.treat=object$call[[1]][[1]]$na.treat)

      podiumPlot <- podium(dd,
             ordering=ordering,
             xlab = xlab, ylab = ylab,
             lines.show = lines.show,
             lines.alpha = lines.alpha,
             lines.lwd = lines.lwd,
             lines.lty = lines.lty,
             col=col,
             lines.col = lines.col,
             dots.pch = dots.pch,
             dots.cex = dots.cex,
             places.lty = places.lty,
             places.col = places.col,
             legendfn = legendfn,
             layout.heights=layout.heights,
             ...)

      if (length(names(x)) > 1) {
        title(subt,outer=T,line=-3)
      }

      append(podiumPlots, podiumPlot)
    }
}

#' Creates a podium plot
#'
#' Creates a podium plot from a challenge object.
#'
#' @param object The challenge object.
#' @param ordering
#' @param xlab A string specifying the x-axis label.
#' @param ylab A string specifying the y-axis label.
#' @param lines.show
#' @param lines.alpha
#' @param lines.lwd
#' @param lines.lty
#' @param lines.col
#' @param col
#' @param dots.pch
#' @param dots.cex
#' @param places.lty
#' @param places.col
#' @param legendfn
#' @param layout.heights
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
podium.challenge=function(object,
                          ordering,
                          xlab = NULL, ylab = NULL,
                          lines.show = FALSE, lines.alpha = 0.2,
                          lines.lwd = 1,
                          lines.lty = 1,
                          col,lines.col = col,
                          dots.pch = 19, dots.cex = 1,
                          places.lty = 2, places.col = 1,
                          legendfn = function(algs, cols) {
                            legend("topleft", algs, lwd = 1, col = cols, bg = "white")
                          },
                          layout.heights=c(1,0.4),
                          ...) {

  ranking=object%>%rank( ties.method = "random" )

  task <- ranking$matlist[[1]]

  dat=as.data.frame(table(task[[attr(object, "algorithm")]],
                          task$rank,
                          dnn=c("algorithm","rank")),
                    responseName = "Count")

  form=as.formula(paste(attr(object,"case"), attr(object,"algorithm"), sep="~"))
  ranks=acast(task, form, value.var="rank")
  values=acast(task, form, value.var=attr(object, "value"))
  nranks=acast(dat, algorithm~rank, value.var="Count")

  nalgs <- ncol(ranks)
  algs <- colnames(ranks)

  barorder <- order(ordering)
  orderedAlgorithms= names(ordering)[barorder]

  ylim=range(task[[attr(object,"value")]], na.rm = TRUE)

  dotplotborders <- (0:nalgs) * nalgs
  dotplaces <- (1:nalgs) - 0.5
  names(dotplaces) <- orderedAlgorithms

  linecols <- sapply(lines.col, function(c) {
    r <- col2rgb(c)
    rgb(r[1], r[2], r[3],
        alpha = round(255 * lines.alpha),
        maxColorValue = 255)
  })

  opar <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2), nrow = 2, byrow = TRUE),
         heights =layout.heights)

  mar <- par("mar")
  par(mar = c(0, mar[2], mar[3], mar[4]))

  plot(dotplotborders, rep(ylim[2], nalgs + 1),
       type = "n",
       ylim = ylim, ylab = ylab, xlab = "",
       axes = F)
  axis(1, at = dotplotborders,
       labels = NA, lwd = par("lwd"))
  axis(2, lwd = par("lwd"))
  box()
  abline(v = dotplotborders, lty = places.lty, col = places.col)
  linesegments <- function(x, y, ...) {
    n <- length(x)
    segments(x[-n], y[-n], x[-1], y[-1], ...)
  }
  drawthe <- function(fn, col, ...) {
    for (i in 1:nrow(values)) {
      r <- ranks[i, ]
      o <- order(r)
      performances <- (values[i, ])[o]
      places <- (dotplaces[names(r)] + ((r - 1) * nalgs))[o]
      fn(places, performances, 
         col = col[names(r)[o]], 
         pch = switch((length(dots.pch)!=1)+1,
                      dots.pch,
                      dots.pch[names(r)[o]]), 
         lty = switch((length(lines.lty)!=1)+1,
                      lines.lty,
                      lines.lty[names(r)[o]]),
         ...)
    }
  }
  if (lines.show) drawthe(linesegments, linecols, lwd = lines.lwd)

  drawthe(points, col,  cex = dots.cex)


  par(mar = c(mar[1], mar[2], 0, mar[4]))
  barplot(nranks[barorder,],
          beside = TRUE,
          width = 1,
          axes = F,
          space = c(0, 0),
          border = NA,
          ylim = c(0, nrow(ranks)),
          names.arg = paste(1:nalgs, ".", sep = ""),
          col = col[orderedAlgorithms],
          xlab = xlab)
  axis(1, at = c(0, dotplotborders),
       labels = NA, lwd = par("lwd"))
  box()
  par(opar)
  legendfn(orderedAlgorithms,
           col[orderedAlgorithms])
}
