podium <- function(object,...) UseMethod("podium")
podium.default <- function(object, ...) stop("not implemented for this class")

podium.ranked.list=function(object,
                            xlab = NULL,
                            ylab = NULL,
                            lines.show = TRUE,
                            lines.alpha = 0.2,
                            lines.lwd = 1,
                            col,
                            lines.col = col,
                            dots.pch = 19,
                            dots.cex = 1,
                            places.lty = 2,
                            places.col = 1,
                            legendfn = function(algs, cols) {
                              legend("topleft", algs, lwd = 1, col = cols, bg = "white")
                            },
                            layout.heights=c(1,0.4),
                            ...){
    if (is.null(xlab)) xlab <- "Podium"
    if (is.null(ylab)) ylab <- "Performance"
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
                      smallBetter = !attr(x,"largeBetter"),
                      na.treat=object$call[[1]][[1]]$na.treat)

      podiumPlot <- podium(dd,
             ordering=ordering,
             xlab = xlab, ylab = ylab,
             lines.show = lines.show,
             lines.alpha = lines.alpha,
             lines.lwd = lines.lwd,
             col=col,
             lines.col = lines.col,
             dots.pch = dots.pch,
             dots.cex = dots.cex,
             places.lty = places.lty,
             places.col = places.col,
             legendfn = legendfn,
             layout.heights=layout.heights,
             ...)
      title(subt,outer=T,line=-3)

      append(podiumPlots, podiumPlot)
    }
}

podium.challenge=function(object,
                          ordering,
                          xlab = NULL, ylab = NULL,
                          lines.show = FALSE, lines.alpha = 0.2,
                          lines.lwd = 1,
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
      fn(places, performances, col = col[names(r)[o]], ...)
    }
  }
  if (lines.show) drawthe(linesegments, linecols, lwd = lines.lwd)

  drawthe(points, col, pch = dots.pch, cex = dots.cex)

  legendfn(orderedAlgorithms,
           col[orderedAlgorithms])

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
}
