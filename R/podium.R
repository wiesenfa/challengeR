podium.challenge=function(x,ranking.fun, layout.heights=c(1,0.4), #=function(x) aggregateThenRank(x,FUN = median,ties.method = "average"),
                          ...){
  ranking=x%>%ranking.fun
  if (inherits(ranking,"ranked.list")){
    ranking_list=lapply(ranking$matlist, function(a) t(a[,"rank",drop=F])["rank",])
    for (subt in names(x)){
      dd=as.challenge(x[[subt]],value=attr(x,"value"), algorithm=attr(x,"algorithm") ,case=attr(x,"case"),
                      annotator = attr(x,"annotator"),
                      smallBetter = !attr(x,"inverseOrder"))
      xx=as.warehouse.challenge(dd)
      podium.AlgorithmPerformance(xx, ranking=ranking_list[[subt]],layout.heights=layout.heights,...)
      title(subt,outer=T,line=-3)    
      }
    
  } else {
    ranking=t(ranking$mat[,"rank",drop=F])["rank",]
    x=as.warehouse.challenge(x)
    podium.AlgorithmPerformance(x, ranking=ranking,layout.heights=layout.heights,...)
    
  }
  
}

podium.ranked=function(x, layout.heights=c(1,0.4), #=function(x) aggregateThenRank(x,FUN = median,ties.method = "average"),
                       ...){
  ranking=x#%>%ranking.fun
  ranking=t(ranking$mat[,"rank",drop=F])["rank",]
  x=as.warehouse.challenge(x$data)
  podium.AlgorithmPerformance(x, ranking=ranking,layout.heights=layout.heights,...)
}

podium.ranked.list=function(x, layout.heights=c(1,0.4), #=function(x) aggregateThenRank(x,FUN = median,ties.method = "average"),
                            ...){
  ranking=x#%>%ranking.fun
  x=x$data
  ranking_list=lapply(ranking$matlist, function(a) t(a[,"rank",drop=F])["rank",])
  for (subt in names(x)){
    dd=as.challenge(x[[subt]],value=attr(x,"value"), algorithm=attr(x,"algorithm") ,case=attr(x,"case"),
                    annotator = attr(x,"annotator"),
                    smallBetter = !attr(x,"inverseOrder"))
    xx=as.warehouse.challenge(dd)
    podium.AlgorithmPerformance(xx, ranking=ranking_list[[subt]],layout.heights=layout.heights,...)
    title(subt,outer=T,line=-3)    }
  
}


podium.AlgorithmPerformance=
  function (x,ranking, xlab = NULL, ylab = NULL, lines.show = FALSE, lines.alpha = 0.2, 
            lines.lwd = 1, col,lines.col = col, dots.pch = 19, dots.cex = 1, 
            places.lty = 2, places.col = 1, legendfn = function(algs, 
                                                                cols) {
              legend("topleft", algs, lwd = 1, col = cols, bg = "white")
            }, layout.heights=c(1,0.4), 
            ...) {
    #   stopifnot(nlevels(x$datasets[, drop = TRUE]) == 1)
    #   stopifnot(nlevels(x$performances[, drop = TRUE]) == 1)
    m <- do.call(cbind, split(x$value, x$algorithms))
    if (is.null(xlab)) xlab <- "Podium"
    if (is.null(ylab)) ylab <- "Performance" #levels(x$performances[, drop = TRUE])
    if (missing(col)) col <- attr(x, "algorithm_colors")
    # beplot0.matrix
    
    podium.matrix(m, ranking=ranking,col = col, xlab = xlab, ylab = ylab, lines.show = lines.show, 
                  lines.alpha = lines.alpha, lines.lwd = lines.lwd, lines.col = lines.col, 
                  dots.pch = dots.pch, dots.cex = dots.cex, places.lty = places.lty, 
                  places.col = places.col, legendfn = legendfn,
                  inverseOrder=attributes(x)$challenge$inverseOrder,layout.heights=layout.heights,
                  ...)
  }


podium.matrix=
  function (x, 
            inverseOrder=FALSE,ranking, 
            col = 1:ncol(x), xlab = NULL, ylab = NULL, lines.show = FALSE, 
            lines.alpha = 0.2, lines.lwd = 1, lines.col = col, dots.pch = 19, 
            dots.cex = 1, places.lty = 2, places.col = 1, layout.heights=c(1,0.4), legendfn = function(algs, 
                                                                              cols) {
              legend("topleft", algs, lwd = 1, col = cols, bg = "white")
            }, 
            ...) {
    
    nalgs <- ncol(x)
    algs <- colnames(x)
    ranks <- t(apply((-1)^(inverseOrder)*x, 1, rank, ties.method = "random"))
    nranks <- apply(ranks, 2, function(y) table(factor(y, levels = 1:nalgs)))
    # originally in benchmark package. But why * (nalgs:1)/nalgs ?
    #   barranks <- rank(colSums((-1)^(inverseOrder)*x * (nalgs:1)/nalgs), ties.method = "random")
    #replace by user defined ranking
    barranks <- ranking
    barorder <- order(barranks)
    
    dotplotborders <- (0:nalgs) * nalgs
    dotplaces <- (1:nalgs) - 0.5
    names(dotplaces) <- names(barranks)[barorder]
    
    barcols <- col
    dotcols <- col
    linecols <- sapply(lines.col, function(c) {
      r <- col2rgb(c)
      rgb(r[1], r[2], r[3], alpha = round(255 * lines.alpha), 
          maxColorValue = 255)
    })
    
    opar <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), heights =layout.heights)
    mar <- par("mar")
    par(mar = c(0, mar[2], mar[3], mar[4]))
    
    plot(dotplotborders, rep(max(x), nalgs + 1), type = "n", 
         ylim = range(x, na.rm = TRUE), ylab = ylab, xlab = "", 
         axes = F)
    axis(1, at = dotplotborders, labels = NA, lwd = par("lwd"))
    axis(2, lwd = par("lwd"))
    box()
    abline(v = dotplotborders, lty = places.lty, col = places.col)
    linesegments <- function(x, y, ...) {
      n <- length(x)
      segments(x[-n], y[-n], x[-1], y[-1], ...)
    }
    drawthe <- function(fn, col, ...) {
      for (i in 1:nrow(x)) {
        r <- ranks[i, ]
        o <- order(r)
        performances <- (x[i, ])[o]
        places <- (dotplaces[names(r)] + ((r - 1) * nalgs))[o]
        fn(places, performances, col = col[names(r)[o]], ...)
      }
    }
    if (lines.show) 
      drawthe(linesegments, linecols, lwd = lines.lwd)
    drawthe(points, dotcols, pch = dots.pch, cex = dots.cex)
    
    legendfn(names(barranks)[barorder], dotcols[names(barranks)][barorder])
    
    par(mar = c(mar[1], mar[2], 0, mar[4]))
    barplot(t(nranks[, barorder]), beside = TRUE, width = 1, 
            axes = F, space = c(0, 0), border = NA, ylim = c(0, nrow(x)), 
            names.arg = paste(1:nalgs, ".", sep = ""), col = col[names(barranks)][barorder], 
            xlab = xlab)
    axis(1, at = c(0, dotplotborders), labels = NA, lwd = par("lwd"))
    box()
    par(opar)
  }





