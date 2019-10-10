rankingHeatmap.challenge=function (x, ranking.fun=function(x)  aggregateThenRank(x,FUN,ties.method = "min"),col, breaks,vp = viewport(),...) {
  ranking=x%>%ranking.fun
  if (inherits(ranking,"ranked.list")){
    ranking_list=lapply(ranking$matlist, function(a) t(a[,"rank",drop=F])["rank",])
    for (subt in names(x)){
      rankingorder <- order(ranking_list[[subt]])
      inverseOrder=attributes(x)$inverseOrder
      
      ncases=length(unique(x[[subt]][[attr(x,"case")]]))
      if (missing(col)){
        col=viridisLite::viridis(ncases+1,direction=-1)
        breaks=seq(0,ncases+1,length.out=ncases+1)
      } #else breaks=NA
      
      dd=as.challenge(x[[subt]],value=attr(x,"value"), algorithm=attr(x,"algorithm") ,case=attr(x,"case"))
      xx=as.warehouse.challenge(dd)
      m <- do.call(cbind, split(xx$value, xx$algorithms))
      
      nalgs <- ncol(m)
      algs <- colnames(m)
      ranks <- t(apply((-1)^(inverseOrder)*m, 1, rank, ties.method = "random"))
      nranks <- apply(ranks, 2, function(y) table(factor(y, levels = 1:nalgs)))
      
      myplot <-pheatmap((nranks[, names(ranking_list[[subt]])[rankingorder]]),cluster_rows = F,cluster_cols = F,ylab="rank",color =col,breaks=breaks,silent=T,main=subt,...)
      grid.newpage()
      pushViewport(vp)
      grid.draw(myplot$gtable)
      
    }
      
  } else {
    
    ranking=t(ranking$mat[,"rank",drop=F])["rank",]
    rankingorder <- order(ranking)
    inverseOrder=attributes(x)$inverseOrder
    
    ncases=length(unique(x[[attr(x,"case")]]))
    if (missing(col)){
      col=viridisLite::viridis(ncases+1,direction=-1)
      breaks=seq(0,ncases+1,length.out=ncases+1)
    } #else breaks=NA
    
    x=as.warehouse.challenge(x)
    m <- do.call(cbind, split(x$value, x$algorithms))
    
    nalgs <- ncol(m)
    algs <- colnames(m)
    ranks <- t(apply((-1)^(inverseOrder)*m, 1, rank, ties.method = "random"))
    nranks <- apply(ranks, 2, function(y) table(factor(y, levels = 1:nalgs)))
    
    myplot <-pheatmap((nranks[, names(ranking)[rankingorder]]),cluster_rows = F,cluster_cols = F,ylab="rank",color =col,breaks=breaks,silent=T,...)
  #  grid.newpage()
    pushViewport(vp)
    grid.draw(myplot$gtable)
  }
  }
