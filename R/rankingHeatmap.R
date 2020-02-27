rankingHeatmap <- function(x,...) UseMethod("rankingHeatmap")
rankingHeatmap.default <- function(x, ...) stop("not implemented for this class")

rankingHeatmap.ranked=function (x,ties.method="min",...) {
  ordering=rownames(x$mat)[order(x$mat$rank)]
  #dd=x$data  
  # dd will be same as x$data, except that na.treat is handled if aggregateThenRank
  dd=as.challenge(x$data,
                  value=attr(x$data,"value"), 
                  algorithm=attr(x$data,"algorithm") ,
                  case=attr(x$data,"case"),
                  annotator = attr(x$data,"annotator"),
                  smallBetter = !attr(x$data,"largeBetter"),
                  na.treat=x$call[[1]][[1]]$na.treat)
  
  rankingHeatmap(dd,                 
                 ordering=ordering,
                 ties.method=ties.method,...)
}


rankingHeatmap.ranked.list=function (x,ties.method="min",...) {
  
  xx=x$data
  
  a=lapply(names(x$matlist),function(subt){
    ordering=rownames(x$matlist[[subt]])[order(x$matlist[[subt]]$rank)]
    
    dd=as.challenge(xx[[subt]],value=attr(xx,"value"), 
                    algorithm=attr(xx,"algorithm") ,
                    case=attr(xx,"case"),
                    annotator = attr(xx,"annotator"),
                    smallBetter = !attr(xx,"largeBetter"),
                    na.treat=x$call[[1]][[1]]$na.treat)
    
    rankingHeatmap(dd,
                   ordering=ordering,
                   ties.method=ties.method,...)
  })
  a
}


rankingHeatmap.challenge=function(x,
                                  ordering,
                                  ties.method="min",...){
  ranking=x%>%rank( ties.method = ties.method )
  
  dat=as.data.frame(table(ranking$mat[[attr(x,"algorithm")]],
                          ranking$mat$rank,
                          dnn=c("algorithm","rank")),
                    responseName = "Count")
  dat$algorithm=factor(dat$algorithm, levels=ordering)
  # dat$Count=as.factor(dat$Count)
  # dat$Count[dat$Count==0]=NA
  ncases=length(unique(x[[attr(x,"case")]]))
  ggplot(dat)+
    geom_raster(aes(algorithm,rank, fill= Count))+
    geom_hline(yintercept = seq(1.5,max(ranking$mat$rank)-.5,by=1),
               color=grey(.8),size=.3)+
    geom_vline(xintercept = seq(1.5,length(unique(dat$algorithm))-.5,by=1),
               color=grey(.8),size=.3)+
    scale_fill_viridis_c(direction = -1,
                         limits=c(0,ncases),
                         # limits=c(1,ncases),
                         # breaks=function(a) round(seq(a[1],a[2],length.out=5)),
                         # na.value = "white"
    )+
    theme(axis.text.x = element_text(angle = 90),
          aspect.ratio=1)+
    xlab("Algorithm")+
    ylab("Rank")
  #scale_y_discrete(name="Rank",breaks=rev(ranking$matlist[[subt]]$rank))
  
}
