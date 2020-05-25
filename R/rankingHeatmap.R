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

    dd=as.challenge(xx[[subt]],
                    value=attr(xx,"value"),
                    algorithm=attr(xx,"algorithm") ,
                    case=attr(xx,"case"),
                    by=attr(xx, "by"),
                    annotator = attr(xx,"annotator"),
                    smallBetter = !attr(xx,"largeBetter"),
                    na.treat=x$call[[1]][[1]]$na.treat)

    rankingHeatmap(dd,
                   ordering=ordering,
                   ties.method=ties.method,...) + ggtitle(subt)
  })

  # Remove title for single-task data set
  if (length(a) == 1) {
    a[[1]]$labels$title <- NULL
  }

  a
}


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
    geom_hline(yintercept = seq(1.5,max(task$rank)-.5,by=1),
               color=grey(.8),size=.3)+
    geom_vline(xintercept = seq(1.5,length(unique(dat$algorithm))-.5,by=1),
               color=grey(.8),size=.3)+
    scale_fill_viridis_c(direction = -1,
                         limits=c(0,ncases)
    )+
    theme(axis.text.x = element_text(angle = 90),
          aspect.ratio=1)+
    xlab("Algorithm")+
    ylab("Rank")
}
