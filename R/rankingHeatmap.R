rankingHeatmap <- function(x,...) UseMethod("rankingHeatmap")
rankingHeatmap.default <- function(x, ...) stop("not implemented for this class")

rankingHeatmap.ranked=function (x,ties.method="min",...) {
  ordering=rownames(x$mat)[order(x$mat$rank)]
  dd=x$data  
  ranking=dd%>%rank( ties.method = ties.method )
  
  dat=as.data.frame(table(ranking$mat[[attr(dd,"algorithm")]],
                          ranking$mat$rank,
                          dnn=c("algorithm","rank")),
                    responseName = "Count")
  dat$algorithm=factor(dat$algorithm, levels=ordering)
 # dat$Count=as.factor(dat$Count)
  ncases=length(unique(dd[[attr(dd,"case")]]))
#  dat$Count[dat$Count==0]=NA
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
    theme(axis.text.x = element_text(angle = 90))+
    xlab("Algorithm")+ylab("Rank")
}


rankingHeatmap.ranked.list=function (x,ties.method="min",...) {
  xx=x$data
  a=lapply(names(x$matlist),function(subt){
    ordering=rownames(x$matlist[[subt]])[order(x$matlist[[subt]]$rank)]
    
    dd=as.challenge(xx[[subt]],value=attr(xx,"value"), 
                    algorithm=attr(xx,"algorithm") ,
                    case=attr(xx,"case"),
                    annotator = attr(xx,"annotator"),
                    smallBetter = !attr(xx,"largeBetter"))
    ranking=dd%>%rank( ties.method = ties.method )
    dat=as.data.frame(table(ranking$mat[[attr(xx,"algorithm")]],
                            ranking$mat$rank,
                            dnn=c("algorithm","rank")),
                      responseName = "Count")
    dat$algorithm=factor(dat$algorithm, levels=ordering)
    # dat$Count[dat$Count==0]=NA
    ncases=length(unique(dd[[attr(dd,"case")]]))
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
      theme(axis.text.x = element_text(angle = 90))+
      xlab("Algorithm")+
      ylab("Rank")
      #scale_y_discrete(name="Rank",breaks=rev(ranking$matlist[[subt]]$rank))
  })
  a
}

