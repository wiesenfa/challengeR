stability <- function(x,...) UseMethod("stability")
stability.default <- function(x, ...) stop("not implemented for this class")
stabilityByAlgorithm <- function(x,...) UseMethod("stabilityByAlgorithm")
stabilityByAlgorithm.default <- function(x, ...) stop("not implemented for this class")
stabilityByAlgorithmStacked <- function(x,...) UseMethod("stabilityByAlgorithmStacked")
stabilityByAlgorithmStacked.default <- function(x, ...) stop("not implemented for this class")
stabilityByTask <- function(x,...) UseMethod("stabilityByTask")
stabilityByTask.default <- function(x, ...) stop("not implemented for this class")


stability.ranked.list=function(x,
                               ordering,
                               probs=c(.025,.975),
                               max_size=6,
                               freq=FALSE,
                               shape=4,...){ 
  dd=melt(x,measure.vars="rank",value.name="rank")%>%dplyr::rename(task="L1")
  
  if (!missing(ordering)) {
    dd=dd%>%mutate(algorithm=factor(.data$algorithm, levels=ordering))
  } else dd=dd%>%mutate(algorithm=factor(.data$algorithm))
  if (!freq) {
    p = ggplot(dd)+
          geom_count(aes(algorithm ,rank,color=algorithm ,size = stat(prop*100)))
  } else {
    p=ggplot(dd)+
        geom_count(aes(algorithm ,rank,color=algorithm ))
  }  
  
  p+scale_size_area(max_size = max_size)+
    stat_summary(aes(algorithm ,rank ),
                 geom="point",
                 shape=shape,
                 fun.data=function(x) data.frame(y=median(x)),...)+
    stat_summary(aes(algorithm ,rank ),
                 geom="linerange",
                 fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                 ymax=quantile(x,probs[2])))+
    geom_abline(slope=1,color="gray",linetype="dotted")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    guides(size = guide_legend(title="%"))+
    scale_y_continuous(minor_breaks=NULL,
                       limits=c(1,max(5,max(dd$rank))), 
                       breaks=c(1,seq(5,max(5,max(dd$rank)),by=5)))+
    xlab("Algorithm")+
    ylab("Rank")
  
}


rankdist.bootstrap.list=function(x,...){
  rankDist=melt(lapply(x$bootsrappedRanks,t),value.name="rank") %>% dplyr::rename(algorithm="Var2",task="L1")
  rankDist
}



stabilityByAlgorithm.bootstrap.list=function(x,ordering,probs=c(.025,.975),max_size=3,shape=4,single=FALSE,...){
 rankDist=rankdist.bootstrap.list(x)
 
if (!missing(ordering)) rankDist=rankDist%>%mutate(algorithm=factor(.data$algorithm, levels=ordering))
if (single==FALSE){
  ggplot(rankDist)+
    geom_count(aes(task ,rank,color=algorithm,size = stat(prop*100), group = task ))+
    scale_size_area(max_size = max_size)+
    stat_summary(aes(task ,rank ),
                 geom="point",
                 shape=shape,
                 fun.data=function(x) data.frame(y=median(x)),...)+
    stat_summary(aes(task ,rank ),
                 geom="linerange",
                 fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                 ymax=quantile(x,probs[2])))+
    facet_wrap(vars(algorithm))+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    guides(size = guide_legend(title="%"))+
    scale_y_continuous(minor_breaks=NULL,
                       limits=c(1,max(5,max(rankDist$rank))), 
                       breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))+
    xlab("Task")+
    ylab("Rank")
  
} else {
  pl=list()
  for (alg in ordering){
    rankDist.alg=subset(rankDist,rankDist$algorithm==alg)
    pl[[alg]]=ggplot(rankDist.alg)+
            geom_count(aes(task ,rank,color=algorithm,size = stat(prop*100), group = task ))+
            scale_size_area(max_size = max_size)+
            stat_summary(aes(task ,rank ),
                         geom="point",
                         shape=shape,
                         fun.data=function(x) data.frame(y=median(x)),...)+
            stat_summary(aes(task ,rank ),
                         geom="linerange",
                         fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                         ymax=quantile(x,probs[2])))+
            facet_wrap(vars(algorithm))+ 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
            guides(size = guide_legend(title="%"))+
            scale_y_continuous(minor_breaks=NULL,
                               limits=c(1,max(5,max(rankDist$rank))), 
                               breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))+
            xlab("Task")+
            ylab("Rank")
    
    
  }
  pl
}
 
}




stabilityByAlgorithmStacked.bootstrap.list=function(x,ordering,freq=FALSE,...){
  rankDist=rankdist.bootstrap.list(x)
  if (!missing(ordering)) rankDist=rankDist%>%mutate(algorithm=factor(.data$algorithm, levels=ordering))
  rankDist=rankDist%>%group_by(task)%>%dplyr::count(.data$algorithm,.data$rank)
  rankDist=rankDist%>%group_by(.data$algorithm)%>%mutate(prop=.data$n/sum(.data$n))%>%ungroup
  rankDist=rankDist%>%data.frame%>%mutate(rank=as.factor(.data$rank))   
  
  
  results= melt.ranked.list(x,measure.vars="rank",value.name="rank") %>%dplyr::select(-.data$variable)
  colnames(results)[3]="task"
  if (!missing(ordering)) results=results%>%mutate(algorithm=factor(.data$algorithm, levels=ordering))
  
  if (freq)  
    ggplot(rankDist) +
      geom_bar(aes(rank,n,fill=task ),   
               position = "stack", stat = "identity") +    
      facet_wrap(vars(algorithm))+ 
      geom_vline(aes(xintercept=rank,
                     color=task),
                 size=.6,linetype="dotted",
                 data=results )+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
      xlab("Rank")
  else    
    ggplot(rankDist)+
      geom_bar(aes(rank,prop,fill=task ),   
               position = "stack", stat = "identity")+
      facet_wrap(vars(algorithm))+ 
      geom_vline(aes(xintercept=rank,
                     color=task),
                 size=.4,linetype="dotted",data=results)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
      xlab("Rank")
}


stability.bootstrap=function(x,
                             ordering,
                             probs=c(.025,.975),
                             max_size=3,
                             size.ranks=.3*theme_get()$text$size,
                             shape=4,...){
  if (missing(ordering)) ordering=  names(sort(t(x$mat[,"rank",drop=F])["rank",]))
  a=list(bootsrappedRanks=list(x$bootsrappedRanks),
         matlist=list(x$mat))
  names(a$bootsrappedRanks)=names(a$matlist)=""
  stabilityByTask.bootstrap.list(a,
                                 ordering=ordering,
                                 probs=probs,
                                 max_size = max_size,
                                 size.ranks=size.ranks,
                                 shape=shape,...)
  
  
}



stabilityByTask.bootstrap.list=function(x,
                                        ordering,
                                        probs=c(.025,.975),
                                        max_size=3,
                                        size.ranks=.3*theme_get()$text$size,
                                        shape=4,...){
  rankDist=rankdist.bootstrap.list(x)
  ranks=melt.ranked.list(x,measure.vars="rank", value.name = "full.rank")
  colnames(ranks)[4]="task"
  if (!missing(ordering)) {
    ranks$algorithm=factor(ranks$algorithm, levels=ordering)
    rankDist=rankDist%>%mutate(algorithm=factor(.data$algorithm, levels=ordering))
  }
 
  ggplot(rankDist)+ 
    geom_count(aes(algorithm ,
                   rank,
                   color=algorithm,
                   size = stat(prop*100), 
                   group = algorithm ))+
    scale_size_area(max_size = max_size)+
    geom_abline(slope=1,color="gray",linetype="dotted")+
    stat_summary(aes(algorithm ,rank ),
                 geom="point",
                 shape=shape,
                 fun.data=function(x) data.frame(y=median(x)),...)+
    stat_summary(aes(algorithm ,rank ),
                 geom="linerange",
                 fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),ymax=quantile(x,probs[2])))+
    geom_text(aes(x=algorithm,y=1,label=full.rank),
              nudge_y=-.6, 
              vjust = 0,
              size=size.ranks,
              fontface="plain",family="sans",data=ranks) +    
    coord_cartesian(clip = 'off')+
    facet_wrap(vars(task))+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    guides(size = guide_legend(title="%"))+
    scale_y_continuous(minor_breaks=NULL,
                       limits=c(.4,max(5,max(rankDist$rank))), 
                       breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))+
    xlab("Algorithm")+
    ylab("Rank")
}



