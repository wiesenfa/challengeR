methodsplot.challenge=function(x, methods=list(
  testBased=.%>%test() %>% rank(ties.method = "min"),
  meanThenRank=  .%>%  aggregate(  FUN="mean") %>% rank(ties.method = "min"),
  medianThenRank=.%>% aggregate(  FUN="median") %>% rank(ties.method = "min"),
  rankThenMean= .%>%rank(ties.method = "min") %>%  aggregate(  FUN="mean") %>%rank(ties.method = "min"),
  rankThenMedian=.%>%rank(ties.method = "min") %>%  aggregate(  FUN="median") %>%rank(ties.method = "min")
),
ordering, ...){
  
  if (inherits(x,"list"))  {
    a=lapply(methods,function(fun) fun(x))  
    dat=melt(a,measure.vars="rank")
     colnames(dat)[4:5]=c("task","rankingMethod")  
    
    if (missing(ordering)){
      lev=sort(unique(dat$algorithm))
      lab=lev
    } else {
      lev=ordering
      #   lab=paste(1:length(ordering),ordering)
      lab=lev
    }
    
    dat=dat%>% dplyr::rename(rank=value)%>%mutate(rank=factor(rank))%>%
      mutate(task=factor(task))%>%
      mutate(algorithm=factor(algorithm, levels=lev,labels = lab))
    
    ggplot(data = dat) +
      aes(x = rankingMethod, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+
      facet_wrap( ~ task)+xlab("Ranking method")  +ylab("Rank")+ theme(
        # legend.position = "none",
        #panel.spacing = unit(0, "lines"),
        #strip.background = element_blank(),
        #     strip.text.x = element_text(angle = 90),
        #    strip.text.y = element_text(angle = 0),
        strip.placement = "outside",
        #   axis.text.y = element_text(color=levels(dat$algorithm)) ,
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )#+legend_none()
    
  } else {
    
    a=lapply(methods,function(fun) fun(x))  
    if (is.null(x$fulldata)) a=lapply(methods,function(fun) fun(x$data)) else {
      a=lapply(methods,function(fun) fun(x$fulldata))
      a=lapply(a, function(aa) {
        res=aa
        res$mat=aa$mat[rownames(a$testBased$mat)%in%rownames(x$mat),]
        res
      })
      
    } 
    dat=melt(a)
    colnames(dat)[ncol(dat)]=c("rankingMethod")  
    
    if (missing(ordering)){
      lev=sort(unique(dat$algorithm))
      lab=lev
    } else {
      lev=ordering
      #  lab=paste(1:length(ordering),ordering)
      lab=lev
    }
    
    dat=dat%>% mutate(rank=factor(rank))%>%
      mutate(algorithm=factor(algorithm, levels=lev,labels = lab))
    
    ggplot(data = dat) +
      aes(x = rankingMethod, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+xlab("Ranking method")+ylab("Rank")+
      theme(
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )
    
  }
  
  
}


methodsplot.ranked=function(x, methods=list(
  testBased=.%>%test() %>% rank(ties.method = "min"),
  meanThenRank=  .%>%  aggregate(  FUN="mean") %>% rank(ties.method = "min"),
  medianThenRank=.%>% aggregate(  FUN="median") %>% rank(ties.method = "min"),
  rankThenMean= .%>%rank(ties.method = "min") %>%  aggregate(  FUN="mean") %>%rank(ties.method = "min"),
  rankThenMedian=.%>%rank(ties.method = "min") %>%  aggregate(  FUN="median") %>%rank(ties.method = "min")
), ...){
  
    if (is.null(x$fulldata)) a=lapply(methods,function(fun) fun(x$data)) else {
      a=lapply(methods,function(fun) fun(x$fulldata))
      a=lapply(a, function(aa) {
        res=aa
        res$mat=aa$mat[rownames(a$testBased$mat)%in%rownames(x$mat),]
        res
      })
               
    } 
    dat=melt(a)
    colnames(dat)[ncol(dat)]=c("rankingMethod")  
    
    ordering=  names(sort(t(x$mat[,"rank",drop=F])["rank",]))
    lab=lev=ordering

    dat=dat%>% mutate(rank=factor(rank))%>%
      mutate(algorithm=factor(algorithm, levels=lev,labels = lab))
    
    ggplot(data = dat) +
      aes(x = rankingMethod, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+xlab("Ranking method")+ylab("Rank")+
      theme(
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )
    
}

# methodsplot.ranked.list does not exist, use methodpsplot.challenge instead since consonsus ranking needed for ordering (or alphabetical ordering instead)


#similar plot to methods plot, instead of across ranking methods across tasks
lineplot.challenge=function(x, ordering,...){
  if (inherits(x,"list"))  {
    dat=melt(x,measure.vars="rank")
    colnames(dat)[4]=c("task")  
    
    if (missing(ordering)){
      lev=sort(unique(dat$algorithm))
      lab=lev
    } else {
      lev=ordering
      lab=paste(1:length(ordering),ordering)
    }
    
    dat=dat%>% dplyr::rename(rank=value)%>%mutate(rank=factor(rank))%>%
      mutate(task=factor(task))%>%
      mutate(algorithm=factor(algorithm, levels=lev,labels = lab))
    
    
    
    
    ggplot(data = dat) +
      aes(x = task, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )
    
  } else stop("Only applicable to multiple tasks")
}

