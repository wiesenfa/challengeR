methodsplot <- function(x,...) UseMethod("methodsplot")
methodsplot.default <- function(x, ...) stop("not implemented for this class")

methodsplot.challenge=function(x, 
                               na.treat=NULL,
                               methods=list(testBased=.%>%test() %>% rank(ties.method = "min"),
                                            meanThenRank=  .%>%  aggregate(  FUN="mean") %>% rank(ties.method = "min"),
                                            medianThenRank=.%>% aggregate(  FUN="median") %>% rank(ties.method = "min"),
                                            rankThenMean= .%>%rank(ties.method = "min") %>%  aggregate(  FUN="mean") %>%rank(ties.method = "min"),
                                            rankThenMedian=.%>%rank(ties.method = "min") %>%  aggregate(  FUN="median") %>%rank(ties.method = "min")
                                            ),
                               ordering, ...){
  
   if (!inherits(x,"list")){
    if (any(is.na(x[,attr(x, "value")]))) { # only if missings present, else do nothing
      if (is.null(na.treat)){
        warning("Please specify na.treat in as.challenge()")
        return(NULL)
      } else {
        x=as.challenge(x,
                      value=attr(x,"value"), 
                      algorithm=attr(x,"algorithm") ,
                      case=attr(x,"case"),
                      annotator = attr(x,"annotator"),
                      smallBetter = !attr(x,"largeBetter"),
                      na.treat=na.treat)
       } 
    }
  } else {
    if (any(sapply(x, 
                    function(task) any(is.na(task[,attr(x, "value")])))))  { # only if missings present, else do nothing
      if (is.null(na.treat)){
        warning("Please specify na.treat in as.challenge()")
        return(NULL)
      } else {
        xx = melt(x,
                  id.vars=c(attr(x,"value"), 
                            attr(x,"algorithm") ,
                            attr(x,"case"),
                            attr(x,"annotator"),
                            attr(x,"by")
        ))
        
        x=as.challenge(xx,
                       value=attr(x,"value"), 
                       algorithm=attr(x,"algorithm") ,
                       case=attr(x,"case"),
                       by=attr(x,"by"),
                       annotator = attr(x,"annotator"),
                       smallBetter = !attr(x,"largeBetter"),
                       na.treat=na.treat)
      } 
    }
  }

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
    
    dat=dat%>% 
      dplyr::rename(rank=.data$value)%>%
      mutate(rank=factor(.data$rank))%>%
      mutate(task=factor(.data$task))%>%
      mutate(algorithm=factor(.data$algorithm, levels=lev,labels = lab))
    
    ggplot(data = dat) +
      aes(x = rankingMethod, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+
      facet_wrap( ~ task)+
      xlab("Ranking method")  +
      ylab("Rank")+ 
      theme(
        # legend.position = "none",
        #panel.spacing = unit(0, "lines"),
        #strip.background = element_blank(),
        #     strip.text.x = element_text(angle = 90),
        #    strip.text.y = element_text(angle = 0),
        strip.placement = "outside",
        #   axis.text.y = element_text(color=levels(dat$algorithm)) ,
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) 
      )#+legend_none()
    
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
    
    dat=dat%>% 
      mutate(rank=factor(.data$rank))%>%
      mutate(algorithm=factor(.data$algorithm, levels=lev,labels = lab))
    
    ggplot(data = dat) +
      aes(x = rankingMethod, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+
      xlab("Ranking method")+
      ylab("Rank")+
      theme(
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )
    
  }
  
  
}




methodsplot.ranked=function(x, 
                            methods=list(testBased=.%>%test() %>% rank(ties.method = "min"),
                                         meanThenRank=  .%>%  aggregate(  FUN="mean") %>% rank(ties.method = "min"),
                                         medianThenRank=.%>% aggregate(  FUN="median") %>% rank(ties.method = "min"),
                                         rankThenMean= .%>%rank(ties.method = "min") %>%  aggregate(  FUN="mean") %>%rank(ties.method = "min"),
                                         rankThenMedian=.%>%rank(ties.method = "min") %>%  aggregate(  FUN="median") %>%rank(ties.method = "min")
                                         ), 
                            ...){
  
  na.treat=x$call[[1]][[1]]$na.treat
  if (any(is.na(x$data[,attr(x, "value")]))) { #missings present
    if (is.null(na.treat)){
      warning("Please specify na.treat in as.challenge()")
      return(NULL)
    } else {
      if (is.null(x$fulldata)) {
        xx=x$data
        x$data=as.challenge(xx,
                        value=attr(xx,"value"), 
                        algorithm=attr(xx,"algorithm") ,
                        case=attr(xx,"case"),
                        annotator = attr(xx,"annotator"),
                        smallBetter = !attr(xx,"largeBetter"),
                        na.treat=na.treat)
      } else {
        xx=x$fulldata
        x$fulldata=as.challenge(xx,
                            value=attr(xx,"value"), 
                            algorithm=attr(xx,"algorithm") ,
                            case=attr(xx,"case"),
                            annotator = attr(xx,"annotator"),
                            smallBetter = !attr(xx,"largeBetter"),
                            na.treat=na.treat)
      }
    } 
  }
  
  
    if (is.null(x$fulldata)) {
      a=lapply(methods,function(fun) fun(x$data)) 
    } else {
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

    dat=dat%>% 
      mutate(rank=factor(.data$rank))%>%
      mutate(algorithm=factor(.data$algorithm, levels=lev,labels = lab))
    
    ggplot(data = dat) +
      aes(x = rankingMethod, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+
      xlab("Ranking method")+
      ylab("Rank")+
      theme(
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )
    
}

# methodsplot.ranked.list does not exist, use methodpsplot.challenge instead since consonsus ranking needed for ordering (or alphabetical ordering instead)





#similar plot to methods plot, instead of across ranking methods across tasks
lineplot <- function(x,...) UseMethod("lineplot")
lineplot.default <- function(x, ...) stop("not implemented for this class")

lineplot.challenge=function(x, 
                            ordering,...){
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
    
    dat=dat%>% 
      dplyr::rename(rank=.data$value)%>%
      mutate(rank=factor(.data$rank))%>%
      mutate(task=factor(.data$task))%>%
      mutate(algorithm=factor(.data$algorithm, levels=lev,labels = lab))
    
    ggplot(data = dat) +
      aes(x = task, y = rank, color=algorithm, group=algorithm ) +
      geom_line(size=1)+
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )
    
  } else stop("Only applicable to multiple tasks")
}

