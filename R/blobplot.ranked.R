blobplot <- function(x,...) UseMethod("blobplot")
blobplot.default <- function(x, ...) stop("not implemented for this class")

blobplot.ranked=function(x,ties.method="min",probs=c(.025,.975),max_size=6,shape=4,...){ 
  ordering=rownames(x$mat)[order(x$mat$rank)]
  xx=x$data  
  ranking=xx%>%rank( ties.method = ties.method )
  
  dat=ranking$mat
  algorithm=attr(xx,"algorithm")
  dat[[algorithm]]=factor(dat[[algorithm]], levels=ordering)
  #dat[["case"]]=as.character(dat[["case"]])
  p = ggplot(dat)+
    #  geom_line(aes(x = algorithm, y = rank,color=case,  group=case ),size=.2,linetype=1,show.legend = F)+
    geom_count(aes(!!as.symbol(algorithm),
                   rank,
                   color=!!as.symbol(algorithm),
                   size = stat(prop*100)))
  
  p+scale_size_area(max_size = max_size)+
    stat_summary(aes(!!as.symbol(algorithm),rank),
                 geom="point",shape=shape,
                 fun.data=function(x) data.frame(y=median(x)),...) +
    stat_summary(aes(!!as.symbol(algorithm),rank ),
                 geom="linerange",
                 fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),
                                                 ymax=quantile(x,probs[2])))+
    geom_abline(slope=1,color="gray",linetype="dotted")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    guides(size = guide_legend(title="%"),color="none")+
    scale_y_continuous(minor_breaks=NULL,
                       limits=c(1,max(5,max(dat$rank))), 
                       breaks=c(1,seq(5,max(5,max(dat$rank)),by=5)))+
    xlab("Algorithm")+ylab("Rank")
}



