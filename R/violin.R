violin <- function(x,...) UseMethod("violin")
violin.default <- function(x, ...) stop("not implemented for this class")

violin.bootstrap=function(x,...){
  a=list(bootsrappedRanks=list(x$bootsrappedRanks),
         matlist=list(x$mat))
  names(a$bootsrappedRanks)=names(a$matlist)=""
  violin.bootstrap.list(a,...)
  
}


violin.bootstrap.list=function(x,...){
  ken=melt(kendall.bootstrap.list(x))
  colnames(ken)[2]="Task"
  cat("\n\nSummary Kendall's tau\n")
  ss=ken%>%group_by(Task)%>%
    summarise(mean=mean(value,na.rm=T),
              median=median(value,na.rm=T),
              q25=quantile(value,probs = .25,na.rm=T),
              q75=quantile(value,probs = .75,na.rm=T))%>% 
    arrange(desc(median))
  
  print(as.data.frame(ss))
  
  ken%>%mutate(Task=factor(.data$Task, 
                           levels=ss$Task))%>%
    ggplot(aes(Task,value))+
    geom_violin(alpha=.3,
                color=NA,
                fill="blue")+
    geom_boxplot(width=0.1, 
                 fill="white")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "none")+
    ylab("Kendall's tau")+
    scale_y_continuous(limits=c(min(min(ken$value),0),
                                max(max(ken$value),1)))
}



kendall.bootstrap.list=function(x){
  ken=lapply(1:length(x$bootsrappedRanks),function(Task){
    id=match(rownames( x$bootsrappedRanks[[Task]]),
             rownames(x$matlist[[Task]]) )
    sapply(x$bootsrappedRanks[[Task]], 
           function(bootSample) suppressWarnings(kendall(bootSample,
                                                         x$matlist[[Task]]$rank[id])))
  } )
  names(ken)=names((x$bootsrappedRanks))
  
  if (sum(is.na(x))>0){
   cat("Bootstrap samples without variability in rankings (all algorithms ranked 1) excluded.\n Frequency of such samples by task:\n",fill = T)
    sapply(ken,function(x) sum(is.na(x)))
  }
  
  
  return(ken)
  
}


density.bootstrap.list=function(x,...){
  ken=melt(kendall.bootstrap.list(x))
  colnames(ken)[2]="Task"
  
  cat("\n\nSummary Kendall's tau\n")
  ss=ken%>%group_by(Task)%>%
    summarise(mean=mean(value,na.rm=T),
              median=median(value,na.rm=T),
              q25=quantile(value,probs = .25,na.rm=T),
              q75=quantile(value,probs = .75,na.rm=T))%>% 
    arrange(desc(median))
  
  print(as.data.frame(ss))
  
  ggplot(ken)+
    geom_density(aes(value,fill=Task),alpha=.3,color=NA)
}

