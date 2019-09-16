kendall.bootstrap.list=function(x){
  ken=lapply(1:length(x$bootsrappedRanks),function(task){
    id=match(rownames( x$bootsrappedRanks[[task]]),rownames(x$matlist[[task]]) )
    sapply(x$bootsrappedRanks[[task]], function(bootSample) suppressWarnings(kendall(bootSample,x$matlist[[task]]$rank[id])))
  } )
  names(ken)=names((x$bootsrappedRanks))
  
  cat("Bootstrap samples without variability in rankings (all algorithms ranked 1) excluded.\n Frequency of such samples by task:\n",fill = T)
  sapply(ken,function(x) sum(is.na(x)))
  
  
  return(ken)
  
}


density.bootstrap.list=function(x,...){
  ken=melt(kendall.bootstrap.list(x))
  colnames(ken)[2]="task"
  
  cat("\n\nSummary Kendall's tau\n")
  ss=ken%>%group_by(task)%>%
    summarise(mean=mean(value,na.rm=T),median=median(value,na.rm=T),q25=quantile(value,probs = .25,na.rm=T),q75=quantile(value,probs = .75,na.rm=T))%>% 
    arrange(desc(median))
  
  print(ss)
  
  ggplot(ken)+
    geom_density(aes(value,fill=task),alpha=.3,color=NA)#+
  #  ggtitle("Densities of pairwise Kendall's tau",subtitle= "betw. original and bootstap rankings")
  
}

violin.bootstrap=function(x,...){
  a=list(bootsrappedRanks=list(x$bootsrappedRanks),
         matlist=list(x$mat))
  names(a$bootsrappedRanks)=names(a$matlist)="Task"
  violin.bootstrap.list(a,...)
  
}
violin.bootstrap.list=function(x,...){
  ken=melt(kendall.bootstrap.list(x))
  colnames(ken)[2]="task"
  cat("\n\nSummary Kendall's tau\n")
  ss=ken%>%group_by(task)%>%
    summarise(mean=mean(value,na.rm=T),median=median(value,na.rm=T),q25=quantile(value,probs = .25,na.rm=T),q75=quantile(value,probs = .75,na.rm=T))%>% 
    arrange(desc(median))
  
  print(ss)
  
  ken%>%mutate(task=factor(task, levels=ss$task))%>%ggplot(aes(task,value,fill=task))+
    geom_violin(alpha=.3,color=NA)+
    geom_boxplot(width=0.1, fill="white")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "none")#+
  #  ggtitle("Violin plot of pairwise Kendall's tau",subtitle= "betw. original and bootstap rankings")
}

