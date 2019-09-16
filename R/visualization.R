stability1.ranked.list=function(x,ordering,probs=c(.025,.975),max_size=6,freq=FALSE,...){ # MOEGLICHEKEIT ZUM SORTIEREN FEHLT
  dd=melt(x)  %>%filter(variable=="rank")%>%dplyr::select(-variable)%>%dplyr::rename(task="L1",rank="value")
  
  if (!missing(ordering)) dd=dd%>%mutate(algorithm=factor(algorithm, levels=ordering))
  else dd=dd%>%mutate(algorithm=factor(algorithm))
  if (!freq)   p=ggplot(dd)+
      geom_count(aes(algorithm ,rank,color=algorithm ,size = stat(prop)))
  else   p=ggplot(dd)+
      geom_count(aes(algorithm ,rank,color=algorithm ))
  
  p+scale_size_area(max_size = max_size)+#scale_size_area(max_size = 6)
    #    stat_summary(aes(algorithm ,rank,color=algorithm ),fun.data=function(x) data.frame(ymin=min(x),y=x,ymax=max(x)))+
    stat_summary(aes(algorithm ,rank ),shape=18,size=.3,
                 fun.data=function(x) data.frame(ymin=quantile(x,probs[1]),y=median(x),ymax=quantile(x,probs[2])))+geom_abline(slope=1,color="gray",linetype="dotted")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    scale_y_continuous(minor_breaks=NULL,limits=c(1,max(5,max(dd$rank))), breaks=c(1,seq(5,max(5,max(dd$rank)),by=5)))#,max(rankDist$rank)))
 #   ggtitle("point for each observed rank, 95% interval&median in black")
  
}


rankdist.bootstrap.list=function(x,...){
  rankDist=melt(lapply(x$bootsrappedRanks,t))  
  rankDist=rankDist%>% dplyr::rename(algorithm="Var2",rank="value",task="L1")
  rankDist
}



stability1.bootstrap.list=function(x,ordering,max_size=3,...){
 rankDist=rankdist.bootstrap.list(x)
 
#  rankDist%>%#mutate(algorithm=factor(algorithm, levels=p12$algorithm))%>% # MOEGLICHEKEIT ZUM SORTIEREN FEHLT
#    ggplot()+
#   stat_summary(aes(task ,rank,color=task ),size=.4,
#                fun.data=function(x) data.frame(ymin=min(x),y=x,ymax=max(x)))+
# #  stat_summary(aes(task ,rank,color=task ),geom="point",fun.y=function(x) x)+
# #  stat_summary(aes(task ,rank ),geom="point",shape=4,fun.y = median)
#   stat_summary(aes(task ,rank ),shape=18,size=.3,
#                fun.data=function(x) data.frame(ymin=quantile(x,.25),y=median(x),ymax=quantile(x,.75)))+
#   facet_wrap(vars(algorithm))+ 
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
#    ggtitle("point for each observed rank, quartile range&median in black")

if (!missing(ordering)) rankDist=rankDist%>%mutate(algorithm=factor(algorithm, levels=ordering))
 
  ggplot(rankDist)+
    geom_count(aes(task ,rank,color=task,size = stat(prop), group = task ))+
     scale_size_area(max_size = max_size)+#scale_size_area(max_size = 6)
  # stat_summary(aes(algorithm,rank,color=algorithm),size=.4,
  #              fun.data=function(x) data.frame(ymin=min(x),y=x,ymax=max(x)))+ 
    stat_summary(aes(task ,rank), shape=18, size=.3, fun.data=function(x) data.frame(ymin=quantile(x,.025),y=median(x),ymax=quantile(x,.975)))+
    facet_wrap(vars(algorithm))+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    scale_y_continuous(minor_breaks=NULL,limits=c(1,max(5,max(rankDist$rank))), breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))#,max(rankDist$rank)))#+
 #   ggtitle("point for each observed rank, 95% interval &median in black")
 #+  scale_x_discrete(limits = p12$algorithm)
 
}




stability1stacked.bootstrap.list=function(x,ordering,freq=FALSE,...){
  rankDist=rankdist.bootstrap.list(x)
   if (!missing(ordering)) rankDist=rankDist%>%mutate(algorithm=factor(algorithm, levels=ordering))
 rankDist=rankDist%>%group_by(task)%>%dplyr::count(algorithm,rank)
  rankDist=rankDist%>%group_by(algorithm)%>%mutate(prop=n/sum(n))%>%ungroup
  rankDist=rankDist%>%data.frame%>%mutate(rank=as.factor(rank))     #filter(task %in% unique(data_matrix1$task[data_matrix1$phase==1]))


  results= x%>% melt.ranked.list %>%filter(variable=="rank")%>%dplyr::select(-variable)%>%
    dplyr::rename(rank=value)# %>% right_join(tasks)%>% right_join(phase)
  colnames(results)[3]="task"
   if (!missing(ordering)) results=results%>%mutate(algorithm=factor(algorithm, levels=ordering))

if (freq)  
  ggplot(rankDist)+
    geom_bar(aes(rank,n,fill=task ),   position = "stack", stat = "identity")+
    facet_wrap(vars(algorithm))+ 
    geom_vline(aes(xintercept=rank,color=task),size=.6,linetype="dotted",data=results
              # %>%mutate(algorithm=factor(algorithm, levels=p12$algorithm))%>%filter(task %in% unique(data_matrix1$task[data_matrix1$phase==1]))
    )+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))#+
#    ggtitle("bootstrap rank frequencies+ original") 
else    
  ggplot(rankDist)+
    geom_bar(aes(rank,prop,fill=task ),   position = "stack", stat = "identity")+
    facet_wrap(vars(algorithm))+ 
    geom_vline(aes(xintercept=rank,color=task),size=.4,linetype="dotted",data=results
              # %>%mutate(algorithm=factor(algorithm, levels=p12$algorithm))%>%filter(task %in% unique(data_matrix1$task[data_matrix1$phase==1]))
    )+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
 #   ggtitle("bootstrap rank proportions+ original") 
}


stability2.bootstrap=function(x,ordering,max_size=3,...){
  a=list(bootsrappedRanks=list(x$bootsrappedRanks),
         matlist=list(x$mat))
  names(a$bootsrappedRanks)=names(a$matlist)="Task"
  stability2.bootstrap.list(a,max_size = max_size,ordering=ordering)
  
  
}



stability2.bootstrap.list=function(x,ordering,max_size=3,...){
  rankDist=rankdist.bootstrap.list(x)
  ranks=melt.ranked.list(x, value.name = "full.rank")
  colnames(ranks)[4]="task"
  ranks=ranks[ranks$variable=="rank",]
  if (!missing(ordering)) {
    ranks$algorithm=factor(ranks$algorithm, levels=ordering)
    rankDist=rankDist%>%mutate(algorithm=factor(algorithm, levels=ordering))
  }
 
  ggplot(rankDist)+ 
    geom_count(aes(algorithm ,rank,color=algorithm,size = stat(prop), group = algorithm ))+
     scale_size_area(max_size = max_size)+
    geom_abline(slope=1,color="gray",linetype="dotted")+
  # geom_step(aes(x=algorithm,y=full.rank),data=ranks,color="gray",linetype="dotted")+
    stat_summary(aes(algorithm ,rank), shape=18, size=.3, fun.data=function(x) data.frame(ymin=quantile(x,.025),y=median(x),ymax=quantile(x,.975)))+
    geom_text(aes(x=algorithm,y=1,label=full.rank),nudge_y=-.5, # Set the position of the text to always be at '14.25'
              vjust = 0,
              size=.3*theme_get()$text$size,#rel(3),
              fontface="plain",family="sans",data=ranks
    ) +    coord_cartesian(clip = 'off')+
    facet_wrap(vars(task))+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    scale_y_continuous(minor_breaks=NULL,limits=c(.5,max(5,max(rankDist$rank))), breaks=c(1,seq(5,max(5,max(rankDist$rank)),by=5)))#,max(rankDist$rank)))#+
 #   ggtitle("point for each observed rank, 95% interval &median in black")

}



