significanceMap.ranked=function(object,alpha=0.05,p.adjust.method="holm",order=FALSE,size.rank=.3*theme_get()$text$size,...){
  
  relensemble= object$data%>%decision.challenge(alpha=alpha,p.adjust.method=p.adjust.method) %>% as.relation
  
  significanceMap(object=object$mat,
                  relation_object=relensemble,order=order,size.rank=size.rank,...
  )
  
}  

significanceMap.ranked.list=function(object,alpha=0.05,p.adjust.method="holm",order=FALSE,size.rank=.3*theme_get()$text$size,...){
  a=object$data%>%decision.challenge(alpha=alpha,p.adjust.method=p.adjust.method)
  aa=lapply(a,as.relation.challenge.incidence)
  names(aa)=names(object$data)
  
  relensemble= do.call(relation_ensemble,args = aa)
  
  res=list()
  for (Task in names(object$data)){
    res[[Task]]=significanceMap.data.frame(object=object$matlist[[Task]],
                          relation_object=relensemble[[Task]],order=order,size.rank=size.rank,...
    )+ggtitle(Task)
    
  }
  
  res  
}  


#a=significanceMap(object,alpha=0.05,p.adjust.method="holm")

significanceMap.data.frame=function(object,relation_object,order=FALSE,size.rank=.3*theme_get()$text$size,...){
  object$algorithm=rownames(object)
  inc=relation_incidence(relation_object)
  
  #  object[order(object$rank),]
  if (order){
    scores=apply(inc,1,function(x) sum(x==0)-1)#+1-nrow(inc))
    # ordering=  names(sort(scores,decreasing = F))
    # scores=data.frame(algorithm=names(scores),score=scores,stringsAsFactors =F)
    scores2=apply(inc,2,function(x) sum(x==1))[names(scores)]#+1-nrow(inc))
    scores=data.frame(algorithm=names(scores),score=scores,score2=scores2,stringsAsFactors =F)
    scores=right_join(scores,object,by="algorithm")
    
    ordering= (scores[order(scores$score,scores$score2,
                            scores$rank),"algorithm"])
    scores=scores[,1:3]
  } else ordering=  names(sort(t(object[,"rank",drop=F])["rank",]))
  
  
  
  inc=inc[ordering,]
  

incidence.mat=melt(inc)
colnames(incidence.mat)=c("algorithm","notsigPair",     "decision")
incidence.mat$algorithm=as.character(incidence.mat$algorithm)
incidence.mat$notsigPair=as.character(incidence.mat$notsigPair)
#incidence.mat=incidence.mat[incidence.mat$decision==as.numeric(depictSignificant) ,]
incidence.mat=right_join(incidence.mat,object,by="algorithm")
# 
if (order) incidence.mat=right_join(incidence.mat,scores,by="algorithm")

incidence.mat=incidence.mat%>%mutate(algorithm=factor(algorithm, levels=ordering),
                                     notsigPair=factor(notsigPair, levels=ordering))

incidence.mat$decision=as.factor(incidence.mat$decision)
#fixy=nrow(object)+.5  
fixy=0
th_get=theme_get()
p=ggplot(incidence.mat)+
  geom_raster(aes(algorithm,notsigPair,fill=decision),...)+geom_raster(aes(algorithm,algorithm),fill="white")+
geom_abline(slope=1) +    
  coord_cartesian(clip = 'off')+
  theme(aspect.ratio=1,
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.margin=unit(c(1,1,1,1), "lines"),
        legend.position="none"
  )+ylab("Algorithm")+xlab("Algorithm")+scale_fill_manual(values=cividis(2,begin=0,end=1,alpha=.7))



# grid on top 
lt=th_get$panel.grid$linetype
if (is.null(lt)) lt=th_get$line$linetype

#p=p+theme(panel.background = element_rect(fill = NA),panel.ontop=TRUE) #-> grid will be on top of diagonal
#fix:
  f=ggplot_build(p)
  p= p + geom_vline(xintercept=f$layout$panel_params[[1]]$x.major_source, linetype=lt,color=th_get$panel.grid$colour, size=rel(th_get$panel.grid.major$size))+
    geom_hline(yintercept=f$layout$panel_params[[1]]$y.major_source, linetype=lt,color=th_get$panel.grid$colour, size=rel(th_get$panel.grid.major$size))+
    geom_abline(slope=1)+
    geom_text(aes(x=algorithm,y=fixy,label=rank),nudge_y=.5, # Set the position of the text to always be at '14.25'
              vjust = 0,
              size=size.rank,#rel(0.8),
              fontface="plain",family="sans"
    )

  
if (order) p=  p+
    #                geom_text(aes(x=algorithm,y=nrow(scores),label=score),nudge_y=0.5, # Set the position of the text to always be at '14.25'
    geom_text(aes(x=algorithm,y=fixy,label=score),nudge_y=0, # Set the position of the text to always be at '14.25'
              vjust = 0, size=size.rank,#rel(0.5),
              fontface="plain",family="sans") + 
    # annotate("text",x=0,y=nrow(scores)+1,  vjust = 0, size=3,fontface="plain",family="sans",label="original")+
    # annotate("text",x=0,y=nrow(scores)+.5,  vjust = 0, size=3,fontface="plain",family="sans",label="new")
    annotate("text",x=0,y=fixy+.5,  vjust = 0, size=size.rank, #rel(0.8),
             fontface="plain",family="sans",label="original")+
    annotate("text",x=0,y=fixy,  vjust = 0, size=size.rank,#rel(0.8),
             fontface="plain",family="sans",label="new")
  
return(p)

}

significancePlot=function(object,relation_object,order=FALSE,depictSignificant=FALSE,...){
  object$algorithm=rownames(object)
  inc=relation_incidence(relation_object)
  
#  object[order(object$rank),]
  if (order){
    scores=apply(inc,1,function(x) sum(x==0)-1)#+1-nrow(inc))
    # ordering=  names(sort(scores,decreasing = F))
    # scores=data.frame(algorithm=names(scores),score=scores,stringsAsFactors =F)
    scores2=apply(inc,2,function(x) sum(x==1))[names(scores)]#+1-nrow(inc))
    scores=data.frame(algorithm=names(scores),score=scores,score2=scores2,stringsAsFactors =F)
    scores=right_join(scores,object,by="algorithm")

    ordering= (scores[order(scores$score,scores$score2,
                            scores$rank),"algorithm"])
     scores=scores[,1:3]
  } else ordering=  names(sort(t(object[,"rank",drop=F])["rank",]))
  
  
  
  inc=inc[ordering,]
  
  
  incidence.mat=melt(inc)
  colnames(incidence.mat)=c("algorithm","notsigPair",     "decision")
  incidence.mat$algorithm=as.character(incidence.mat$algorithm)
  incidence.mat$notsigPair=as.character(incidence.mat$notsigPair)
  incidence.mat=incidence.mat[incidence.mat$decision==as.numeric(depictSignificant) ,]
  incidence.mat=right_join(incidence.mat,object,by="algorithm")
  
  # incidence.list=lapply(1:nrow(inc),function(x) colnames(inc)[which(inc[x,]==as.numeric(depictSignificant))])
  # names(incidence.list)=ordering
  # incidence.list=lapply(incidence.list,function(x){
  #   # r=data.frame(rank=object[x,"rank"])
  #   # r$notsigPair=x
  #   # r
  #   data.frame(notsigPair=x)
  # } )
  # #  incidence.list=lapply(incidence.list,function(x){x[order(x$rank),]})
  # incidence.mat=melt(incidence.list,id="notsigPair")#%>%select(-variable)
  # colnames(incidence.mat)[2]="algorithm"
  # #  colnames(incidence.mat)[3]="algorithm"
  # incidence.mat=right_join(incidence.mat,object,by="algorithm")
  # 
  if (order) incidence.mat=right_join(incidence.mat,scores,by="algorithm")
  
  incidence.mat=incidence.mat%>%mutate(algorithm=factor(algorithm, levels=ordering),
                                       notsigPair=factor(notsigPair, levels=ordering))
  
  
#fixy=nrow(object)+.5  
fixy=0
th_get=theme_get()

  p=ggplot(incidence.mat)+
    geom_point(aes(algorithm,notsigPair,color=algorithm),...)+
    geom_abline(slope=1)+
    geom_text(aes(x=algorithm,y=fixy,label=rank),nudge_y=.5, # Set the position of the text to always be at '14.25'
                  vjust = 0,
               size=rel(0.5),fontface="plain",family="sans"
               ) +    
    coord_cartesian(clip = 'off')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          plot.margin=unit(c(1,1,1,1), "lines"),
          legend.position="none"
          )+ylab("Algorithm")
  if (order) p=  p+
#                geom_text(aes(x=algorithm,y=nrow(scores),label=score),nudge_y=0.5, # Set the position of the text to always be at '14.25'
                geom_text(aes(x=algorithm,y=fixy,label=score),nudge_y=0, # Set the position of the text to always be at '14.25'
                vjust = 0, size=1*th_get$text$size,#rel(0.8),
                fontface="plain",family="sans") + 
                # annotate("text",x=0,y=nrow(scores)+1,  vjust = 0, size=3,fontface="plain",family="sans",label="original")+
                # annotate("text",x=0,y=nrow(scores)+.5,  vjust = 0, size=3,fontface="plain",family="sans",label="new")
                annotate("text",x=0,y=fixy+.5,  vjust = 0, size=rel(0.5),fontface="plain",family="sans",label="original")+
                annotate("text",x=0,y=fixy,  vjust = 0, size=rel(0.5),fontface="plain",family="sans",label="new")
  return(p)
  
}
