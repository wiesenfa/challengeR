significanceMap=function(rankedMat,relation_object,order=FALSE,size.rank=.2*theme_get()$text$size,...){
  rankedMat$algorithm=rownames(rankedMat)
  inc=relation_incidence(relation_object)
  
  #  rankedMat[order(rankedMat$rank),]
  if (order){
    scores=apply(inc,1,function(x) sum(x==0)-1)#+1-nrow(inc))
    # ordering=  names(sort(scores,decreasing = F))
    # scores=data.frame(algorithm=names(scores),score=scores,stringsAsFactors =F)
    scores2=apply(inc,2,function(x) sum(x==1))[names(scores)]#+1-nrow(inc))
    scores=data.frame(algorithm=names(scores),score=scores,score2=scores2,stringsAsFactors =F)
    scores=right_join(scores,rankedMat,by="algorithm")
    
    ordering= (scores[order(scores$score,scores$score2,
                            scores$rank),"algorithm"])
    scores=scores[,1:3]
  } else ordering=  names(sort(t(rankedMat[,"rank",drop=F])["rank",]))
  
  
  
  inc=inc[ordering,]
  

incidence.mat=melt(inc)
colnames(incidence.mat)=c("algorithm","notsigPair",     "decision")
incidence.mat$algorithm=as.character(incidence.mat$algorithm)
incidence.mat$notsigPair=as.character(incidence.mat$notsigPair)
#incidence.mat=incidence.mat[incidence.mat$decision==as.numeric(depictSignificant) ,]
incidence.mat=right_join(incidence.mat,rankedMat,by="algorithm")
# 
if (order) incidence.mat=right_join(incidence.mat,scores,by="algorithm")

incidence.mat=incidence.mat%>%mutate(algorithm=factor(algorithm, levels=ordering),
                                     notsigPair=factor(notsigPair, levels=ordering))

incidence.mat$decision=as.factor(incidence.mat$decision)
#fixy=nrow(rankedMat)+.5  
fixy=0
th_get=theme_get()
p=ggplot(incidence.mat)+
  geom_raster(aes(algorithm,notsigPair,fill=decision),...)+geom_raster(aes(algorithm,algorithm),fill="white")+
geom_abline(slope=1) +    
  coord_cartesian(clip = 'off')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.margin=unit(c(1,1,1,1), "lines"),
        legend.position="none"
  )+ylab("Algorithm")



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

significancePlot=function(rankedMat,relation_object,order=FALSE,depictSignificant=FALSE,...){
  rankedMat$algorithm=rownames(rankedMat)
  inc=relation_incidence(relation_object)
  
#  rankedMat[order(rankedMat$rank),]
  if (order){
    scores=apply(inc,1,function(x) sum(x==0)-1)#+1-nrow(inc))
    # ordering=  names(sort(scores,decreasing = F))
    # scores=data.frame(algorithm=names(scores),score=scores,stringsAsFactors =F)
    scores2=apply(inc,2,function(x) sum(x==1))[names(scores)]#+1-nrow(inc))
    scores=data.frame(algorithm=names(scores),score=scores,score2=scores2,stringsAsFactors =F)
    scores=right_join(scores,rankedMat,by="algorithm")

    ordering= (scores[order(scores$score,scores$score2,
                            scores$rank),"algorithm"])
     scores=scores[,1:3]
  } else ordering=  names(sort(t(rankedMat[,"rank",drop=F])["rank",]))
  
  
  
  inc=inc[ordering,]
  
  
  incidence.mat=melt(inc)
  colnames(incidence.mat)=c("algorithm","notsigPair",     "decision")
  incidence.mat$algorithm=as.character(incidence.mat$algorithm)
  incidence.mat$notsigPair=as.character(incidence.mat$notsigPair)
  incidence.mat=incidence.mat[incidence.mat$decision==as.numeric(depictSignificant) ,]
  incidence.mat=right_join(incidence.mat,rankedMat,by="algorithm")
  
  # incidence.list=lapply(1:nrow(inc),function(x) colnames(inc)[which(inc[x,]==as.numeric(depictSignificant))])
  # names(incidence.list)=ordering
  # incidence.list=lapply(incidence.list,function(x){
  #   # r=data.frame(rank=rankedMat[x,"rank"])
  #   # r$notsigPair=x
  #   # r
  #   data.frame(notsigPair=x)
  # } )
  # #  incidence.list=lapply(incidence.list,function(x){x[order(x$rank),]})
  # incidence.mat=melt(incidence.list,id="notsigPair")#%>%select(-variable)
  # colnames(incidence.mat)[2]="algorithm"
  # #  colnames(incidence.mat)[3]="algorithm"
  # incidence.mat=right_join(incidence.mat,rankedMat,by="algorithm")
  # 
  if (order) incidence.mat=right_join(incidence.mat,scores,by="algorithm")
  
  incidence.mat=incidence.mat%>%mutate(algorithm=factor(algorithm, levels=ordering),
                                       notsigPair=factor(notsigPair, levels=ordering))
  
  
#fixy=nrow(rankedMat)+.5  
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
