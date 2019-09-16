boxplot.challenge=function(x,ranking.fun=function(x) aggregateThenRank(x,FUN = median,ties.method = "min"),...){
  algo=attr(x,"algorithm")
  value=attr(x,"value")
  if (!is.null(ranking.fun))    ranking=x%>%ranking.fun
  
  if (inherits(x,"data.frame")){
    if (!is.null(ranking.fun)) x[[algo]]=factor(x[[algo]],levels=rownames(ranking$mat[order(ranking$mat$rank),]))
    a<-ggBoxplot.data.frame(x,algo,value)
  }  else {
    if (!is.null(ranking.fun)){
      for (i in names(x)) {
        x[[i]][[algo]]==factor(x[[i]][[algo]], levels=rownames(ranking$matlist[[i]][order(ranking$matlist[[i]]$rank),]))
      }
    }
    a<-ggBoxplot.list(x,algo,value)
    
  }
  a
}


ggBoxplot.data.frame=function(x,xx,yy,...){ #formula=metric_value~algorithm_id
  a=ggplot(aes_string(xx,yy),data=x)+geom_boxplot(outlier.shape = NA)+geom_jitter(position=position_jitter(width=0.2, height=0),color="blue",size=.3)+ theme(axis.text.x=element_text(angle = -90, hjust = 0))+scale_y_continuous(limits=c(0,1))
  a
  
}

ggBoxplot.list=function(x,xx,yy,...){ #formula=metric_value~algorithm_id
  
  # dat=melt(x,id.vars=c(algo,attr(x,"by")))
  # a=ggplot(aes_string(algo,value),data=dat)+geom_boxplot(outlier.shape = NA)+geom_jitter(position=position_jitter(width=0.2, height=0),color="blue",size=.3)+ theme(axis.text.x=element_text(angle = -90, hjust = 0))+facet_wrap((attr(x,"by")))
  
  a=lapply(1:length(x),function(id){
    ggplot(aes_string(xx,yy),data=x[[id]])+geom_boxplot(outlier.shape = NA)+geom_jitter(position=position_jitter(width=0.2, height=0),color="blue",size=.3)+ggtitle(names(x)[id]) + theme(axis.text.x=element_text(angle = -90, hjust = 0))+scale_y_continuous(limits=c(0,1))
    #  algos=unique(x[[id]][[attr(terms.formula(metric_value~algorithm_id),"term.labels")]])
    # if (length(algos)==1) title(xlab=algos)
    
  })
  a
}
  

Boxplot.list=function(x,formula,...){ #formula=metric_value~algorithm_id
  a=lapply(1:length(x),function(id){
    (boxplot(formula,data=x[[id]],ylim=c(0,1),las=2, 
             outline=FALSE,main=names(x)[id],
              ...))
    (stripchart(formula, data = x[[id]], 
                vertical = TRUE, method = "jitter", 
                pch = 21, col = "blue", add=TRUE,...) )
    algos=unique(x[[id]][[attr(terms.formula(metric_value~algorithm_id),"term.labels")]])
    if (length(algos)==1) title(xlab=algos)
    
  })
  
}


Boxplot.comparedRanks.list=function(x,...){
  tau=sapply(x,function(z) z$tau)
  boxplot(tau,ylim=c(0,1.0),las=2, outline=FALSE, 
          ylab="Kendall's tau",...)
  stripchart(tau, 
             vertical = TRUE, method = "jitter", 
             pch = 21, col = "blue", add=TRUE,...) 
  
}


Boxplot.bootstrap.list=function(x,...){
  winner.noboot=winner.ranked.list(x) 
  x2=winnerFrequencies(x)
  n.bootstraps= ncol(x$bootsrappedRanks[[1]])
  perc_boot_Winner=lapply(1:length(x2),function(i){
    x2.i=x2[[i]]
    winner.id=which(rownames(x2.i)%in%rownames(winner.noboot[[i]])) #could be multiple winners!!!!
    100*x2.i[winner.id,3,drop=F]/n.bootstraps
  })
  
  boxplot(unlist(perc_boot_Winner),ylim=c(0,100),las=2, outline=FALSE,
          ylab="% Bootstraps",xlab="Winner ranks 1",
          sub=paste(n.bootstraps,"Bootstraps"),...)
  stripchart(unlist(perc_boot_Winner), 
             vertical = TRUE, method = "jitter", 
             pch = 21, col = "blue", add=TRUE,...) 
}




# winnerFrequencies(bb)
 # winner.datax=winner(datax,InverseOrder=TRUE) #no bootstrap
 #  x2=numberRank1(datax,originalranking.datax,datax_boot)
 #  boot_W_1=x2[which(x2$algorithm_id==winner.datax),3]
 #  boot_NW_1=sum(x2[-which(x2$algorithm_id==winner.datax),3]>9)
# perc_boot_Winner=boot_W_1/Data$N_Bootstraps
# perc_boot_NotWinner=Data$Bootstrap_Rank1_NotWinner/(Data$N_Algo-1)

