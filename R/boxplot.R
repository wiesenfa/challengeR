
boxplot.ranked.list=function(x,
                             color="blue",
                             jitter.width=0.25,...){
  algo=attr(x$data,"algorithm")
  value=attr(x$data,"value")
  ranking=x
  x=x$data

  for (i in names(x)) {
    x[[i]][[algo]]=factor(x[[i]][[algo]],
                          levels=rownames(ranking$matlist[[i]][order(ranking$matlist[[i]]$rank),]))
  }

  a=lapply(1:length(x),function(id){
    ggplot(aes_string(algo,value),data=x[[id]])+
      geom_jitter(position=position_jitter(width=jitter.width, height=0),
                  color=color,...)+
      geom_boxplot(outlier.shape = NA,fill=NA)+
      ggtitle(names(x)[id]) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0))

  })
  names(a) = names(x)

  # Remove title for single-task data set
  if (length(a) == 1) {
    a[[1]]$labels$title <- NULL
  }
  if (length(a) >1) class(a) <- "ggList"
  else a <- a[[1]]
  
  a
}






boxplot.comparedRanks.list=function(x,...){
  tau=sapply(x,function(z) z$tau)
  boxplot(tau,ylim=c(0,1.0),las=2, outline=FALSE,
          ylab="Kendall's tau",...)
  stripchart(tau,
             vertical = TRUE, method = "jitter",
             pch = 21, col = "blue", add=TRUE,...)

}


boxplot.bootstrap.list=function(x,...){
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
 # winner.datax=winner(datax, largeBetter=TRUE) #no bootstrap
 #  x2=numberRank1(datax,originalranking.datax,datax_boot)
 #  boot_W_1=x2[which(x2$algorithm_id==winner.datax),3]
 #  boot_NW_1=sum(x2[-which(x2$algorithm_id==winner.datax),3]>9)
# perc_boot_Winner=boot_W_1/Data$N_Bootstraps
# perc_boot_NotWinner=Data$Bootstrap_Rank1_NotWinner/(Data$N_Algo-1)

