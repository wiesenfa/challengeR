relation_dissimilarity.ranked.list=function(x,method=kendall,...){  #method in kendall, spearmansFootrule, spearmansWeightedFootrule or any other function with two arguments
  tt=names(x$matlist)
  n.tt=length(tt)
  tau=matrix(NA,n.tt,n.tt)
  colnames(tau)=rownames(tau)=tt
  aa=melt(x,measure.vars="rank")
  for (i in 1:n.tt){
    for (j in 1:n.tt){
      temp=aa%>%filter(L1==as.character(tt[i]))%>% 
        right_join(aa%>%filter(L1==as.character(tt[j])),by="algorithm")
      tau[i,j]=method(temp$value.x,temp$value.y) 
    }
  }
  
  if (method(1:2,1:2)==1 & method(2:1,1:2)==-1)  as.dist(1-tau)  #if two identical objects yield value of 1, method seems to be a correlation
  else as.dist(tau) #distance
}


as.relation.ranked.list=function(x,...){
 res= lapply(x$matlist,function(z){
    r=z[,"rank"]
    names(r)=rownames(z)
    as.relation(r)
  } )
 class(res)="relation_ensemble"
 res
}
