decision.challenge=function(x,                             na.treat=0, #entweder na.rm, numeric value oder function
                            alpha=0.05,p.adjust.method="none",alternative="one.sided",# only needed for significance 
                            test.fun=function(x,y) wilcox.test(x,y,
                                                               alternative = alternative,exact=FALSE,
                                                               paired = TRUE)$p.value,
                            parallel=FALSE,progress="none",...){
  
  if (alternative!="two.sided") alternative=ifelse(attr(x,"inverseOrder") ,yes="greater",no="less")
  call=match.call(expand.dots = T)  
  
  object=x
  algorithm=attr(object,"algorithm")
  dataset_id=attr(object,"case")
  inverseOrder=attr(object,"inverseOrder") 
  x=attr(object,"value")
  if(missing(dataset_id)| missing(inverseOrder)) stop("arguments case and alpha need to be given in as.challenge()")
  
  
  if (inherits(object,"list")){
    matlist=llply(1:length(object), function(id){ #lapply(object, function(y){
      piece=object[[id]]
      if (length(unique(piece[[algorithm]]))<=1){
        warning("only one ", algorithm, " available in element ", names(object)[id])
        #return(data.frame())  
        #    return(data.frame("prop_significance"=rep(NA,length(unique(piece[[algorithm]]))),row.names = unique(piece[[algorithm]])))  
        return(matrix(NA,1,1))
      } 
      if (is.numeric(na.treat)) piece[,x][is.na(piece[,x])]=na.treat
      else if (is.function(na.treat)) piece[,x][is.na(piece[,x])]=na.treat(piece[,x])
      mat=Decision(piece, x, algorithm, dataset_id, alpha, inverseOrder,p.adjust.method=p.adjust.method,alternative=alternative,test.fun=test.fun)
      #mat=sapply(mat,function(x){x[is.na(x)]=0;x} )
      mat=as.data.frame(mat)
      mat[is.na(mat)]=0
      mat=as.matrix(mat)
      class(mat)=c(class(mat),"challenge.incidence")
      mat
      
    }, 
    .parallel=parallel,.progress=progress )
    names(matlist)=names(x)
    return(matlist)
  } else {
    if (length(unique(object[[algorithm]]))<=1){
      warning("only one ", algorithm, " available")
      matlist=(matrix(NA,1,1))
    } else {
      mat=Decision(object, x, algorithm, dataset_id, alpha, inverseOrder,p.adjust.method=p.adjust.method,alternative=alternative,test.fun=test.fun)
    }
    #    mat=sapply(mat,function(x){x[is.na(x),drop=F]=0;x} )
    mat=as.data.frame(mat)
    mat[is.na(mat)]=0
    mat=as.matrix(mat)
    class(mat)=c(class(mat),"challenge.incidence")
    return(mat)
    
  }
}


Decision=function(object,x,by,dataset_id,alpha, inverseOrder=FALSE,p.adjust.method="none",alternative="one.sided",
                  test.fun=function(x,y) wilcox.test(x,y,
                                                     alternative = alternative,exact=FALSE,
                                                     paired = TRUE)$p.value
){
  algorithms=unique(object[[by]])
  if (length(unique(object[[dataset_id]]))==1){
    warning("Only one data set in task")
    return(matrix(NA,1,1))
    #    return(matrix(NA,dimnames=list(algorithms,algorithms)))
  } else {
    combinations=expand.grid(algorithms,algorithms)[,2:1]
    combinations=combinations[apply(combinations,1,function(z) anyDuplicated(z)==0),] # remove i==j
    
    pvalues=sapply(1:nrow(combinations), function(it){ #1:nrow(combinations)
      dat1=object[object[[by]]==combinations[it,1],]
      dat2=object[object[[by]]==combinations[it,2],]
      id=intersect(dat2[,dataset_id],dat1[,dataset_id])
      dat1=dat1[match(id,dat1[,dataset_id]),x]
      dat2=dat2[match(id,dat2[,dataset_id]),x]
      # wilcox.test(dat1,dat2,
      #             alternative = alternative,exact=FALSE,
      #             paired = TRUE)$p.value
      test.fun(dat1,dat2)
      
    })
    decisions=as.numeric(p.adjust(pvalues,method=p.adjust.method)<= alpha)
    res=cbind(combinations,decisions)
    reshape2::acast(res,Var2~Var1,value.var="decisions")
  }
}


# test.fun=function(x,y) suppressWarnings(frdAllPairsExactTest(list(x,y),p.adjust.method ="none")$p.value)
# test.fun=function(x,y) wilcox.test(x,y,
#                                    alternative = alternative,exact=FALSE,
#                                    paired = TRUE)$p.value





as.relation.challenge.incidence=function(x, verbose = FALSE, ...) {
  r <- relation(incidence = x, ...)
  
  
  # if ( x$type == "=" ) {
  #   props <- check_indifference_preference(r)
  #   class <- "indiffpref"
  # }
  # else {
  props <- check_strict_preference(r)
  class <- "strictpref"
  r$.Meta$is_decreasing <- FALSE
  # }
  
  r$.Meta <- c(r$.Meta,
               structure(props, names = sprintf("is_%s", names(props))))
  
  if ( verbose ) {
    # cat(sQuote(x$type), "preference relation:\n")
    
    for ( p in names(props) ) {
      cat(sprintf("%s = %s:\n", p, props[[p]]))
      print(relation_violations(r, p, TRUE))
    }
  }
  
  structure(r, class = c(class, class(r)))
}

check_strict_preference=
function(x) {
  list(irreflexive = relation_is_irreflexive(x),
       asymmetric = relation_is_asymmetric(x),
       transitive = relation_is_transitive(x),
       negatively_transitive = relation_is_negatively_transitive(x),
       trichotomous = relation_is_trichotomous(x))
}

# library(plyr)
# 
# a=challenge_multi%>%decision.challenge()
# relation(incidence=a$BRATS_L1)
# as.relation.challenge.incidence(a$BRATS_L1)
# aa=lapply(a,function(x) relation(incidence = x))
# 
#   relensemble= do.call(relation_ensemble,args = aa)
# 
# plot(relensemble)
# 




# significanceRanking=function(data,alpha,InverseOrder=TRUE){
#   data$algorithm_id_n=factor(data$algorithm_id)
#   levels(data$algorithm_id_n)=c(1:length(unique(data$algorithm_id)))
#   data$algorithm_id_n=as.numeric(paste(data$algorithm_id_n))
#   count=rep(NA,length(unique(data$algorithm_id_n)))
# 
#   for (my.ii in 1:length(unique(data$algorithm_id_n))){
#     count[my.ii]=0
#     for (my.jj in 1:length(unique(data$algorithm_id_n))){
#       if (!identical(my.ii,my.jj)){
#         dataii=subset(data,algorithm_id_n==my.ii)
#         datajj=subset(data,algorithm_id_n==my.jj)
#         datamerged=merge(dataii,datajj,by="dataset_id")
#         xx=try(wilcox.test(datamerged$metric_value.x,datamerged$metric_value.y,
#                            alternative = ifelse(InverseOrder,yes="greater",no="less"),exact=FALSE,
#                            paired = TRUE))
#         count[my.ii]=ifelse(xx$p.value <= alpha,count[my.ii]+1,count[my.ii])
#       }
#     }
#   }
# 
#   count=count/(length(unique(data$algorithm_id_n))-1)
#   
#   res=data.frame(unique(data$algorithm_id),count)
#   names(res)=c("algorithm_id","prop_significance")
#   res$rank_significance = rankNA(-res$prop_significance)
#   return(res)
# }
# 



# significance=function(object,x,by,dataset_id,alpha, inverseOrder=FALSE){
#     algorithms=unique(object[[by]])
#   if (length(unique(object[[dataset_id]]))==1){
#     warning("Only one data set in task")
#     return(data.frame("prop_significance"=rep(NA,length(algorithms)),row.names = algorithms))
#  #     return(data.frame("prop_significance"=numeric()))
#   } else {
#     # unique.combinations=t(combn(u,2))
#     combinations=expand.grid(algorithms,algorithms)[,2:1]
#     combinations=combinations[apply(combinations,1,function(z) anyDuplicated(z)==0),] # remove i==j
#   
#     pvalues=sapply(1:nrow(combinations), function(it){ #1:nrow(combinations)
#    #   dat1=object[object[[by]]==combinations[it,1],]
#    #   dat2=object[object[[by]]==combinations[it,2],]
#    #   datamerged=merge(dat1,dat2,by=dataset_id)
#    # try(as.numeric(wilcox.test(datamerged[[paste0(x,".x")]],datamerged[[paste0(x,".y")]],
#    #                    alternative = ifelse(inverseOrder,yes="greater",no="less"),exact=FALSE,
#    #                    paired = TRUE)$p.value<= alpha)
#    # )
#       #faster:
#       dat1=object[object[[by]]==combinations[it,1],]
#       dat2=object[object[[by]]==combinations[it,2],]
#       id=intersect(dat2[,dataset_id],dat1[,dataset_id])
#       dat1=dat1[match(id,dat1[,dataset_id]),x]
#       dat2=dat2[match(id,dat2[,dataset_id]),x]
#       #try(
#         as.numeric(wilcox.test(dat1,dat2,
#                          alternative = ifelse(inverseOrder,yes="greater",no="less"),exact=FALSE,
#                          paired = TRUE)$p.value<= alpha)
#        #)
#     })
#     algo.ids=lapply(algorithms,function(algo) which(combinations[,1]==algo))
#     names(algo.ids)=algorithms
#     prop_significance=sapply(algo.ids, function(z) sum(pvalues[z]))/(length(algorithms)-1)
#     return(data.frame("prop_significance"=prop_significance,row.names = names(prop_significance)))
#   }
# }

# significance=function(object,...){
#   a=decision.challenge(object,...)
#   prop_significance=  rowSums(a)/nrow(a)
#   return(data.frame("prop_significance"=prop_significance,row.names = names(prop_significance)))
# }

significance=function(object,x,algorithm,dataset_id,alpha, inverseOrder=FALSE,...){
  # algorithm=attr(object,"algorithm")
  # dataset_id=attr(object,"case")
  # inverseOrder=attr(object,"inverseOrder") 
  # x=attr(object,"value")
  
  xx=as.challenge(object,value=x,algorithm=algorithm,case=dataset_id,smallBetter = !inverseOrder,check=FALSE)
  a=decision.challenge(xx,...)
  prop_significance=  rowSums(a)/nrow(a)
  return(data.frame("prop_significance"=prop_significance,row.names = names(prop_significance)))
}


#dropNA=function(x) x[!is.na(x)]
# x=DataDSC%>% subset(task_id_n==1)
# x="metric_value"
# by="algorithm_id"
# dataset_id="dataset_id"
# inverseOrder=TRUE
# alpha=.05



# uu=expand.grid(u,u)[,2:1]
# uu=uu[apply(uu,1,function(z) anyDuplicated(z)==0),]
# 
# zz=rev(uu[1,])
# k=(apply(uu,1,function(z) all(z==zz)))
# rev(uu[1,])
# dim(uu)
# dim(combn(u,2))*2

