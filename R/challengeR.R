# as.challenge=function(object, value, algorithm ,case=NULL,#subset=NULL,
#                       by=NULL, annotator=NULL, smallBetter=FALSE,check=TRUE){
#   #if (!is.null(subset)) object=object%>% subset(subset=subset) else object=object
#   object=object[,c(value,algorithm,case,by,annotator)]
#   
#   if (!is.null(by)) object=by(object,by=by)
#   
#   # sanity checks
#   if (check){
#     if (is.data.frame(object)){
#       object=droplevels(object)
#       all1=apply(table(object[[algorithm]],object[[case]]), 2,function(x) all(x==1))
#       if (!all(all1)) stop ("case(s) (", paste(names(which(all1!=1)),collapse=", "), ") is/are not available for all algorithms or appear(s) more than once for the same algorithm")
#     } else {
#       object=lapply(object,droplevels)
#       lapply(names(object), function(task){
#         all1=apply(table(object[[task]][[algorithm]],object[[task]][[case]]), 2,function(x) all(x==1))
#         if (!all(all1)) stop ("case(s) (", paste(names(which(all1!=1)),collapse=", "), ") is/are not available for all algorithms or appear(s) more than once for the same algorithm in task ", task)
#         # add check that all algoirthms contained in all tasks
#       })
#     }
#    
#   }
#     
#   attr(object,"algorithm")=algorithm
#   attr(object,"value")=value
#   attr(object,"case")=case
#   attr(object,"annotator")=annotator
#   attr(object,"by")=by 
#   attr(object,"inverseOrder")=!smallBetter
#   class(object)=c("challenge",class(object))
#   object
# }
as.challenge=function(object, value, algorithm ,case=NULL,#subset=NULL,
                      by=NULL, annotator=NULL, smallBetter=FALSE,check=TRUE){
  #if (!is.null(subset)) object=object%>% subset(subset=subset) else object=object
  object=object[,c(value,algorithm,case,by,annotator)]
  
  
  # sanity checks
  if (check){
#    if (is.data.frame(object)){
      if (is.null(by)){
        missingData=object %>% expand(!!as.symbol(algorithm),!!as.symbol(case))%>% anti_join(object,by=c(algorithm,case))
        if (nrow(missingData)>0) {
          message("Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")
          print(as.data.frame(missingData))
          object=as.data.frame(object %>% complete(!!as.symbol(algorithm),!!as.symbol(case)))
      } else {
        object=droplevels(object)
        all1=apply(table(object[[algorithm]],object[[case]]), 2,function(x) all(x==1))
        if (!all(all1)) stop ("Case(s) (", paste(names(which(all1!=1)),collapse=", "), ") appear(s) more than once for the same algorithm")
        
      }

    } else {
        object=by(object,by=by)
        object=lapply(object,droplevels)
        for (task in names(object)){
          missingData=object[[task]] %>% expand(!!as.symbol(algorithm),!!as.symbol(case))%>% anti_join(object[[task]],by=c( algorithm,case))
          if (nrow(missingData)>0) {
            message("Performance of not all algorithms is observed for all cases in task ",task,". Inserted as missings in following cases:")
            print(as.data.frame(missingData))
            object[[task]]=as.data.frame(object[[task]] %>% complete(!!as.symbol(algorithm),!!as.symbol(case)))
           } else {
            all1=apply(table(object[[task]][[algorithm]],object[[task]][[case]]), 2,function(x) all(x==1))
            if (!all(all1)) stop ("Case(s) (", paste(names(which(all1!=1)),collapse=", "), ") appear(s) more than once for the same algorithm in task ", task)
         }
         }
  
    }
    
  }
  
  attr(object,"algorithm")=algorithm
  attr(object,"value")=value
  attr(object,"case")=case
  attr(object,"annotator")=annotator
  attr(object,"by")=by 
  attr(object,"inverseOrder")=!smallBetter
  class(object)=c("challenge",class(object))
  object
}


