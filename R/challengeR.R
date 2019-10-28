as.challenge=function(object, value, algorithm ,case=NULL,#subset=NULL,
                      by=NULL, annotator=NULL, smallBetter=FALSE,check=TRUE){
  #if (!is.null(subset)) object=object%>% subset(subset=subset) else object=object
  object=object %>% dplyr::select(value,algorithm,case,by,annotator)
  if (!is.null(by)) object=by(object,by=by)
  
  # sanity checks
  if (check){
    if (is.data.frame(object)){
      object=droplevels(object)
      all1=apply(table(object[[algorithm]],object[[case]]), 2,function(x) all(x==1))
      if (!all(all1)) stop ("case(s) (", paste(names(which(all1!=1)),collapse=", "), ") is/are not available for all algorithms or appear(s) more than once for the same algorithm")
    } else {
      object=lapply(object,droplevels)
      lapply(names(object), function(task){
        all1=apply(table(object[[task]][[algorithm]],object[[task]][[case]]), 2,function(x) all(x==1))
        if (!all(all1)) stop ("case(s) (", paste(names(which(all1!=1)),collapse=", "), ") is/are not available for all algorithms or appear(s) more than once for the same algorithm in task ", task)
        # add check that all algoirthms contained in all tasks
      })
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


# fillMissings.challenge=function(object){
#   aa=data.frame(alg=c(1,2,1),id=c(3,3,3),val=c(.1,.2,.3))
#   tidyr::expand(object,object[[algorithm]],id)%>%dplyr::left_join(aa,by=c("alg","id"))
#   
# }
