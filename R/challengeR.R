as.challenge=function(object, value, algorithm ,case=NULL,#subset=NULL,
                      by=NULL, annotator=NULL, smallBetter=FALSE){
  #if (!is.null(subset)) object=object%>% subset(subset=subset) else object=object
  object=object %>% dplyr::select(value,algorithm,case,by,annotator)
  if (!is.null(by)) object=by(object,by=by)
 attr(object,"algorithm")=algorithm
  attr(object,"value")=value
  attr(object,"case")=case
  attr(object,"annotator")=annotator
  attr(object,"by")=by 
  attr(object,"inverseOrder")=!smallBetter
  class(object)=c("challenge",class(object))
  object
}



