# merge.list=function(x,y,by="row.names",suffixes = c(".1",".2"),...){
#   if (is.list(x) & is.list(y)){
#     if (!all.equal(names(x),names(y))) stop("list elements must have same names and lists must have same length")
#     res=lapply(1:length(x), function(z){
#       merge(x[[z]],y[[z]],by=by,suffixes=suffixes,...)
#     })
#     names(res)=names(x)
#     res
#     
#   } else stop("Comparison of a list and a data.frame under construction")
# }

merge.list=function(x,y,by="row.names",suffixes = c(".1",".2"),...){
  if (is.list(x) & is.list(y)){
    #if (!all.equal(names(x),names(y))) stop("list elements must have same names and lists must have same length")
    common.elements=intersect(names(x),names(y))
    
    res=lapply(common.elements, function(z){
      merge(x[[z]],y[[z]],by=by,suffixes=suffixes,...)
    })
    names(res)=common.elements
    res
    
  } else stop("Comparison of a list and a data.frame under construction")
}
  
quickmerge.list=function(x,y){
  if (is.list(x) & is.list(y)){
    #if (!all.equal(names(x),names(y))) stop("list elements must have same names and lists must have same length")
    common.elements=intersect(names(x),names(y))
    
    res=lapply(common.elements, function(z){
      dat1=x[[z]]
      dat2=y[[z]]
      dat1=dat1[order(rownames(dat1)),,drop=F]
      dat2=dat2[order(rownames(dat2)),,drop=F]
      if (all(rownames(dat1)==rownames(dat2))) {
          qq=cbind(dat1,dat2)
          rownames(qq)=rownames(dat1)
          qq
      }
      else {
        id=intersect(rownames(dat1),rownames(dat2))
        dat1=dat1[match(id,rownames(dat1)),]
        dat2=dat2[match(id,rownames(dat2)),,drop=F]
        qq=cbind(dat1,dat2)
        rownames(qq)=rownames(dat1)
        qq
      }
    })
    names(res)=common.elements
    res
    
  } else stop("Comparison of a list and a data.frame under construction")
}
