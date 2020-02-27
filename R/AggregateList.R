
# FUNname=capture.output(get_expr(FUN))[2]
# FUNname=sub("UseMethod(","",FUNname,fixed = T)

# depracate?
aggregateList=function(object,
                       x,
                       FUN=mean){
  if (is.character(FUN)) {
      FUNname=paste0(x,"_",FUN)
      FUN=try(eval(parse(text=FUN)),silent = T)
    } else FUNname=paste0(x,"_aggregated")
    
  melt(object)%>%
    filter(variable==x)%>%
    group_by(algorithm)%>%
    summarise(!!FUNname := FUN(value))%>% 
    as.data.frame
}



