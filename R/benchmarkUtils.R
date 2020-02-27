# to link with benchmark package (CRAN archived)

as.warehouse.challenge=function(x,...){
  x$.ds="data"
  x$.perf="perf"
  form=as.formula(paste(attr(x,"case"),attr(x,"algorithm"),".perf",".ds",sep="~"))
   ar=acast(x,form,value.var = attr(x,"value"))
   
#   ar=acast(dd,case~alg_name~score~subtask,value.var = attr(object,"value"))
  names(dimnames(ar))  =c("samp", "alg" , "perf", "ds")
  w=benchmark::as.warehouse.array4dim(ar)
  apm <- w$viewAlgorithmPerformance(performances = "perf",datasets="data")
  attr(apm,"challenge")=attributes(x)[-(1:2)]
 apm
}


