splitby <-
function(x,by){
    if (length(by)==1) split(x,x[,by])
    else split(x,as.list(x[,by]))
  }
