rankNA2 <-
function(x,ties.method="min",inverseOrder=F){
    r=rank((-1)^(inverseOrder)*x,ties.method=ties.method,na.last="keep")  #xtfrm maybe faster alternative
    #r[is.na(x)]<-sum(r[is.na(x)])/sum(is.na(x))
    if (ties.method=="min") r[is.na(x)]<-max(r,na.rm=TRUE)+1 
    if (ties.method=="average") r[is.na(x)]<-max(r,na.rm=TRUE)+mean(1:sum(is.na(x)))
    r
}
