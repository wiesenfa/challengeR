rankNA2 <-
function(x,ties.method="min",inverseOrder=F){
    r=rank((-1)^(inverseOrder)*x,ties.method=ties.method,na.last="keep")  #xtfrm maybe faster alternative
    #r[is.na(x)]<-sum(r[is.na(x)])/sum(is.na(x))
    r[is.na(x)]<-max(r)+1 
    r
}
