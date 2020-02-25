rankNA2 <-
function(x,ties.method="min",largeBetter=F){
    r=rank((-1)^(largeBetter)*x,ties.method=ties.method,na.last="keep")  #xtfrm maybe faster alternative
    if (any(is.na(x))){
        maxrank=ifelse(all(is.na(x)), yes=0, no=max(r,na.rm=TRUE))
        if (ties.method=="min") r[is.na(x)]<-maxrank+1 
        if (ties.method=="average") r[is.na(x)]<-maxrank+mean(1:sum(is.na(x)))
    }
    r
}
