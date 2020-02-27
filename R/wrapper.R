aggregateThenRank=function(object,FUN,ties.method = "min",...){
  object %>% 
    aggregate(FUN=FUN,...) %>% 
    rank(ties.method = ties.method)
}

testThenRank=function(object,FUN,ties.method = "min",...){
  object %>% 
    aggregate(FUN="significance",...) %>% 
    rank(ties.method = ties.method)
}

rankThenAggregate=function(object,
                           FUN,
                           ties.method = "min"
                           ){
  object %>% 
        rank(ties.method = ties.method)%>% 
          aggregate(FUN=FUN) %>% 
          rank(ties.method = ties.method) #small rank is always best, i.e. largeBetter always FALSE
}


