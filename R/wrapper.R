# AggregateThenRank=function(object,x,algorithm,FUN,ties.method = "average",inverseOrder=FALSE){
#   object %>% 
#     Aggregate(x=x,algorithm=algorithm,FUN=FUN) %>% 
#     Rank(ties.method = ties.method,inverseOrder=inverseOrder)
# }
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

# bei Aggregate könnte x und algorithm in formel "metric~alogrithm"
# RankThenAggregate=function(object,x,algorithm,by,FUN,ties.method = "average",inverseOrder=FALSE){
#   object %>% 
#         Rank(x=x,by=by,ties.method = ties.method,inverseOrder=inverseOrder)%>% 
#           Aggregate(algorithm=algorithm,FUN=FUN) %>% 
#           Rank(ties.method = ties.method,inverseOrder=FALSE) #small rank is always best, i.e. inverseOrder always FALSE
# }
rankThenAggregate=function(object,#x,algorithm,by,
                           FUN,ties.method = "min"#,inverseOrder=FALSE
                           ){
  # object %>% 
  #       Rank(x=x,by=by,ties.method = ties.method,inverseOrder=inverseOrder)%>% 
  #         Aggregate(algorithm=algorithm,FUN=FUN) %>% 
  #         Rank(ties.method = ties.method,inverseOrder=FALSE) #small rank is always best, i.e. inverseOrder always FALSE
  object %>% 
        rank(ties.method = ties.method)%>% 
          aggregate(FUN=FUN) %>% 
          rank(ties.method = ties.method) #small rank is always best, i.e. inverseOrder always FALSE
}


  # a12=  Data%>% subset(metric=="DICE") %>% subset(task_id_n==1) %>% 
  #         Rank(x="metric_value",by=c("annotator_id", "dataset_id"),ties.method = "average",inverseOrder=T)%>% 
  #         Aggregate(by="algorithm_id",FUN=mean) %>% 
  #         Rank(ties.method = "average",inverseOrder=F)
