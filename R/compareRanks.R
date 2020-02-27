kendall=function(a,b) cor(a,b,method="kendall")
spearmansWeightedFootrule=function(a,b)  sum(abs(a-b)/pmin(a,b))
spearmansFootrule=function(a,b)  sum(abs(a-b))
# SpearmansFootrule=function(a,b)  sum(abs(match(a, b) - a))
# SpearmansWeightedFootrule=function(a,b)  sum(abs(match(a, b) - a)/pmin(a,b))


compareRanks <- function(x,...) UseMethod("compareRanks")
compareRanks.default <- function(x, ...) stop("not implemented for this class")

compareRanks.ranked <-function(x,
                               y,
                               FUN=kendall,...){
    mat=merge(x$mat,
              y$mat,
              by="row.names",
              suffixes = c(".1",".2"),
              ...)
    tau=FUN(mat$rank.1,
            mat$rank.2)
    res=list(tau=tau,
             mat=mat)
    class(res)="comparedRanks"
    res
  }


 compareRanks.ranked.list <-function(x,
                                     y,
                                     FUN=kendall,...){
    matlist=merge.list(x$matlist,
                       y$matlist
                       ,...)
    res=lapply(1:length(matlist), 
               function(z){
                 tau=FUN(matlist[[z]][,"rank.1"],
                         matlist[[z]][,"rank.2"])
                 res=list(tau=tau,
                          mat=matlist[[z]])
                 class(res)="comparedRanks"
                 res
                 })
    names(res)=names(matlist)
    class(res)="comparedRanks.list"
   res
  }




print.comparedRanks <-
function(x,...)  {
  cat("\n")
  print(x$mat)
  cat("\nKendall's tau =",x$tau,"\n-------------------------------------------------------\n")
 }


