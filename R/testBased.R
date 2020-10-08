decision <- function(x,...) UseMethod("decision")
decision.default <- function(x, ...) stop("not implemented for this class")

decision.challenge=function(x,
                            na.treat=NULL, #  it can be 'na.rm', numeric value or function
                            alpha=0.05,
                            p.adjust.method="none",
                            alternative="one.sided",
                            test.fun=function(x,y) wilcox.test(x,y,
                                                               alternative = alternative,exact=FALSE,
                                                               paired = TRUE)$p.value,
                            parallel=FALSE,
                            progress="none",...){

  if (is.null(na.treat)){ #na.treat only optional if no missing values in data set
    if (!inherits(x,"list")){
      if (!any(is.na(x[,attr(x, "value")]))) na.treat="na.rm" # there are no missings so set na.treat by dummy "na.rm" has no effect
      else stop("Please specify na.treat in as.challenge()")
    } else {
      if (!any(sapply(x,
                      function(task) any(is.na(task[,attr(x, "value")]))))) na.treat="na.rm" # there are no missings so set na.treat by dummy "na.rm" has no effect
      else stop("Please specify na.treat in as.challenge()")
    }
  }


  if (alternative!="two.sided") alternative=ifelse(!attr(x,"smallBetter"),
                                                   yes="greater",
                                                   no="less")
  call=match.call(expand.dots = T)

  object=x
  algorithm=attr(object,"algorithm")
  case=attr(object,"case")
  value=attr(object,"value")
  smallBetter=attr(object,"smallBetter")
  if(missing(case)| missing(smallBetter)) stop("arguments case and alpha need to be given in as.challenge()")


  if (inherits(object,"list")){
    matlist=llply(1:length(object),
                  function(id){
                    piece=object[[id]]
                    if (length(unique(piece[[algorithm]]))<=1){
                      warning("only one ", algorithm, " available in element ", names(object)[id])
                    }
                    if (is.numeric(na.treat)) piece[,value][is.na(piece[,value])]=na.treat
                    else if (is.function(na.treat)) piece[,value][is.na(piece[,value])]=na.treat(piece[,value][is.na(piece[,value])])
                    else if (na.treat=="na.rm") piece=piece[!is.na(piece[,value]),]
                    mat=Decision(piece, value, algorithm, case, alpha, smallBetter,
                                 p.adjust.method=p.adjust.method,
                                 alternative=alternative,
                                 test.fun=test.fun)
                    mat=as.data.frame(mat)
                    mat[is.na(mat)]=0
                    mat=as.matrix(mat)
                    class(mat)=c(class(mat),"challenge.incidence")
                    mat

                  },
                  .parallel=parallel,
                  .progress=progress )
    names(matlist)=names(object)
    return(matlist)
  } else {
    if (length(unique(object[[algorithm]]))<=1){
      warning("only one ", algorithm, " available")
      matlist=(matrix(NA,1,1))
    } else {
      mat=Decision(object,
                   value,
                   algorithm,
                   case,
                   alpha,
                   smallBetter,
                   p.adjust.method=p.adjust.method,
                   alternative=alternative,
                   test.fun=test.fun)
    }
    mat=as.data.frame(mat)
    mat[is.na(mat)]=0
    mat=as.matrix(mat)
    class(mat)=c(class(mat),"challenge.incidence")
    return(mat)

  }
}


Decision=function(object,
                  value,
                  by,
                  case,
                  alpha,
                  smallBetter=TRUE,
                  p.adjust.method="none",
                  alternative="one.sided",
                  test.fun=function(x,y) wilcox.test(x,y,
                                                     alternative = alternative,exact=FALSE,
                                                     paired = TRUE)$p.value){
  algorithms=unique(object[[by]])
  if (length(unique(object[[case]]))==1){
    warning("Only one case in task.")
  }

  combinations=expand.grid(algorithms,algorithms)[,2:1]
  combinations=combinations[apply(combinations,1,function(z) anyDuplicated(z)==0),] # remove i==j

  pvalues=sapply(1:nrow(combinations), function(it){
    dat1=object[object[[by]]==combinations[it,1],]
    dat2=object[object[[by]]==combinations[it,2],]
    id=intersect(dat2[,case],dat1[,case])
    dat1=dat1[match(id,dat1[,case]),value]
    dat2=dat2[match(id,dat2[,case]),value]
    test.fun(dat1,dat2)

  })
  decisions=as.numeric(p.adjust(pvalues,
                                method=p.adjust.method)<= alpha)
  res=cbind(combinations,decisions)
  reshape2::acast(res,
                  Var2~Var1,
                  value.var="decisions")
}


as.relation.challenge.incidence=function(x,
                                         verbose = FALSE, ...) {
  r <- relation(incidence = x, ...)

  props <- check_strict_preference(r)
  class <- "strictpref"
  r$.Meta$is_decreasing <- FALSE

  r$.Meta <- c(r$.Meta,
               structure(props, names = sprintf("is_%s", names(props))))

  if ( verbose ) {
    for ( p in names(props) ) {
      cat(sprintf("%s = %s:\n", p, props[[p]]))
      print(relation_violations(r, p, TRUE))
    }
  }

  structure(r, class = c(class, class(r)))
}

check_strict_preference=
function(x) {
  list(irreflexive = relation_is_irreflexive(x),
       asymmetric = relation_is_asymmetric(x),
       transitive = relation_is_transitive(x),
       negatively_transitive = relation_is_negatively_transitive(x),
       trichotomous = relation_is_trichotomous(x))
}


significance=function(object,
                      value,
                      algorithm,
                      case,
                      alpha,
                      smallBetter=TRUE,...) {

  xx=as.challenge(object,
                  value=value,
                  algorithm=algorithm,
                  case=case,
                  smallBetter = smallBetter,
                  check=FALSE)
  a=decision.challenge(xx, alpha=alpha, ...)
  prop_significance=rowSums(a)/(ncol(a)-1)
  return(data.frame("prop_significance"=prop_significance,
                    row.names = names(prop_significance)))
}
