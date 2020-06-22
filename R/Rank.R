Rank <- function(object,...) UseMethod("Rank")
Rank.default <- function(object, ...) rank(object,...)  #base::rank

Rank.list <- function(object,
                      x,
                      annotator,
                      ties.method="min",
                      largeBetter=FALSE,
                      ...){

    call=match.call(expand.dots = T)
    annotator.missing=missing(annotator)
    if (any(sapply(object,
                   function(task) {
                     (attr(object,"check") &&
                      largeBetter &&
                      any(is.na(task[[x]])) &&
                      min(task[[x]], na.rm=TRUE)==0)
                  })
            )) {
        message("There are missing metric values and metric values exactly equal to zero.
                 Have some actually missing values been entered as zero in some instances?
                 If yes, specify optional argument na.treat=0 in as.challenge().")
    }

    matlist=lapply(object,
                   function(task){
                     if (annotator.missing){
                       res=bind_rows(
                         lapply(split(task,
                                      task[[attr(object,"case")]]),
                                function(task.case)
                                  cbind(task.case,
                                        rank=rankNA2(task.case[[x]],
                                                     ties.method = ties.method,
                                                     largeBetter = largeBetter)
                                        )
                                )
                         )
                        class(res)[2]="ranked"
                        res
                       } else {
                         byAnnotator=split(task,
                                           as.list(task[,annotator]))
                         temp=bind_rows(
                           lapply(byAnnotator,
                                  function(annotator){
                                    bind_rows(
                                      lapply(split(annotator,
                                                   annotator[[attr(object,"case")]]),
                                             function(annotator.case)
                                               cbind(annotator.case,
                                                     rank=rankNA2(annotator.case[[x]],
                                                                  ties.method = ties.method,
                                                                  largeBetter = largeBetter)
                                                     )
                                             )
                                    )
                                    }
                                  )
                           )
                         class(temp)[2]="ranked"
                         temp
                         }
                     }
                   )
    res=list(FUN = . %>% (call),
             call=list(call),
             data=object,
             matlist=matlist)

    class(res)=c("ranked.list",class(res))
    res
}
