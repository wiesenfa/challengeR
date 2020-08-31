#' Title
#'
#' @param object
#' @param value
#' @param algorithm
#' @param case
#' @param taskName Optional for single-task data set that does not contain a task column.
#' @param by The name of the column that contains the task identifiers. Required for multi-task data set.
#' @param annotator
#' @param smallBetter
#' @param na.treat
#' @param check
#'
#' @return
#' @export
#'
#' @examples
as.challenge=function(object,
                      value,
                      algorithm ,
                      case=NULL,
                      taskName=NULL,
                      by=NULL,
                      annotator=NULL,
                      smallBetter=FALSE,
                      na.treat=NULL, # optional
                      check=TRUE) {

  object=as.data.frame(object[,c(value, algorithm, case, by, annotator)])

  # sanity checks
  if (check) {

    if (!is.null(by) && !is.null(taskName)) {
      warning("Argument 'taskName' is ignored for multi-task data set.")
    }

    # Add task column for data set without task column by using the specified task name.
    if (is.null(by) && !is.null(taskName)) {
      taskName <- trimws(taskName)

      if (taskName == "") {
        stop("Argument 'taskName' is empty.")
      }

      object <- cbind(task=taskName, object)
      by = "task"
    }

    # Add task column for data set without task column by using a dummy task name.
    if (is.null(by) && is.null(taskName)) {
      object <- cbind(task="dummyTask", object)
      by = "task"
    }

    object=splitby(object,by=by)
    object=lapply(object,droplevels)
    for (task in names(object)) {
      # check for missing cases
        missingData=object[[task]] %>%
          expand(!!as.symbol(algorithm),
                 !!as.symbol(case))%>%
          anti_join(object[[task]],
                    by=c( algorithm,case))
        if (nrow(missingData)>0) {
          if (length(object) == 1 ) { # single task 
            message("Performance of not all algorithms is observed for all cases. Inserted as missings in following cases:")
          } else { # multi task
            message("Performance of not all algorithms is observed for all cases in task '",
                    task,
                    "'. Inserted as missings in following cases:")
            
          }
          print(as.data.frame(missingData))
          object[[task]]=as.data.frame(object[[task]] %>%
                                         complete(!!as.symbol(algorithm),
                                                  !!as.symbol(case)))
        }
      # check duplicate cases
         all1=apply(table(object[[task]][[algorithm]],
                           object[[task]][[case]]),
                     2,
                     function(x) all(x==1))
          if (!all(all1)) {
            n.duplicated <- sum(all1!=1)
            
            if (length(object) == 1 ) { # single task 
              if (n.duplicated/length(all1) >= 1/5) { # at least a quarter of the cases is duplicated
                stop ("The following cases appear more than once. Please revise. ",  
                      "Or are you considering a multi-task challenge and forgot to specify argument 'by'?\n",
                      "Cases: ",
                      paste(names(which(all1!=1)), collapse=", ")
                      )
              } else {
                stop ("The following case(s) appear(s) more than once. Please revise.\n",  
                      "Case(s): ",
                      paste(names(which(all1!=1)), collapse=", ")
                      )
              }
            } else { # multi task
              stop ("The following case(s) appear(s) more than once for the same algorithm in task '",
                    task, "'. Please revise.\n",
                     "Case(s): ",
                    paste(names(which(all1!=1)), collapse=", ")
                    )

            }
          }

      if (!is.null(na.treat)) {
        if (is.numeric(na.treat)) object[[task]][,value][is.na(object[[task]][,value])]=na.treat
        else if (is.function(na.treat)) object[[task]][,value][is.na(object[[task]][,value])]=na.treat(object[[task]][,value][is.na(object[[task]][,value])])
        else if (na.treat=="na.rm") object[[task]]=object[[task]][!is.na(object[[task]][,value]),]
      }
    }
  }

  attr(object,"algorithm")=algorithm
  attr(object,"value")=value
  attr(object,"case")=case
  attr(object,"annotator")=annotator
  attr(object,"by")=by
  attr(object,"largeBetter")=!smallBetter
  attr(object,"check")=check
  class(object)=c("challenge", class(object))
  object
}
