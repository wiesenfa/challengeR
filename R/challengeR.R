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
    missingData = n.missing = list()
    for (task in names(object)) {
      if (!all(is.numeric(object[[task]][[value]]))) stop("Performance values must be numeric.")

      n.missing[[task]] <- sum(is.na(object[[task]][[value]])) # already missing before na.treat; for report
      if (n.missing[[task]]>0) message("Note: ", n.missing, " missing cases have been found in the data set.")
      # check for missing cases
        missingData[[task]]=object[[task]] %>%
          expand(!!as.symbol(algorithm),
                 !!as.symbol(case))%>%
          anti_join(object[[task]],
                    by=c( algorithm,case))
        if (nrow(missingData[[task]])>0) {
             if (length(object) == 1 ) { # single task
            message("Performance of not all algorithms has been observed for all cases. Therefore, missings have been inserted in the following cases:")
          } else { # multi task
            message("Performance of not all algorithms has been observed for all cases in task '",
                    task,
                    "'. Therefore, missings have been inserted in the following cases:")

          }
          print(as.data.frame(missingData[[task]]))
          object[[task]]=as.data.frame(object[[task]] %>%
                                         complete(task,
                                                  !!as.symbol(algorithm),
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
                stop ("The following case(s) appear(s) more than once for the same algorithm. Please revise. ",
                      "Or are you considering a multi-task challenge and forgot to specify argument 'by'?\n",
                      "Case(s): ",
                      paste(names(which(all1!=1)), collapse=", ")
                      )
              } else {
                stop ("The following case(s) appear(s) more than once for the same algorithm. Please revise.\n",
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
        else if (is.character(na.treat) && na.treat=="na.rm") object[[task]]=object[[task]][!is.na(object[[task]][,value]),]
      }
    }
  }
  if (check==TRUE && (any(sapply(missingData, function(x) nrow(x))>0) |any(n.missing>0)))  {
    if (is.null(na.treat)) message("For aggregate-then-rank, na.treat will have to be specified. ",
                                   "For rank-then-aggregate, missings will implicitly lead to the algorithm ranked last for the missing test case."
                               )
    else if (is.numeric(na.treat)) message("All missings have been replaced by the value ", na.treat,".\n")
    else if (is.character(na.treat) && na.treat=="na.rm") message("All missings have been removed.")
    else if (is.function(na.treat)) {
      message("Missings have been replaced using function ")
      print(na.treat)
    }
  }

  if (check==TRUE){
    attr(object,"n.missing")=n.missing
    attr(object,"missingData")=missingData
  }
  attr(object,"na.treat")=na.treat

  attr(object,"algorithm")=algorithm
  attr(object,"value")=value
  attr(object,"case")=case
  attr(object,"annotator")=annotator
  attr(object,"by")=by
  attr(object,"smallBetter")=smallBetter
  attr(object,"check")=check
  class(object)=c("challenge", class(object))
  object
}
