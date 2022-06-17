# Copyright (c) German Cancer Research Center (DKFZ)
# All rights reserved.
#
# This file is part of challengeR.
#
# challengeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# challengeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with challengeR. If not, see <https://www.gnu.org/licenses/>.

#' Constructs a challenge object
#'
#' Constructs an S3 object to represent the configuration of an assessment data set originating from a benchmarking competition (so-called "challenge").
#'
#' @section Assessment data set:
#' The toolkit provides visualization approaches for both challenges designed around a single task (single-task challenges) and for challenges comprising multiple tasks (multi-task challenges).
#' For a single-task challenge, the assessment data set (argument \code{object}) requires the following columns:
#' \itemize{
#'   \item test case identifier (string or numeric)
#'   \item algorithm identifier (string or numeric)
#'   \item performance value (numeric)
#' }
#'
#' For a multi-task challenge, the assessment data set (argument \code{object}) requires the following columns:
#' \itemize{
#'   \item task identifier (string or numeric)
#'   \item test case identifier (string or numeric)
#'   \item algorithm identifier (string or numeric)
#'   \item performance value (numeric)
#' }
#'
#' @section Sanity check:
#' It is highly recommended that the sanity check is not disabled when the data set is provided initially.
#' It checks that:
#' \itemize{
#'   \item performance values are numeric (if not, raises error)
#'   \item algorithm performances are observed for all cases (if not, adds them as NA and emits a message)
#'   \item cases appear only once for the same algorithm (if not, raises error)
#' }
#' If the argument \code{na.treat} for treatment of NA is specified, NAs will be handled respectively.
#'
#' It might be reasonable to disable the sanity check for further computations (e.g., for performance reasons
#' during bootstrapping (\code{\link{bootstrap.ranked.list}}) where cases are actually allowed to appear more than once for the same algorithm).
#'
#' @param object A data frame containing the assessment data.
#' @param case A string specifying the name of the column that contains the case identifiers.
#' @param algorithm A string specifying the name of the column that contains the algorithm identifiers.
#' @param value A string specifying the name of the column that contains the performance values.
#' @param by A string specifying the name of the column that contains the task identifiers. Required for multi-task data set.
#' @param taskName A string specifying the task name for single-task data set that does not contain a task column.
#'   This argument is optional for a single-task data set and is ignored for a multi-task data set.
#' @param annotator If multiple annotators annotated the test cases, a string specifying the name of the column that contains the annotator identifiers. Only applies to rang-then-aggregate. Use with caution: Currently not tested.
#' @param smallBetter A boolean specifying whether small performance values indicate better algorithm performance.
#' @param na.treat Indicates how missing perfomance values are treated if sanity check is enabled. It can be 'na.rm', numeric value or function.
#'   For a numeric value or function, NAs will be replaced by the specified values. For 'na.rm', rows that contain missing values will be removed.
#' @param check A boolean to indicate to perform a sanity check of the specified data set and arguments if set to \code{TRUE}.
#'
#' @return An S3 object to represent the configuration of an assessment data set.
#'
#' @examples
#' # single-task data set
#'
#' # multi-task data set
#'
#' @export
as.challenge=function(object,
                      case,
                      algorithm,
                      value,
                      by=NULL,
                      taskName=NULL,
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
            message("Performance of not all algorithms has been observed for all cases.\nTherefore, missings have been inserted in the following cases:")
          } else { # multi task
            message("Performance of not all algorithms has been observed for all cases in task '",
                    task,
                    "'.\nTherefore, missings have been inserted in the following cases:")

          }
          print(as.data.frame(missingData[[task]]))
          object[[task]]=as.data.frame(object[[task]] %>%
                                         complete(!!as.symbol(by),
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

  if (check==TRUE && (any(sapply(missingData, function(x) nrow(x))>0) | any(n.missing>0)))  {
    ##
    ## The message below was disabled because it can cause misinformation even we supply na.treat to as.challenge object
    ##
    # if (is.null(na.treat)) message("For aggregate-then-rank, na.treat will have to be specified. ",
    #                                "For rank-then-aggregate, missings will implicitly lead to the algorithm ranked last for the missing test case.",
    #                                "na.treat obligatory if report is intended to be compiled."
    #                            )
    if (is.numeric(na.treat)) message("All missings have been replaced by the value ", na.treat,".\n")
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
