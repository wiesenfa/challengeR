subset <- function(x,...) UseMethod("subset")
subset.default <- function(x, ...) base::subset(x, ...)


subset.comparedRanks.list=function(x,
                                   tasks,...){
  res=x[tasks]
  class(res)="comparedRanks.list"
  res
}

subset.list=function(x,
                     tasks,...){
  x[tasks]
}

subset.aggregated.list=function(x,
                                tasks,...){
  call=match.call(expand.dots = T)
  if (!is.null(as.list(call$top))) stop("Subset of algorithms only sensible for single task challenges.")
  matlist=x$matlist[tasks]
  res=list(matlist=matlist,
           call=list(x$call,call),
           data=x$data,
           FUN =  . %>% (x$FUN) %>%  (call)
  )

  class(res)=class(x)
  res

}

which.top=function(object,
                   top){
  mat=object$mat[object$mat$rank<=top,]
  rownames(mat)#[order(mat$rank)]
}

#' Extracts a subset of algorithms or tasks
#'
#' Extracts the top performing algorithms or a subset of tasks.
#'
#' @section Reports for subsets (top list) of algorithms:
#' If ties are present in the ranking, the subset will consist of more than \code{top} algorithms.
#' Line plots for ranking robustness can be used to check whether algorithms performing well in other
#' ranking methods are excluded. Bootstrapping still takes entire uncertainty into account.
#' Podium plots and ranking heatmaps neglect excluded algorithms. Only available for single-task challenges
#' (for multi-task challenges not sensible because each task would contain a different set of algorithms).
#'
#' @section Reports for subsets of tasks:
#' You may want to recompute the consensus ranking after creating the subset. An error will be raised
#' if a task identifier is not contained in the assessment data set to avoid subsequent errors.
#'
#'
#' @param x The ranked asssessment data set.
#' @param top A positive integer specifying the amount of top performing algorithms to be retrieved.
#' @param tasks A vector of strings containing the task identifiers that should remain in the subset.
#' @param ... Further arguments passed to or from other functions.
#'
#' @return An S3 object of class "ranked.list" to represent a ranked assessment data set.
#'
#' @examples
#'
#' \dontrun{
#'  # only show the top 3 algorithms according to the chosen ranking method
#'  subset(ranking, top = 3) %>% report(...)
#' }
#'
#' \dontrun{
#'  # restrict report to tasks "task1" and "task2"
#'  subset(ranking, tasks=c("task1", "task2")) %>% report(...)
#' }
#'
#' @export
subset.ranked.list <- function(x,
                               top,
                               tasks,...) {

  if (!missing(top) & length(x$matlist) != 1)  stop("Subset of algorithms only sensible for single-task challenges. Otherwise no consensus ranking is possible.")

  if (!missing(top)){
    taskMat <- x$matlist[[1]]
    taskData <- x$data[[1]]
    objectTop=x
    objectTop$matlist[[1]]=taskMat[taskMat$rank<=top,]

    taskMatRowNames <- rownames(objectTop$matlist[[1]])
    attribute <- attr(objectTop$data,"algorithm")

    selectedRowNames <- taskData[[attribute]] %in% taskMatRowNames
    objectTop$data[[1]] <- taskData[selectedRowNames,]
    if (is.factor(objectTop$data[[1]][[attribute]])) objectTop$data[[1]][[attribute]] <- droplevels(objectTop$data[[1]][[attribute]])

    objectTop$fulldata=x$data
    return(objectTop)
  } else if (!missing(tasks)){

    if (is.character(tasks) && any(!tasks%in%names(x$matlist))) {
      stop("There is/are no task(s) called ",paste(tasks[!tasks%in%names(x$matlist)],collapse = " and "),".")
    }
    res=list(matlist=x$matlist[tasks],
             data=x$data[tasks],
             call=x$call,
             FUN=x$FUN,
             FUN.list=x$FUN.list
    )

    attrib=attributes(x$data)
    attrib$names=attr(res$data,"names")
    attributes(res$data)=attrib
    class(res)=c("ranked.list","list")
    return(res)
  }
}


#' Extracts a subset of algorithms or tasks
#'
#' Extracts the top performing algorithms or a subset of tasks.
#'
#' @section Reports for subsets (top list) of algorithms:
#' If ties are present in the ranking, the subset will consist of more than \code{top} algorithms.
#' Line plots for ranking robustness can be used to check whether algorithms performing well in other
#' ranking methods are excluded. Bootstrapping still takes entire uncertainty into account.
#' Podium plots and ranking heatmaps neglect excluded algorithms. Only available for single-task challenges
#' (for multi-task challenges not sensible because each task would contain a different set of algorithms).
#'
#' @section Reports for subsets of tasks:
#' You may want to recompute the consensus ranking after creating the subset. An error will be raised
#' if a task identifier is not contained in the assessment data set to avoid subsequent errors.
#'
#'
#' @param x The bootstrapped, ranked asssessment data set.
#' @param top A positive integer specifying the amount of top performing algorithms to be retrieved.
#' @param tasks A vector of strings containing the task identifiers that should remain in the subset.
#' @param ... Further arguments passed to or from other functions.
#'
#' @return An S3 object of class "bootstrap.list" to represent a bootstrapped, ranked assessment data set.
#'
#' @examples
#'
#' \dontrun{
#'  # only show the top 3 algorithms according to the chosen ranking method
#'  subset(ranking_bootstrapped, top = 3) %>% report(...)
#' }
#'
#' \dontrun{
#'  # restrict report to tasks "task1" and "task2" and recompute consensus ranking
#'  meanRanks <- subset(ranking, tasks = c("task1", "task2")) %>% consensus(method = "euclidean")
#' }
#'
#' @export
subset.bootstrap.list=function(x,
                               top,
                               tasks, ...) {

  if (!missing(top) & length(x$matlist) != 1)  stop("Subset of algorithms only sensible for single-task challenges. Otherwise no consensus ranking is possible.")

  if (!missing(top)){
    objectTop <- subset.ranked.list(x, top = top)

    objectTop$bootsrappedRanks[[1]] <- objectTop$bootsrappedRanks[[1]][rownames(objectTop$matlist[[1]]),]
    objectTop$bootsrappedAggregate[[1]] <- objectTop$bootsrappedAggregate[[1]][rownames(objectTop$matlist[[1]]),]
    return(objectTop)
  } else if (!missing(tasks)){
    if (is.character(tasks) && any(!tasks%in%names(x$matlist))) {
      stop("There is/are no task(s) called ",paste(tasks[!tasks%in%names(x$matlist)],collapse = " and "),".")
    }

    res=list(bootsrappedRanks=x$bootsrappedRanks[tasks],
             bootsrappedAggregate=x$bootsrappedAggregate[tasks],
             matlist=x$matlist[tasks],
             data=x$data[tasks],
             FUN=x$FUN
    )

    attrib=attributes(x$data)
    attrib$names=attr(res$data,"names")
    attributes(res$data)=attrib
    class(res)="bootstrap.list"
    return(res)
  }
}
