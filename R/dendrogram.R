dendrogram <- function(object,...) UseMethod("dendrogram")
dendrogram.default <- function(object, ...) stop("not implemented for this class")

dendrogram.ranked.list <- function(object,
                                   dist = "symdiff", #the distance measure to be used. see ?relation_dissimilarity
                                   method = "complete", #the agglomeration method to be used. see ?hclust
                                   ... # arguments passed to stats:::plot.hclust
                                   ){
  relensemble=as.relation.ranked.list(object)
  d <- relation_dissimilarity(relensemble, 
                              method = dist)
  clust <- hclust(d,
                  method=method)
  dots <- match.call(expand.dots = FALSE)$...
  if (is.null(dots$xlab)) dots$xlab <- ""
  if (is.null(dots$sub)) dots$sub <- ""
  if (is.null(dots$main)) dots$main <- paste0("Cluster Dendrogram (", method, " agglomeration)")

  do.call(plot,
          c(list(x = clust), dots) )
    invisible(list(dist = d,
                   hclust = clust
                   ))

}
