
as.warehouse.challenge=function(x,...){
  x$.ds="data"
  x$.perf="perf"
  form=as.formula(paste(attr(x,"case"),attr(x,"algorithm"),".perf",".ds",sep="~"))
   ar=acast(x,form,value.var = attr(x,"value"))
   
#   ar=acast(dd,case~alg_name~score~subtask,value.var = attr(object,"value"))
  names(dimnames(ar))  =c("samp", "alg" , "perf", "ds")
  w=as.warehouse.array4dim(ar)
  apm <- w$viewAlgorithmPerformance(performances = "perf",datasets="data")
  attr(apm,"challenge")=attributes(x)[-(1:2)]
 apm
}




######
#coppied from archived package benchmark used by as.warehouse.challenge(). deprecate in future versions
as.warehouse.array4dim=
function(x, ...) {
  stopifnot(length(dim(x)) == 4)

  B <- dim(x)[1]
  algorithms <- dimnames(x)[[2]]
  performances <- dimnames(x)[[3]]
  datasets <- dimnames(x)[[4]]

  w <- warehouse(datasets, B,
                 algorithms = algorithms,
                 performances = performances)

  for ( d in length(datasets) )
    w$data[[d]]$AlgorithmPerformance[, , ] <- x[, , , d]

  w
}


warehouse=
function(datasets, B,
         algorithms = NULL,
         performances = NULL,
         characteristics = NULL,
         tests = NULL) {

  if ( length(datasets) != length(B) )
    B <- rep(B, length(datasets))


  a <- mapply(DatasetList, datasets, B,
              MoreArgs = list(algorithms = algorithms,
                              performances = performances,
                              characteristics = characteristics,
                              tests = tests),
              SIMPLIFY = FALSE)
  names(a) <- datasets


  ## Proto object and default data views:
  a <- proto(data = a)

  a$meta <- list(datasets = datasets, B = B,
                 algorithms = algorithms,
                 performances = performances,
                 characteristics = characteristics,
                 tests = tests,
                 algorithm_colors = default_colors(algorithms = algorithms))


  if ( !is.null(algorithms) & !is.null(performances) ) {
    setViewAlgorithmPerformance(a)

    if ( !is.null(tests) )
      setViewTestResult(a)
  }

  if ( !is.null(characteristics) ) {
    setViewDatasetCharacterization(a)
    setViewDatasetBasisCharacterization(a)
  }


  structure(a, class = c("warehouse", class(a)))
}

DatasetList=
  function(dataset, B,
           algorithms = NULL,
           performances = NULL,
           characteristics = NULL,
           tests = NULL) {
    
    a <- list()
    
    if ( !is.null(algorithms) && !is.null(performances) ) {
      a$AlgorithmPerformance <- AlgorithmPerformanceArray(B, algorithms,
                                                          performances)
    }
    
    # if ( !is.null(characteristics) ) {
    #   a$DatasetCharacterization <- DatasetCharacterizationArray(B, characteristics)
    #   a$DatasetBasisCharacterization <- DatasetCharacterizationArray(1, characteristics)
    # }
    
    if ( !is.null(tests) ) {
      a$TestResult <- TestResultArray(B, tests)
    }
    
    
    structure(a, class = c("DatasetList", class(a)),
              dataset = dataset)
  }


default_colors=
  function(n = length(algorithms), algorithms = NULL) {
    # Based on ggplot2:::ScaleHue
    h <- c(0, 360) + 15
    l <- 65
    c <- 100
    
    start <- 1
    direction <- -1
    
    rotate <- function(x) (x + start) %% 360 * direction
    
    if ( (diff(h) %% 360) < 1 ) {
      h[2] <- h[2] - 360 / n
    }
    
    structure(grDevices::hcl(h = rotate(seq(h[1], h[2], length = n)),
                             c = c, l = l),
              names = algorithms)
  }



setViewAlgorithmPerformance=
  function(object) {
    
    object$viewAlgorithmPerformance <- function(.,
                                                datasets = NULL,
                                                algorithms = NULL,
                                                performances = NULL) {
      
      if ( is.null(datasets) )
        datasets <- .$meta$datasets
      
      if ( is.null(algorithms) )
        algorithms <- .$meta$algorithms
      
      if ( is.null(performances) )
        performances <- .$meta$performances
      
      
      view <- lapply(.$data[datasets],
                     function(ds)
                       ds$AlgorithmPerformance[,
                                               algorithms,
                                               performances,
                                               drop = FALSE])
      attr(view, "varname") <- "datasets"
      
      view <- melt(view)
      view$datasets <- as.factor(view$datasets)
      view$samples <- as.factor(view$samples)
      
      view <- view[, c("samples", "datasets", "algorithms", "performances", "value")]
      
      
      structure(view, class = c("AlgorithmPerformance", class(view)),
                algorithm_colors = .$meta$algorithm_colors)
    }
    
    invisible(NULL)
  }



setViewTestResult=
  function(object) {
    
    object$viewTestResult <- function(.,
                                      datasets = NULL,
                                      tests = NULL) {
      
      if ( is.null(datasets) )
        datasets <- .$meta$datasets
      
      if ( is.null(tests) )
        tests <- .$meta$tests
      
      
      view <- lapply(.$data[datasets],
                     function(ds)
                       ds$TestResult[,
                                     tests,
                                     drop = FALSE])
      attr(view, "varname") <- "datasets"
      
      view <- melt(view)
      view$datasets <- as.factor(view$datasets)
      
      view <- view[, c("samples", "datasets", "tests", "value")]
      
      
      structure(view, class = c("TestResult", class(view)))
    }
    
    invisible(NULL)
  }

setViewDatasetCharacterization=
  function(object) {
    
    object$viewDatasetCharacterization <- function(.,
                                                   datasets = NULL,
                                                   characteristics = NULL,
                                                   basis = TRUE) {
      
      if ( is.null(datasets) )
        datasets <- .$meta$datasets
      
      if ( is.null(characteristics) )
        characteristics <- .$meta$characteristics
      
      
      view <- lapply(.$data[datasets],
                     function(ds)
                       ds$DatasetCharacterization[,
                                                  characteristics,
                                                  drop = FALSE])
      attr(view, "varname") <- "datasets"
      
      view <- melt(view)
      view$datasets <- as.factor(view$datasets)
      view$samples <- as.factor(view$samples)
      
      view <- view[, c("samples", "datasets", "characteristics", "value")]
      
      if ( basis ) {
        basis <- .$viewDatasetBasisCharacterization(datasets = datasets,
                                                    characteristics = characteristics)
        basis$samples <- "basis"
        view <- rbind(view, basis)
      }
      
      
      structure(view, class = c("DatasetCharacterization", class(view)))
    }
    
    invisible(NULL)
  }



setViewDatasetBasisCharacterization=
  function(object) {
    
    object$viewDatasetBasisCharacterization <- function(.,
                                                        datasets = NULL,
                                                        characteristics = NULL) {
      
      if ( is.null(datasets) )
        datasets <- .$meta$datasets
      
      if ( is.null(characteristics) )
        characteristics <- .$meta$characteristics
      
      
      view <- lapply(.$data[datasets],
                     function(ds)
                       ds$DatasetBasisCharacterization[,
                                                       characteristics,
                                                       drop = FALSE])
      attr(view, "varname") <- "datasets"
      
      view <- melt(view)
      view$datasets <- as.factor(view$datasets)
      
      view <- view[, c("datasets", "characteristics", "value")]
      
      
      structure(view, class = c("DatasetBasisCharacterization", class(view)))
    }
    
    invisible(NULL)
  }


AlgorithmPerformanceArray=
function(B, algorithms, performances) {
  WarehouseArray(B, algorithms = algorithms, performances = performances,
                 class = "AlgorithmPerformanceArray")
}

WarehouseArray=
function(B, ..., class) {
  d <- list(...)
  
  dim <- c(B, sapply(d, length))
  dimnames <- c(list(samples = NULL), d)
  
  a <- array(NA_integer_, dim = dim, dimnames = dimnames)
  
  structure(a, class = c(class, class(a)))
}

