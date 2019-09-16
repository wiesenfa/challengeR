Analyzing assessment data of biomedical image analysis competitions and
visualization of results
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation

<!-- To get the current released version from CRAN: -->

<!-- ```{r} -->

<!-- ## install challengeR from CRAN -->

<!-- install.packages("challengeR") -->

<!-- ``` -->

To get the current development version of the R package from
Github:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

BiocManager::install("Rgraphviz")

devtools::install_github("wiesenfa/challengeR", dependencies = TRUE)
```

# Usage

## Setup

Load
package

``` r
library(challengeR)
```

<!-- # Visualization of the impact of priors in terms of effective sample size -->

<!-- This section (same as `vignette("vignettePlanning", package = "ESS")`)  visualizes how `plotECSS()` can be used to quantify the impact of a prior (including mixture priors and empirical Bayes power priors/commensurate priors) in terms of effective current sample sizes on a grid of true values of the data generating process. -->

Load data set e.g. in csv format

``` r
data_matrix=read.csv(file) # see ?read.csv for help
```

In the following use simulated data instead

``` r
n=50

set.seed(4)
strip=runif(n,.9,1)
ideal=cbind(task="ideal",
            rbind(
              data.frame(alg_name="A1",value=runif(n,.9,1),case=1:n),
              data.frame(alg_name="A2",value=runif(n,.8,.89),case=1:n),
              data.frame(alg_name="A3",value=runif(n,.7,.79),case=1:n),
              data.frame(alg_name="A4",value=runif(n,.6,.69),case=1:n),
              data.frame(alg_name="A5",value=runif(n,.5,.59),case=1:n)
            ))

set.seed(1)
fullyRandom=data.frame(task="fullyRandom",
                       alg_name=factor(paste0("A",rep(1:5,each=n))),
                       value=plogis(rnorm(5*n,1.5,1)),case=rep(1:n,times=5)
                       )

strip2=seq(.8,1,length.out=5)
a=permute::allPerms(1:5)
worstCase=data.frame(task="worstCase",
                     alg_name=c(t(a)),
                     value=rep(strip2,nrow(a)),
                     case=rep(1:nrow(a),each=5)
                     )
worstCase=rbind(worstCase,
                data.frame(task="worstCase",alg_name=1:5,value=strip2,case=max(worstCase$case)+1)
          )
worstCase$alg_name=factor(worstCase$alg_name,labels=paste0("A",1:5))

data_matrix=rbind(ideal, fullyRandom, worstCase)
```

## Analysis and report for a single task

``` r
# Use only task "fullyRandom" in object data_matrix
  dataSubset=subset(data_matrix, task=="fullyRandom")

  challenge_single=as.challenge(dataSubset, 
                                # Specify which column contains the algorithm, 
                                # which column contains a test case identifier 
                                # and which contains the metric value
                                algorithm="alg_name", case="case", value="value", 
                                # Specify if small metric values are better
                                smallBetter = FALSE)

# Perform ranking using aggregateThenRank(), rankThenAggregate() or testThenRank(), 
# e.g. use "mean-then-rank"
  object=challenge_single%>%aggregateThenRank(FUN = mean, # aggregation function, 
                                                          # e.g. mean, median, min, max, 
                                                          # or e.g. function(x) quantile(x, probs=0.05)
                                              na.treat=0, # either "na.rm" to remove missing data, 
                                                          # set missings to numeric value (e.g. 0) 
                                                          # or specify a function, 
                                                          # e.g. function(x) min(x)
                                              ties.method = "min" # a character string specifying 
                                                                  # how ties are treated, see ?base::rank
                                              )  
  object
  # same as
  #  object=challenge_single %>% aggregate(FUN = mean, na.treat=0) %>% rank(ties.method = "min")
  # similarly for rankThenAggregate(),
  #  object=challenge_single %>% rank(ties.method = "min") %>% 
  #                      aggregate(FUN = mean) %>% rank(ties.method = "min")
  
# Perform bootstrapping
  set.seed(1)
  boot_object=object%>%bootstrap(nboot=1000)

# generate report
  report(boot_object, 
         file = "filename.pdf", format = "PDF") # format can be "PDF", "HTML" or "Word"
```

## Analysis and report for multiple tasks

``` r
# Same as above but with 'by="task"' where variable "task" contains the task identifier
  challenge_multi=as.challenge(data_matrix, 
                               by="task", 
                               algorithm="alg_name", case="case", value="value", 
                               smallBetter = FALSE)


# Perform ranking as above, e.g. use test-then-rank based on Wilcoxon signed rank test
  object=challenge_multi%>%testThenRank(alpha=0.05, # significance level
                                        p.adjust.method="none",  # method for adjustment for 
                                                                 # multiple testing, see ?p.adjust
                                        na.treat=0, # either "na.rm" to remove missing data, 
                                                    # set missings to numeric value (e.g. 0) 
                                                    # or specify a function, e.g. function(x) min(x)
                                        ties.method = "min" # a character string specifying 
                                                            # how ties are treated, see ?base::rank
                             )
  object

# Perform bootstrapping using multiple CPUs
  library(doParallel)
  registerDoParallel(cores=8)  
  set.seed(1)
  boot_object=object%>%bootstrap(nboot=1000, parallel=TRUE, progress = "none")
  stopImplicitCluster()


# Compute ranking consensus across tasks
  # E.g. consensus ranking according to mean ranks across tasks. 
  # See ?relation_consensus for different methods to derive consensus ranking
  meanRanks=object%>%consensus(method = "euclidean") 
  meanRanks # note that there are ties (i.e. some algorithms have identical mean rank)

# generate report as above, but with additional specification of consensus ranking
  report(boot_object, 
         consensus=names(meanRanks),
         file = "filename.pdf", 
         format = "PDF")
```

# Reference

<!-- Wiesenfarth, M., Maier-Hein, L., Reinke, A., Kopp-Schneider, A.. Challenge Visualization  -->

<!-- *Journal*. -->
