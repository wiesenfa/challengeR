Methods and open-source toolkit for analyzing and visualizing challenge
results
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

Note that this is an early experimental version (version 0.2.4).

This is ongoing work, there may be updates with possibly major changes.
*Please make sure that you use the most current version\!*

Change log at the end of this document.

# Installation

<!-- To get the current released version from CRAN: -->

<!-- ```{r} -->

<!-- ## install challengeR from CRAN -->

<!-- install.packages("challengeR") -->

<!-- ``` -->

Requires R version \>= 3.5.2 (<https://www.r-project.org>).

Further, a recent version of Pandoc (\>= 1.12.3) is required. RStudio
(<https://rstudio.com>) automatically includes this so you do not need
to download Pandoc if you plan to use rmarkdown from the RStudio IDE,
otherwise you’ll need to install Pandoc for your platform
(<https://pandoc.org/installing.html>). Finally, if you want to generate
a pdf report you will need to have LaTeX installed (e.g. MiKTeX, MacTeX
or TinyTeX).

To get the current development (experimental) version of the R package
from
Github:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("Rgraphviz", dependencies = TRUE)
devtools::install_github("wiesenfa/challengeR", dependencies = TRUE)
```

If you are asked whether you want to update installed packages and you
type “a” for all, you might need administrator rights to update R core
packages. You can also try to type “n” for updating no packages. If you
are asked “Do you want to install from sources the packages which need
compilation? (Yes/no/cancel)”, you can safely type “no”.

If you get *Warning messages* (in contrast to *Error* messages), these
might not be problematic and you can try to proceed.

# Terms of use

Licenced under GPL-3. If you use this software for a publication, cite

Wiesenfarth, M., Reinke, A., Landmann A.L., Cardoso, M.J., Maier-Hein,
L. and Kopp-Schneider, A. (2019). Methods and open-source toolkit for
analyzing and visualizing challenge results. *arXiv preprint
arXiv:1910.05121*

# Usage

Each of the following steps have to be run to generate the report: (1)
Load package, (2) load data, (3) perform ranking, (4) perform
bootstrapping and (5) generation of the report

## 1\. Load package

Load
package

``` r
library(challengeR)
```

<!-- # Visualization of the impact of priors in terms of effective sample size -->

<!-- This section (same as `vignette("vignettePlanning", package = "ESS")`)  visualizes how `plotECSS()` can be used to quantify the impact of a prior (including mixture priors and empirical Bayes power priors/commensurate priors) in terms of effective current sample sizes on a grid of true values of the data generating process. -->

## 2\. Load data

### Data requirements

Data requires the following *columns*

  - a *task identifier* in case of multi-task challenges.
  - a *test case identifier*
  - the *algorithm name*
  - the *metric value*

In case of missing metric values, a missing observation has to be
provided (either as blank field or “NA”).

For example, in a challenge with 2 tasks, 2 test cases and 2 algorithms,
where in task “T2”, test case “case2”, algorithm “A2” didn’t give a
prediction (and thus NA or a blank field for missing value is inserted),
the data set might look like this:

| Task | TestCase | Algorithm | MetricValue |
| :--- | :------- | :-------- | ----------: |
| T1   | case1    | A1        |       0.266 |
| T1   | case1    | A2        |       0.202 |
| T1   | case2    | A1        |       0.573 |
| T1   | case2    | A2        |       0.945 |
| T2   | case1    | A1        |       0.372 |
| T2   | case1    | A2        |       0.898 |
| T2   | case2    | A1        |       0.908 |
| T2   | case2    | A2        |          NA |

### Load data

If you have assessment data at hand stored in a csv file (if you want to
use simulated data skip the following code line) use

``` r
data_matrix=read.csv(file.choose()) # type ?read.csv for help
```

This allows to choose a file interactively, otherwise replace
*file.choose()* by the file path (in style “/path/to/dataset.csv”) in
quotation
marks.

<!-- where "filename" has to be replaced by the filename (and file path). -->

For illustration purposes, in the following simulated data is generated
*instead* (skip the following code chunk if you have already loaded
data). The data is also stored as “data\_matrix.csv” in the
repository.

``` r
if (!requireNamespace("permute", quietly = TRUE)) install.packages("permute")

n=50

set.seed(4)
strip=runif(n,.9,1)
c_ideal=cbind(task="c_ideal",
            rbind(
              data.frame(alg_name="A1",value=runif(n,.9,1),case=1:n),
              data.frame(alg_name="A2",value=runif(n,.8,.89),case=1:n),
              data.frame(alg_name="A3",value=runif(n,.7,.79),case=1:n),
              data.frame(alg_name="A4",value=runif(n,.6,.69),case=1:n),
              data.frame(alg_name="A5",value=runif(n,.5,.59),case=1:n)
            ))

set.seed(1)
c_random=data.frame(task="c_random",
                       alg_name=factor(paste0("A",rep(1:5,each=n))),
                       value=plogis(rnorm(5*n,1.5,1)),case=rep(1:n,times=5)
                       )

strip2=seq(.8,1,length.out=5)
a=permute::allPerms(1:5)
c_worstcase=data.frame(task="c_worstcase",
                     alg_name=c(t(a)),
                     value=rep(strip2,nrow(a)),
                     case=rep(1:nrow(a),each=5)
                     )
c_worstcase=rbind(c_worstcase,
                data.frame(task="c_worstcase",alg_name=1:5,value=strip2,case=max(c_worstcase$case)+1)
          )
c_worstcase$alg_name=factor(c_worstcase$alg_name,labels=paste0("A",1:5))

data_matrix=rbind(c_ideal, c_random, c_worstcase)
```

## 3 Perform ranking

### 3.1 Define challenge object

Code differs slightly for single and multi task challenges.

In case of a single task challenge use

``` r
# Use only task "c_random" in object data_matrix
  dataSubset=subset(data_matrix, task=="c_random")

  challenge=as.challenge(dataSubset, 
                        # Specify which column contains the algorithm, 
                        # which column contains a test case identifier 
                        # and which contains the metric value:
                        algorithm="alg_name", case="case", value="value", 
                        # Specify if small metric values are better
                        smallBetter = FALSE)
```

*Instead*, for a multi-task challenge
use

``` r
# Same as above but with 'by="task"' where variable "task" contains the task identifier
  challenge=as.challenge(data_matrix, 
                         by="task", 
                         algorithm="alg_name", case="case", value="value", 
                         smallBetter = FALSE)
```

### 3.2 Perform ranking

Different ranking methods are available, choose one of them:

  - for “aggregate-then-rank” use (here: take mean for
aggregation)

<!-- end list -->

``` r
ranking=challenge%>%aggregateThenRank(FUN = mean, # aggregation function, 
                                                  # e.g. mean, median, min, max, 
                                                  # or e.g. function(x) quantile(x, probs=0.05)
                                      na.treat=0, # either "na.rm" to remove missing data, 
                                                  # set missings to numeric value (e.g. 0) 
                                                  # or specify a function, 
                                                  # e.g. function(x) min(x)
                                      ties.method = "min" # a character string specifying 
                                                          # how ties are treated, see ?base::rank
                                            )  
```

  - *alternatively*, for “rank-then-aggregate” with arguments as above
    (here: take mean for aggregation):

<!-- end list -->

``` r
ranking=challenge%>%rankThenAggregate(FUN = mean,
                                      ties.method = "min"
                                      )
```

  - *alternatively*, for test-then-rank based on Wilcoxon signed rank
    test:

<!-- end list -->

``` r
ranking=challenge%>%testThenRank(alpha=0.05, # significance level
                                 p.adjust.method="none",  # method for adjustment for
                                                          # multiple testing, see ?p.adjust
                                 na.treat=0, # either "na.rm" to remove missing data,
                                             # set missings to numeric value (e.g. 0)
                                             # or specify a function, e.g. function(x) min(x)
                                 ties.method = "min" # a character string specifying
                                                     # how ties are treated, see ?base::rank
                     )
```

## 4\. Perform bootstrapping

Perform bootstrapping with 1000 bootstrap samples using one CPU

``` r
set.seed(1)
ranking_bootstrapped=ranking%>%bootstrap(nboot=1000)
```

If you want to use multiple CPUs (here: 8 CPUs), use

``` r
library(doParallel)
registerDoParallel(cores=8)  
set.seed(1)
ranking_bootstrapped=ranking%>%bootstrap(nboot=1000, parallel=TRUE, progress = "none")
stopImplicitCluster()
```

## 5\. Generate the report

Generate report in PDF, HTML or DOCX format. Code differs slightly for
single and multi task challenges.

### 5.1 For single task challenges

``` r
ranking_bootstrapped %>% 
  report(title="singleTaskChallengeExample", # used for the title of the report
         file = "filename", 
         format = "PDF", # format can be "PDF", "HTML" or "Word"
         latex_engine="pdflatex", #LaTeX engine for producing PDF output. Options are "pdflatex", "lualatex", and "xelatex"
         clean=TRUE #optional. Using TRUE will clean intermediate files that are created during rendering.
        ) 
```

Argument *file* allows for specifying the output file path as well,
otherwise the working directory is used. If file is specified but does
not have a file extension, an extension will be automatically added
according to the output format given in *format*. Using argument
*clean=FALSE* allows to retain intermediate files, such as separate
files for each figure.

If argument “file” is omitted, the report is created in a temporary
folder with file name “report”.

### 5.1 For multi task challenges

Same as for single task challenges, but additionally consensus ranking
(rank aggregation across tasks) has to be given.

Compute ranking consensus across tasks (here: consensus ranking
according to mean ranks across
tasks):

``` r
# See ?relation_consensus for different methods to derive consensus ranking
meanRanks=ranking%>%consensus(method = "euclidean") 
meanRanks # note that there may be ties (i.e. some algorithms have identical mean rank)
```

Generate report as above, but with additional specification of consensus
ranking

``` r
ranking_bootstrapped %>% 
  report(consensus=meanRanks,
         title="multiTaskChallengeExample",
         file = "filename", 
         format = "PDF", # format can be "PDF", "HTML" or "Word"
         latex_engine="pdflatex"#LaTeX engine for producing PDF output. Options are "pdflatex", "lualatex", and "xelatex"
        )
```

# Changes

#### Version 0.2.4

  - Automatic insertion of missings

#### Version 0.2.3

  - Bug fixes
  - Reports for subsets (top list) of algorithms: Use e.g.
    `subset(ranking_bootstrapped, top=3) %>% report(...)` (or
    `subset(ranking, top=3) %>% report(...)` for report without
    bootstrap results) to only show the top 3 algorithms according to
    the chosen ranking methods, where `ranking_bootstrapped` and
    `ranking` objects as defined in the example. Line plot for ranking
    robustness can be used to check whether algorithms performing well
    in other ranking methods are excluded. Bootstrapping still takes
    entire uncertainty into account. Podium plot neglect and ranking
    heatmap neglect excluded algorithms. Only available for single task
    challenges (for mutli task challenges not sensible because each task
    would contain a different sets of algorithms).
  - Reports for subsets of tasks: Use e.g. `subset(ranking_bootstrapped,
    tasks=c("task1", "task2","task3)) %>% report(...)` to restrict
    report to tasks “task1”, “task2”,"task3. You may want to recompute
    the consensus ranking before using `meanRanks=subset(ranking,
    tasks=c("task1", "task2","task3))%>%consensus(method = "euclidean")`

### Version 0.2.1

  - Introduction in reports now mentions e.g. ranking method, number of
    test cases,…
  - Function `subset()` allows selection of tasks after bootstrapping,
    e.g. `subset(ranking_bootstrapped,1:3)`
  - `report()` functions gain argument `colors` (default:
    `default_colors`). Change e.g. to `colors=viridisLite::inferno`
    which “is designed in such a way that it will analytically be
    perfectly perceptually-uniform, both in regular form and also when
    converted to black-and-white. It is also designed to be perceived by
    readers with the most common form of color blindness.” See package
    `viridis` for further similar functions.

### Version 0.2.0

  - Improved layout in case of many algorithms and tasks (while probably
    still not perfect)
  - Consistent coloring of algorithms across figures
  - `report()` function can be applied to ranked object before
    bootstrapping (and thus excluding figures based on bootstrapping),
    i.e. in the example `ranking %>% report(...)`
  - bug fixes

# Reference

Wiesenfarth, M., Reinke, A., Landmann A.L., Cardoso, M.J., Maier-Hein,
L. and Kopp-Schneider, A. (2019). Methods and open-source toolkit for
analyzing and visualizing challenge results. *arXiv preprint
arXiv:1910.05121*
