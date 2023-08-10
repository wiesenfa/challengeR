Methods and open-source toolkit for analyzing and visualizing challenge
results
================

-   [Introduction](#introduction)
-   [Installation](#installation)
-   [Terms of use](#terms-of-use)
-   [Usage](#usage)
-   [Troubleshooting](#troubleshooting)
-   [Changes](#changes)
-   [Team](#team)
-   [Reference](#reference)

# Introduction

The current framework is a tool for analyzing and visualizing challenge
results in the field of biomedical image analysis and beyond.

Biomedical challenges have become the de facto standard for benchmarking
biomedical image analysis algorithms. While the number of challenges is
steadily increasing, surprisingly little effort has been invested in
ensuring high quality design, execution and reporting for these
international competitions. Specifically, results analysis and
visualization in the event of uncertainties have been given almost no
attention in the literature.

Given these shortcomings, the current framework aims to enable fast and
wide adoption of comprehensively analyzing and visualizing the results
of single-task and multi-task challenges. This approach offers an
intuitive way to gain important insights into the relative and absolute
performance of algorithms, which cannot be revealed by commonly applied
visualization techniques.

# Installation

Requires R version &gt;= 3.5.2 (<https://www.r-project.org>).

Further, a recent version of Pandoc (&gt;= 1.12.3) is required. RStudio
(<https://rstudio.com>) automatically includes this so you do not need
to download Pandoc if you plan to use rmarkdown from the RStudio IDE,
otherwise you’ll need to install Pandoc for your platform
(<https://pandoc.org/installing.html>). Finally, if you want to generate
a PDF report you will need to have LaTeX installed (e.g. MiKTeX, MacTeX
or TinyTeX).

To get the latest released version (master branch) of the R package from
GitHub:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("Rgraphviz", dependencies = TRUE)
devtools::install_github("wiesenfa/challengeR", dependencies = TRUE)
```

If you are asked whether you want to update installed packages and you
type “a” for all, you might need administrator permissions to update R
core packages. You can also try to type “n” for updating no packages. If
you are asked “Do you want to install from sources the packages which
need compilation? (Yes/no/cancel)”, you can safely type “no”.

If you get *warning* messages (in contrast to *error* messages), these
might not be problematic and you can try to proceed. If you encounter
errors during the setup, looking into the “Troubleshooting” section
might be worth it.

For Linux users: Some system libraries might be missing. Check the
output in the R console for further hints carefully during the
installation of packages.

# Terms of use

Copyright (c) German Cancer Research Center (DKFZ). All rights reserved.

challengeR is available under license GPLv2 or any later version.

If you use this software for a publication, please cite:

Wiesenfarth, M., Reinke, A., Landman, B.A., Eisenmann, M., Aguilera
Saiz, L., Cardoso, M.J., Maier-Hein, L. and Kopp-Schneider, A. Methods
and open-source toolkit for analyzing and visualizing challenge results.
*Sci Rep* **11**, 2369 (2021).
<https://doi.org/10.1038/s41598-021-82017-6>

# Usage

Each of the following steps has to be run to generate the report: (1)
Load package, (2) load data, (3) perform ranking, (4) perform
bootstrapping and (5) generation of the report

You can find R scripts for quickstart in the directory “vignettes”. An
overview of all available plots is provided in the “Visualizations”
vignette demonstrating the use of their corresponding plot functions as
well.

Here, we provide a step-by-step guide that leads you to your final
report.

## 1. Load package

Load package

``` r
library(challengeR)
```

## 2. Load data

### Data requirements

Data requires the following *columns*:

-   *task identifier* in case of multi-task challenges (string or
    numeric)
-   *test case identifier* (string or numeric)
-   *algorithm identifier* (string or numeric)
-   *metric value* (numeric)

In case of missing metric values, a missing observation has to be
provided (either as blank field or “NA”).

For example, in a challenge with 2 tasks, 2 test cases and 2 algorithms,
where in task “T2”, test case “case2”, algorithm “A2” didn’t give a
prediction (and thus NA or a blank field for missing value is inserted),
the data set might look like this:

| Task | TestCase | Algorithm | MetricValue |
|:-----|:---------|:----------|------------:|
| T1   | case1    | A1        |       0.266 |
| T1   | case1    | A2        |       0.202 |
| T1   | case2    | A1        |       0.573 |
| T1   | case2    | A2        |       0.945 |
| T2   | case1    | A1        |       0.372 |
| T2   | case1    | A2        |       0.898 |
| T2   | case2    | A1        |       0.908 |
| T2   | case2    | A2        |          NA |

### 2.1 Load data from file

If you have assessment data at hand stored in a csv file (if you want to
use simulated data, skip the following code line) use

``` r
data_matrix <- read.csv(file.choose()) # type ?read.csv for help
```

This allows to choose a file interactively, otherwise replace
*file.choose()* by the file path (in style “/path/to/dataset.csv”) in
quotation marks.

### 2.2 Simulate data

In the following, simulated data is generated *instead* for illustration
purposes (skip the following code chunk if you have already loaded
data). The data is also stored as “inst/extdata/data\_matrix.csv” in the
repository.

``` r
if (!requireNamespace("permute", quietly = TRUE)) install.packages("permute")

n <- 50

set.seed(4)
strip <- runif(n,.9,1)
c_ideal <- cbind(task="c_ideal",
            rbind(
              data.frame(alg_name="A1",value=runif(n,.9,1),case=1:n),
              data.frame(alg_name="A2",value=runif(n,.8,.89),case=1:n),
              data.frame(alg_name="A3",value=runif(n,.7,.79),case=1:n),
              data.frame(alg_name="A4",value=runif(n,.6,.69),case=1:n),
              data.frame(alg_name="A5",value=runif(n,.5,.59),case=1:n)
            ))

set.seed(1)
c_random <- data.frame(task="c_random",
                       alg_name=factor(paste0("A",rep(1:5,each=n))),
                       value=plogis(rnorm(5*n,1.5,1)),case=rep(1:n,times=5)
                       )

strip2 <- seq(.8,1,length.out=5)
a <- permute::allPerms(1:5)
c_worstcase <- data.frame(task="c_worstcase",
                     alg_name=c(t(a)),
                     value=rep(strip2,nrow(a)),
                     case=rep(1:nrow(a),each=5)
                     )
c_worstcase <- rbind(c_worstcase,
                data.frame(task="c_worstcase",alg_name=1:5,value=strip2,case=max(c_worstcase$case)+1)
          )
c_worstcase$alg_name <- factor(c_worstcase$alg_name,labels=paste0("A",1:5))

data_matrix <- rbind(c_ideal, c_random, c_worstcase)
```

## 3. Perform ranking

### 3.1 Define challenge object

Code differs slightly for single- and multi-task challenges.

In case of a single-task challenge use

``` r
# Use only task "c_random" in object data_matrix
dataSubset <- subset(data_matrix, task=="c_random")

challenge <- as.challenge(dataSubset,
                          # Specify which column contains the algorithms, 
                          # which column contains a test case identifier 
                          # and which contains the metric value:
                          algorithm = "alg_name", case = "case", value = "value", 
                          # Specify if small metric values are better
                          smallBetter = FALSE)
```

*Instead*, for a multi-task challenge use

``` r
# Same as above but with 'by="task"' where variable "task" contains the task identifier
challenge <- as.challenge(data_matrix, 
                          by = "task", 
                          algorithm = "alg_name", case = "case", value = "value", 
                          smallBetter = FALSE)
```

### 3.2 Configure ranking

Different ranking methods are available, choose one of them:

-   for “aggregate-then-rank” use (here: take mean for aggregation)

``` r
ranking <- challenge%>%aggregateThenRank(FUN = mean, # aggregation function, 
                                                     # e.g. mean, median, min, max, 
                                                     # or e.g. function(x) quantile(x, probs=0.05)
                                         na.treat = 0, # either "na.rm" to remove missing data, 
                                                       # set missings to numeric value (e.g. 0) 
                                                       # or specify a function, 
                                                       # e.g. function(x) min(x)
                                         ties.method = "min" # a character string specifying 
                                                             # how ties are treated, see ?base::rank
                                        )  
```

-   *alternatively*, for “rank-then-aggregate” with arguments as above
    (here: take mean for aggregation)

``` r
ranking <- challenge%>%rankThenAggregate(FUN = mean,
                                         ties.method = "min"
                                        )
```

-   *alternatively*, for test-then-rank based on Wilcoxon signed rank
    test

``` r
ranking <- challenge%>%testThenRank(alpha = 0.05, # significance level
                                    p.adjust.method = "none", # method for adjustment for
                                                              # multiple testing, see ?p.adjust
                                    na.treat = 0, # either "na.rm" to remove missing data,
                                                  # set missings to numeric value (e.g. 0)
                                                  # or specify a function, e.g. function(x) min(x)
                                    ties.method = "min" # a character string specifying
                                                        # how ties are treated, see ?base::rank
                                   )
```

## 4. Perform bootstrapping

Perform bootstrapping with 1000 bootstrap samples using one CPU

``` r
set.seed(123, kind = "L'Ecuyer-CMRG")
ranking_bootstrapped <- ranking%>%bootstrap(nboot = 1000)
```

If you want to use multiple CPUs (here: 8 CPUs), use

``` r
library(doParallel)
library(doRNG)
registerDoParallel(cores = 8)  
registerDoRNG(123)
ranking_bootstrapped <- ranking%>%bootstrap(nboot = 1000, parallel = TRUE, progress = "none")
stopImplicitCluster()
```

## 5. Generate the report

Generate report in PDF, HTML or DOCX format. Code differs slightly for
single- and multi-task challenges.

### 5.1 For single-task challenges

``` r
ranking_bootstrapped %>% 
  report(title = "singleTaskChallengeExample", # used for the title of the report
         file = "filename", 
         format = "PDF", # format can be "PDF", "HTML" or "Word"
         latex_engine = "pdflatex", #LaTeX engine for producing PDF output. Options are "pdflatex", "lualatex", and "xelatex"
         clean = TRUE #optional. Using TRUE will clean intermediate files that are created during rendering.
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

### 5.2 For multi-task challenges

Same as for single-task challenges, but additionally consensus ranking
(rank aggregation across tasks) has to be given.

Compute ranking consensus across tasks (here: consensus ranking
according to mean ranks across tasks)

``` r
# See ?relation_consensus for different methods to derive consensus ranking
meanRanks <- ranking%>%consensus(method = "euclidean") 
meanRanks # note that there may be ties (i.e. some algorithms have identical mean rank)
```

Generate report as above, but with additional specification of consensus
ranking

``` r
ranking_bootstrapped %>% 
  report(consensus = meanRanks,
         title = "multiTaskChallengeExample",
         file = "filename", 
         format = "PDF", # format can be "PDF", "HTML" or "Word"
         latex_engine = "pdflatex"#LaTeX engine for producing PDF output. Options are "pdflatex", "lualatex", and "xelatex"
        )
```

The consensus ranking is given according to mean ranks across tasks if
method=“euclidean” where in case of ties (equal ranks for multiple
algorithms) the average rank is used, i.e. ties.method=“average”.

# Troubleshooting

In this section we provide an overview of issues that the users reported
and how they were solved.

## Issues related to RStudio

### Issue: Rtools is missing

While trying to install the current version of the repository:

``` r
devtools::install_github("wiesenfa/challengeR", dependencies = TRUE)
```

The following warning showed up in the output:

``` r
WARNING: Rtools is required to build R packages, but is not currently installed.
```

Therefore, Rtools was installed via a separate executable:
<https://cran.r-project.org/bin/windows/Rtools/> and the warning
disappeared.

#### Solution:

Actually there is no need of installing Rtools, it is not really used in
the toolkit. Insted, choose not to install it when it is asked. See
comment in the installation section:

“If you are asked whether you want to update installed packages and you
type “a” for all, you might need administrator rights to update R core
packages. You can also try to type “n” for updating no packages. If you
are asked “Do you want to install from sources the packages which need
compilation? (Yes/no/cancel)”, you can safely type “no”.”

### Issue: Package versions are mismatching

Installing the current version of the tool from GitHub failed.

The error message was:

``` r
byte-compile and prepare package for lazy loading
Error: (converted from warning) package 'ggplot2' was built under R version 3.6.3
Execution halted
ERROR: lazy loading failed for package 'challengeR'
* removing 'C:/Users/.../Documents/R/win-library/3.6/challengeR'
* restoring previous 'C:/Users/.../Documents/R/win-library/3.6/challengeR'
Error: Failed to install 'challengeR' from GitHub:
  (converted from warning) installation of package 'C:/Users/.../AppData/Local/Temp/Rtmp615qmV/file4fd419555eb4/challengeR_0.3.1.tar.gz' had non-zero exit status
```

The problem was that some of the packages that were built under R3.6.1
had been updated, but the current installed version was still R3.6.1.

#### Solution:

The solution was to update R3.6.1 to R3.6.3. Another way would have been
to reset the single packages to the versions built under R3.6.1.

### Issue: Package is missing

Installing the current version of the tool from GitHub failed.

``` r
 devtools::install_github("wiesenfa/challengeR", dependencies = TRUE)
```

The error message was:

``` r
Error: .onLoad failed in loadNamespace() for 'pkgload', details:
  call: loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
  error: there is no package called ‘backports’
```

The problem was that the packages ‘backports’ had not been installed.

#### Solution:

The solution was to install ‘backports’ manually.

``` r
 install.packages("backports")
```

### Issue: Packages are not detected correctly

While trying to install the package after running the following
commands:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("Rgraphviz", dependencies = TRUE)
devtools::install_github("wiesenfa/challengeR", dependencies = TRUE)
```

The error message was:

``` r
ERROR:
1: In file(con, "r") :
 URL 'https://bioconductor.org/config.yaml': status was 'SSL connect error'
2: packages ‘BiocVersion’, ‘Rgraphviz’ are not available (for R version 3.6.1)
```

#### Solution:

The solution was to restart RStudio.

## Issues related to MiKTeX

### Issue: Missing packages

While generating the PDF with MiKTeX (2.9), the following error showed
up:

``` r
fatal pdflatex - gui framework cannot be initialized
```

There is an issue with installing missing packages in LaTeX.

##### Solution:

Open your MiKTeX Console –&gt; Settings, select “Always install missing
packages on-the-fly”. Then generate the report. Once the report is
generated, you can reset the settings to your preferred ones.

### Issue: Unable to generate report

While generating the PDF with MiKTeX (2.9):

``` r
ranking_bootstrapped %>% 
  report(title = "singleTaskChallengeExample", # used for the title of the report
         file = "filename", 
         format = "PDF", # format can be "PDF", "HTML" or "Word"
         latex_engine = "pdflatex", #LaTeX engine for producing PDF output. Options are "pdflatex", "lualatex", and "xelatex"
         clean = TRUE #optional. Using TRUE will clean intermediate files that are created during rendering.
        ) 
```

The following error showed up:

``` r
output file: filename.knit.md

"C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS filename.utf8.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output filename.tex --self-contained --number-sections --highlight-style tango --pdf-engine pdflatex --variable graphics --lua-filter "C:/Users/adm/Documents/R/win-library/3.6/rmarkdown/rmd/lua/pagebreak.lua" --lua-filter "C:/Users/adm/Documents/R/win-library/3.6/rmarkdown/rmd/lua/latex-div.lua" --variable "geometry:margin=1in" 

Error: LaTeX failed to compile filename.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips.

  Warning message:
In system2(..., stdout = if (use_file_stdout()) f1 else FALSE, stderr = f2) :
  '"pdflatex"' not found
```

#### Solution:

The solution was to restart RStudio.

# Changes

#### Version 1.0.5

-   Ensure reproducibility with parallel bootstrapping
    ([T29361](https://phabricator.mitk.org/T29361))

#### Version 1.0.4

-   Fix NaN values cause error
    ([T28746](https://phabricator.mitk.org/T28746))
-   Fix Bars and dots don’t match in podium plot
    ([T29167](https://phabricator.mitk.org/T29167))
-   Fix y-axis of blob plots always scaled to 5
    ([T28966](https://phabricator.mitk.org/T28966))

#### Version 1.0.3

-   Fix ggplot warning in various places of the report
    ([T28710](https://phabricator.mitk.org/T28710))

#### Version 1.0.2

-   Fix error when all metric values are the same
    ([T28453](https://phabricator.mitk.org/T28453))
-   Fix wrong number of algorithms shown in report summary
    ([T28465](https://phabricator.mitk.org/T28465))

#### Version 1.0.1

-   Fix error raised in case there are more tasks than algorithms
    contained in the dataset
    ([T28193](https://phabricator.mitk.org/T28193))
-   Drop restriction that at least three algorithms are required for
    bootstrapping ([T28194](https://phabricator.mitk.org/T28194))
-   Avoid blank pages in PDF report when bootstrapping is disabled
    ([T28201](https://phabricator.mitk.org/T28201))
-   Handle tasks having only one case for bootstrapping
    ([T28202](https://phabricator.mitk.org/T28202))
-   Update citation ([T28210](https://phabricator.mitk.org/T28210))

#### Version 1.0.0

-   Revision of the underlying data structure
-   Roxygen documentation for main functionality
-   Vignettes for quickstart and overview of available plots
    demonstrating the use of their corresponding plot functions
-   Introduction of unit tests (package coverage &gt;70%)
-   Troubleshooting section covering potential issues during setup
-   Finally: Extensive bug fixes and improvements (for a complete
    overview please check the [Phabricator
    tasks](https://phabricator.mitk.org/search/query/vtj0qOqH5qL6/))

#### Version 0.3.3

-   Force line break to avoid that authors exceed the page in generated
    PDF reports

#### Version 0.3.2

-   Correct names of authors

#### Version 0.3.1

-   Refactoring

#### Version 0.3.0

-   Major bug fix release

#### Version 0.2.5

-   Bug fixes

#### Version 0.2.4

-   Automatic insertion of missings

#### Version 0.2.3

-   Bug fixes
-   Reports for subsets (top list) of algorithms: Use
    e.g. `subset(ranking_bootstrapped, top=3) %>% report(...)` (or
    `subset(ranking, top=3) %>% report(...)` for report without
    bootstrap results) to only show the top 3 algorithms according to
    the chosen ranking methods, where `ranking_bootstrapped` and
    `ranking` objects as defined in the example. Line plot for ranking
    robustness can be used to check whether algorithms performing well
    in other ranking methods are excluded. Bootstrapping still takes
    entire uncertainty into account. Podium plot and ranking heatmap
    neglect excluded algorithms. Only available for single-task
    challenges (for multi-task challenges not sensible because each task
    would contain a different set of algorithms).
-   Reports for subsets of tasks: Use
    e.g. `subset(ranking_bootstrapped, tasks=c("task1", "task2","task3")) %>% report(...)`
    to restrict report to tasks “task1”, “task2”,"task3. You may want to
    recompute the consensus ranking before using
    `meanRanks=subset(ranking, tasks=c("task1", "task2", "task3"))%>%consensus(method = "euclidean")`

#### Version 0.2.1

-   Introduction in reports now mentions e.g. ranking method, number of
    test cases,…
-   Function `subset()` allows selection of tasks after bootstrapping,
    e.g. `subset(ranking_bootstrapped,1:3)`
-   `report()` functions gain argument `colors` (default:
    `default_colors`). Change e.g. to `colors=viridisLite::inferno`
    which “is designed in such a way that it will analytically be
    perfectly perceptually-uniform, both in regular form and also when
    converted to black-and-white. It is also designed to be perceived by
    readers with the most common form of color blindness.” See package
    `viridis` for further similar functions.

#### Version 0.2.0

-   Improved layout in case of many algorithms and tasks (while probably
    still not perfect)
-   Consistent coloring of algorithms across figures
-   `report()` function can be applied to ranked object before
    bootstrapping (and thus excluding figures based on bootstrapping),
    i.e. in the example `ranking %>% report(...)`
-   bug fixes

# Team

The developer team includes members from both division of Intelligent
Medical Systems (IMSY) and Biostatistics at the German Cancer Research
Center (DKFZ):

-   Manuel Wiesenfarth
-   Annette Kopp-Schneider
-   Annika Reinke
-   Matthias Eisenmann
-   Laura Aguilera Saiz
-   Elise Récéjac
-   Lena Maier-Hein
-   Ali Emre Kavur

# Reference

Wiesenfarth, M., Reinke, A., Landman, B.A., Eisenmann, M., Aguilera
Saiz, L., Cardoso, M.J., Maier-Hein, L. and Kopp-Schneider, A. Methods
and open-source toolkit for analyzing and visualizing challenge results.
*Sci Rep* **11**, 2369 (2021).
<https://doi.org/10.1038/s41598-021-82017-6>

</br> <img src="Helmholtz_Imaging_Logo.svg" height="70px" /> </br></br>
<img src="DKFZ_Logo.png" height="100px" />
