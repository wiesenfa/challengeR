## Single task, aggregate-then-rank ranking

## 1\. Load package

library(challengeR)

## 2\. Load data

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

## 3 Perform ranking

### 3.1 Define challenge object

dataSubset=subset(data_matrix, task=="c_random")

challenge=as.challenge(dataSubset, algorithm="alg_name", case="case", value="value", smallBetter = FALSE)

### 3.2 Perform ranking

ranking=challenge%>%aggregateThenRank(FUN = mean, na.treat=0, ties.method = "min")  

## 4\. Perform bootstrapping

library(doParallel)
library(doRNG)
registerDoParallel(cores=8)
registerDoRNG(1)
ranking_bootstrapped=ranking%>%bootstrap(nboot=1000, parallel=TRUE, progress="none")
stopImplicitCluster()

## 5\. Generate the report
ranking_bootstrapped %>% 
  report(title="singleTaskChallengeExample", # used for the title of the report
         file = "SingleTask_aggregate-then-rank", 
         format = "PDF", # format can be "PDF", "HTML" or "Word"
         latex_engine="pdflatex", #LaTeX engine for producing PDF output. Options are "pdflatex", "lualatex", and "xelatex"
         clean=TRUE #optional. Using TRUE will clean intermediate files that are created during rendering.
        ) 
