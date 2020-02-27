network <- function(x,...) UseMethod("network")
network.default <- function(x, ...) stop("not implemented for this class")

network.ranked.list=function(x,
                             method = "symdiff", 
                             edge.col,
                             edge.lwd,
                             rate=1.05,
                             cols,
                              ...
                             
){
  # use ranking list
  relensemble=as.relation.ranked.list(x)
  
  # # use relations
  #   a=challenge_multi%>%decision.challenge(p.adjust.method="none")
  #   aa=lapply(a,as.relation.challenge.incidence)
  #   names(aa)=names(challenge_multi)
  #   relensemble= do.call(relation_ensemble,args = aa)
  d <- relation_dissimilarity(relensemble, method = method)

  #coloring
  # # use relations
  # rm <-my.bsranking(relensemble) #for coloring
  # uw <- apply(rm, 2,
  #             function(x) {
  #               w <- which(x == 1)
  #               ifelse(length(w) == 1,
  #                      names(w), "none")
  #             })
  # use ranking list
  uw=lapply(x$matlist,function(task.i) rownames(task.i)[which(task.i$rank==1)])
  uw=sapply(uw, function(task.i) ifelse(length(task.i)==1,yes = task.i,no="none"))
  
  network.dist(d, 
                 edge.col = edge.col,# grDevices::grey.colors(nn), #terrain_hcl(nn, c=c(65,0), l=c(45,90), power=c(1/2,1.5)),
                 edge.lwd =edge.lwd,#4*rev(1.2^seq_len(length(unique(d)))/(1.2^length((unique(d))))),# seq(1, .001, length.out=nn),
                 rate=rate,
                 node.fill = cols[uw],...
  )
}



network.dist=
  function (x, rate=1.05, #ndists.show = length(sort(unique(x))), 
            edge.col = gray(0.7), 
            edge.lwd = 1, 
            node.fill = NULL, 
            ...) {
    nn=length(unique(c(x))) # ==max(rm) number of different distance levels
    if (is.function(edge.col)) edge.col=edge.col(nn)
    data <- as.matrix(x)
    nodes <- colnames(data)
    nnodes <- length(nodes)
    dists <- sort(unique(x))
    ndists <- length(dists)
    dshow <- dists#[seq_len(ndists.show)] 
    ndshow <- length(dshow)
    edge.col <- rep(edge.col, ndshow)
    edge.lwd <- rep(edge.lwd, ndshow)
    edge.len <- ceiling((rate)^dists)# exponential distance
    #   edge.len <- ceiling((1.2)^(seq_len(ndists) - 1)) #verwende ordnung
    #   edge.len <- ceiling((1.05)^(dists-min(dists)+1))# verwende distance mit min==1
    edge.weight <- rev(dists) #rev(seq_len(ndists))
    edge.lty <- c(rep("solid", ndshow), 
                  rep("blank", length(dists) - ndshow))
    graph <- new("graphNEL", 
                 nodes = nodes, 
                 edgemode = "undirected")
    edgeAttrs <- list()
    nodeAttrs <- list()
    for (i in 1:(nnodes - 1)) {
      for (j in (i + 1):nnodes) {
        s <- data[i, j]
        # if (s %in% dshow) {
        t <- which(s == dists)
        graph <- graph::addEdge(nodes[i], nodes[j], graph, 1) #edge.weight[t])
        n <- paste(nodes[i], nodes[j], sep = "~")
        edgeAttrs$len[n] <- edge.len[t] # laenge exponentiell
        #        edgeAttrs$len[n] <- s # laenge prop zu distance
        edgeAttrs$color[n] <- "black"#edge.col[t]
        edgeAttrs$lwd[n] <- edge.lwd[t]
        edgeAttrs$lty[n] <- 1#edge.lty[t]
        #   }
      }
    }
    if (!is.null(node.fill)) 
      nodeAttrs$fillcolor[nodes] <- node.fill
    
    out= list(graph=graph,
              nodeAttrs = nodeAttrs, 
              edgeAttrs = edgeAttrs,
              tasknames=nodes,
              leg.col=node.fill[unique(names(node.fill))]
    )
    class(out)="network"
    out
  }


plot.network=function(x,
                      layoutType = "neato",
                      fixedsize=TRUE,
                      fontsize,
                      width,
                      height,
                      shape="ellipse",
                      cex=.8,
                      ...
){
  graph=x$graph
  nodeAttrs=x$nodeAttrs
  edgeAttrs=x$edgeAttrs
  leg.col=x$leg.col
  
  layoutType = layoutType
  attrs <- Rgraphviz::getDefaultAttrs(layoutType = layoutType)
  attrs$node$fixedsize <- fixedsize
  attrs$node$shape=shape
  if (missing(fontsize)) {
    attrs$node$fontsize <- max(sapply(x$tasknames,nchar))-1
  } else attrs$node$fontsize=fontsize
  if (missing(width)){
    attrs$node$width <- max(sapply(x$tasknames,nchar)) 
  } else attrs$node$width=width
  if (missing(height)) {
    attrs$node$height <- max(sapply(x$tasknames,nchar))/2
  } else attrs$node$height=height
  
  ag <- Rgraphviz::agopen(graph, 
                          "", 
                          layoutType = layoutType, 
                          attrs = attrs, 
                          nodeAttrs = nodeAttrs, 
                          edgeAttrs = edgeAttrs)
  
    plot.new()
    l=legend("topright",  
             names(leg.col), 
             lwd = 1, 
             cex=cex, 
             bg =NA,
             plot=F)# bg="white")
    w <- grconvertX(l$rect$w, to='inches')
    
    Rgraphviz::plot(ag,mai=c(0,0,0,w),...)
    legend(par('usr')[2], par('usr')[4], 
           xpd=NA,  
           names(leg.col), 
           lwd = 1, 
           col = leg.col, 
           bg =NA,
           cex=cex)# bg="white")
 
}





#library(R.utils)
#reassignInPackage("beplot0.matrix","benchmark",my.beplot0.matrix)
#reassignInPackage("beplot0.AlgorithmPerformance","benchmark",my.beplot0.AlgorithmPerformance)
