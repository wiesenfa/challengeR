my.bsgraph0b=
  function (x, rate=1.05, ndists.show = length(sort(unique(x))), edge.col = gray(0.7), 
            edge.lwd = 1, node.fill = NULL, ...) {
    data <- as.matrix(x)
    nodes <- colnames(data)
    nnodes <- length(nodes)
    dists <- sort(unique(x))
    ndists <- length(dists)
    dshow <- dists#[seq_len(ndists.show)] 
    ndshow <- length(dshow)
    edge.col <- rep(edge.col, ndshow)
    edge.lwd <- rep(edge.lwd, ndshow)
    edge.len <- ceiling((rate)^dists)# verwende distance exponentiell
    #   edge.len <- ceiling((1.2)^(seq_len(ndists) - 1)) #verwende ordnung
    #   edge.len <- ceiling((1.05)^(dists-min(dists)+1))# verwende distance mit min==1
    edge.weight <- rev(dists) #rev(seq_len(ndists))
    edge.lty <- c(rep("solid", ndshow), rep("blank", length(dists) - 
                                              ndshow))
    graph <- new("graphNEL", nodes = nodes, edgemode = "undirected")
    edgeAttrs <- list()
    nodeAttrs <- list()
    for (i in 1:(nnodes - 1)) {
      for (j in (i + 1):nnodes) {
        s <- data[i, j]
        # if (s %in% dshow) {
        t <- which(s == dists)
        graph <- addEdge(nodes[i], nodes[j], graph, 1) #edge.weight[t])
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
    # bsgraph0(graph, nodeAttrs = nodeAttrs, edgeAttrs = edgeAttrs)
    list(graph=graph,nodeAttrs = nodeAttrs, edgeAttrs = edgeAttrs)
  }



#library(R.utils)
#reassignInPackage("beplot0.matrix","benchmark",my.beplot0.matrix)
#reassignInPackage("beplot0.AlgorithmPerformance","benchmark",my.beplot0.AlgorithmPerformance)
