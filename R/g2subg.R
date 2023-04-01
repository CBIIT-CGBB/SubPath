
source("divide_large_graph.R")

g2subg <- function(g, max_nodes=20, compon=FALSE){
  if (compon){
    c.out <- components(g);
    c.u   <- unique(c.out$membership);
    i     <- 0;
    out <- NULL;
    for (j in 1:length(c.u)){
      c.i <- which(c.out$membership==c.u[j]);
      s.g <- induced_subgraph(g, c.i);
      out[[j]] <- s.g
    }
    return(out)
  }
  out <- divide_large_graph(g, max_nodes=max_nodes);
  return(out);
}
