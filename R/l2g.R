

l2g <- function(gene, relation, INTXN=TRUE){
  jdat  <- relation;
  i1.i  <- which(jdat[,1] %in% gene);
  i2.i  <- which(jdat[,2] %in% gene);
  if (INTXN){
    i.i   <- intersect(i1.i, i2.i);
  } else {
    i.i   <- unique(c(i1.i, i2.i));
  }
  if (length(i.i) < 2){
    out <- list(g=NULL, out=NULL);
    return(out);
  }
  dat   <- jdat[i.i,];
  g     <- igraph::graph_from_data_frame(dat, directed = F);
  out   <- list(g=g, net=dat);
  return(out);
}