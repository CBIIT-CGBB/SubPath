rm(list=ls());

library(SubPath);

data("HALLMARK");
data("genes");
data("ppi");

gene1 <- unique(genes[,3])
gene2 <- unique(unlist(HALLMARK$pathway.list));
ppi2  <- ppi[,c(3,6)];

out.s <- NULL;
out.n <- NULL;
out.p <- NULL;
out.l <- NULL;
l.i   <- 0;
for (i in 1:length(HALLMARK$pathway.list)){
  gene  <- unlist(HALLMARK$pathway.list[[i]]);
  pname <- names(HALLMARK$pathway.list)[i];
  out1  <- l2g(gene, ppi2);
  if (class(out1$g) != "igraph"){
    next;
  }
  out2  <- g2subg(out1$g, max_nodes=20, compon=F);
  ## merge list (igraph)
  out.p <- c(out.p, out2);
  ## output list element names
  if (length(out2)==1){
    out.n <- c(out.n, pname);
  } else {
    tmp.n <- paste0(pname, "_", 1:length(out2));
    out.n <- c(out.n, tmp.n);
  }
  ## length of sub-pathway
  for (j in 1:length(out2)){
    myg   <- out2[[j]];
    out.s <- c(out.s, length(V(myg)$name));
  }
}

names(out.p) <- out.n

for (i in 1:length(out.p)){
  myg <- out.p[[i]]
  out.l[[i]] <- V(myg)$name;
}
names(out.l) <- out.n;

mydb1 <-g2db(g_list=out.p, creator="YingHu", version="0.1", mydate=NULL)
mydb2 <-l2db(gene_list=out.l, creator="YingHu", version="0.1", mydate=NULL)


doGSEA(db=mydb1, gene1, filter.num=0, fdr=F);
doGSEA(db=mydb2, gene1, filter.num=0, fdr=F);
