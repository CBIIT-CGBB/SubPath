

g2db <- function(g_list, creator=creator, version=version, mydate=NULL){
  n.name  <- names(g_list);
  out.b   <- NULL;
  out.s   <- NULL;
  for (i in 1:length(g_list)){
    g      <- g_list[[i]];
    gene.s <- paste(V(g)$name, collapse = " ");
    out.b  <- rbind(out.b, c(n.name[i], gene.s, n.name[i]));
    out.s[[i]] <- V(g)$name;
  }
  names(out.s) <- n.name;
  colnames(out.b) <- c("pathway.ID", "genes", "pathway.name");
  gene.u.num <- length(unique(unlist(out.s))); 
  if (is.null(mydate)){
    GSEA.db <- list(pathway.list=out.s, pathway.table=out.b, 
                    pathway.version=list("version:" = version, "Date:" = date(), 
                                         "Gene number:" = gene.u.num, "Creator:" = "Ying Hu"));
  } else {
    GSEA.db <- list(pathway.list=out.s, pathway.table=out.b, 
                    pathway.version=list("version:" = version, "Date:" = mydate, 
                                         "Gene number:" = gene.u.num, "Creator:" = "Ying Hu"));
  }
  return(GSEA.db);
}