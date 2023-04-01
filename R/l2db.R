

l2db <- function(gene_list, creator=creator, version=version, mydate=NULL){
  n.name  <- names(gene_list);
  out.s   <- gene_list;
  out.b   <- NULL;
  for (i in 1:length(gene_list)){
    gene.s <- paste(gene_list[[i]], collapse = " ");
    out.b  <- rbind(out.b, c(n.name[i], gene.s, n.name[i]))
  }
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