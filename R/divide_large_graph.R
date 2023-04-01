
## 
divide_large_graph <- function(graph, max_nodes=10) {
  # Apply the Louvain method for community detection
  clusters <- cluster_louvain(graph)
  
  # Initialize an empty list to store the small subgraphs
  small_subgraphs <- list()
  
  # Iterate through each subgraph in the clusters
  for (i in 1:length(clusters)) {
    subgraph <- induced_subgraph(graph, clusters[[i]])
    
    # Check if the subgraph has fewer or equal to max_nodes
    if (vcount(subgraph) <= max_nodes) {
      small_subgraphs[[length(small_subgraphs) + 1]] <- subgraph
    } else {
      # If the subgraph is still too large, recursively divide it further
      small_subgraphs <- c(small_subgraphs, divide_large_graph(subgraph, max_nodes))
    }
  }
  
  return(small_subgraphs)
}
