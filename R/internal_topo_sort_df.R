#' Topological Sort of a Directed Graph (Internal)
#'
#' Internal helper to perform a topological sort on a directed acyclic graph
#' (DAG), represented as a data frame with `from` and `to` columns.
#'
#' @param df A data frame with columns `from` and `to`, each representing
#'   directed edges.
#'
#' @return A character vector with the nodes in topologically sorted order.
#'
#' @keywords internal
topo_sort_df <- function(df) {
  
  edges <- df
  nodes <- unique(c(edges$from, edges$to))
  
  # Initialize: for each node, count incoming edges
  in_degrees <- setNames(integer(length(nodes)), nodes)
  for (to in edges$to) {
    in_degrees[to] <- in_degrees[to] + 1
  }
  
  # Nodes with zero in-degree (no dependencies)
  no_incoming <- names(in_degrees[in_degrees == 0])
  
  result <- character(0)
  
  while (length(no_incoming) > 0) {
    current <- no_incoming[1]
    no_incoming <- no_incoming[-1]
    result <- c(result, current)
    
    # Remove edges from `current` to others
    outgoing <- edges[edges$from == current, "to", drop = TRUE]
    for (node in outgoing) {
      in_degrees[node] <- in_degrees[node] - 1
      if (in_degrees[node] == 0) {
        no_incoming <- c(no_incoming, node)
      }
    }
    # Remove those edges
    edges <- edges[edges$from != current, , drop = FALSE]
  }
  
  if (nrow(edges) > 0) {
    stop("Model has at least one cycle; cannot sort topologically.")
  }
  
  return(result)
}