#' Check Whether a Directed Graph is Acyclic (Internal)
#'
#' Determines whether a directed graph, represented as a data frame of edges,
#' forms a directed acyclic graph (DAG). Uses topological traversal to verify
#' acyclicity.
#'
#' @param paths A data frame with columns `from` and `to`, each representing the
#'   edges of a directed graph.
#'
#' @return A logical value: `TRUE` if the graph is acyclic (i.e., a DAG),
#'   `FALSE` otherwise.
#'
#' @keywords internal
#' @noRd
check_dag <- function(paths) {
  # Create adjacency list
  edges <- split(paths$to, paths$from)
  
  # All nodes
  nodes <- unique(c(paths$from, paths$to))
  
  # Initialize in-degree count
  indegree <- setNames(rep(0, length(nodes)), nodes)
  for (tos in edges) {
    indegree[tos] <- indegree[tos] + 1
  }
  
  # Start with nodes of zero in-degree
  queue <- names(indegree[indegree == 0])
  visited <- character()
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    visited <- c(visited, node)
    
    for (neighbor in edges[[node]]) {
      indegree[neighbor] <- indegree[neighbor] - 1
      if (indegree[neighbor] == 0) {
        queue <- c(queue, neighbor)
      }
    }
  }
  
  return(length(visited) == length(nodes))
}