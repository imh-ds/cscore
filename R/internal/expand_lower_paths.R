#' Expand Paths by Decomposing Composite Lower Paths (Internal)
#'
#' Expands a set of directed edges by replacing higher-level composite nodes in
#' the `from` column with their corresponding lower-level components, as defined
#' in a `composite_list`.
#'
#' @param paths A data frame with columns `from` and `to`, representing directed
#'   edges.
#' @param composite_list A list with an element named `higher`, which is a named
#'   list mapping composite node names to character vectors of their constituent
#'   nodes.
#'
#' @return A data frame with the same structure as `paths`, where composite
#'   `from` nodes are replaced by one row for each of their component nodes.
#'
#' @keywords internal
#' @noRd
expand_lower_paths <- function(
    paths, 
    composite_list) {
  
  higher_names <- names(composite_list$higher)
  
  dplyr::bind_rows(
    paths,
    paths |>
      
      # rows that need duplication
      dplyr::filter(from %in% higher_names) |> 
      
      # turn into lists
      dplyr::mutate(from = purrr::map(from, ~ composite_list$higher[[.x]])) |>
      
      # and unnest into separate rows
      tidyr::unnest(from)
  )
  
}